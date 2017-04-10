;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{Pre--compilation transforms}
;-
(function cc:recentry-fix ( expr rentry )
  (cc:mbcoreast:visit expr expr
    (expr _
      ((Fun
        (if recname node
            (ast:mknode (recname rentry))))
       (else node)))))

(define cc:process-local-variable-metadata
  (mkref (fun (tp id md srcmd) nil)))

(define cc:process-variable-metadata
  (mkref (fun (tp id md) nil)))

(define cc:process-remark-metadata
  (mkref (fun (tp md) nil)))

(function cc:scope-transform ( ast )
  (<> ast
    (ast:revisit scopeloop ((substs nil) (rrec nil) (rentry nil))
                   cc:mbcoreast expr
;= [[SLet]] introduces new variables, visible inside the body scope only.
      ((SLet
        (let* ((newnames (map (fun (_) (gensym)) defs))
               (ntsubs (mapn
                        (fun (odef on)
                          (format odef (nn ov . md)
                                  `(,nn ,on loc ,@md)))
                        defs
                        newnames))
               (nsubs
                (append ntsubs
                        substs))
               (newdefs
                (map-over (czip newnames defs)
                   (fmt (nnm onm dfn . md)
                        `(,nnm ,(scopeloop (cc:recentry-fix dfn (Sm<< onm "_" nnm))
                                           substs rrec onm) ,@md)))))
          `(SLet ,newdefs ,(scopeloop body nsubs rrec nil))))

       (MDAnnot
        (let* ((tmpht (mkhash)))
          (foreach (m defs)
            (format m (v . md)
              (ohashput tmpht v md)))
          (let* ((newsubsts
                  (foreach-map (s substs)
                    (format s (nn on tp . omd)
                      `(,nn ,on ,tp ,@omd ,@(ohashget tmpht nn))))))
            (scopeloop body newsubsts rrec nil))))
       
;= [[SLetRec]] introduces new variables, visible within the scope of all value definitions
;= as well as in the body scope.
       (SLetRec ; TODO:::: Remove redundant closures here!!!!
        (let* ((newnames (map (fun (_) (gensym)) defs))
               (nsubs1
                (append (mapn
                         (fun (odef on)
                           (format odef (nn ov . md)
                                   `(,nn ,on loc ,@md)))
                         defs
                         newnames)
                        substs))
               (nrrec
                (append (map car defs) rrec))
               (nsubs2
                (append (mapn
                          (fun (nn on)
                               `(,nn ,on loc))
                          (map car defs) newnames
                          )
                        substs))
               (newdefs
                (map-over (czip newnames defs)
                   (fmt (nnm onm dfn . md)
                        `(,nnm ,(scopeloop (cc:recentry-fix dfn onm)
                                           nsubs1 nrrec onm) ,@md)))))
          `(SLetRec ,newdefs ,(scopeloop body nsubs2 rrec nil))))
;= Lambda abstraction introduces new variables (arguments), visible inside
;= function body only.
       (Fun
        (let* ((newnames
                (map (fun (_) (gensym)) args))
               (rrecname recname)
               (newrecname
                (if rrecname
                    (list
                     rrecname
                     (if (eqv? rrecname rentry)
                         (alet fnd (find (fmt (onm) (eqv? onm rrecname))
                                         substs)
                               (p:match fnd
                                 (($n1 $nn loc . $md) nn) ; do not catch recs here
                                 (else (gensym)))
                               )
                         (Sm<< recname '_ (gensym)))
                     'rec)
                    nil))
               (nsubsa (map-over (czip args newnames)
                           (fmt ((_ nn . md) . on)
                             `(,nn ,on arg ,@md))))
               (nsubsb (map-over substs
                           (fun (x)
                             (format x (nn on tp . md)
                               (case tp
                                 ((rec)
                                  (if (memq nn rrec) x
                                      `(,nn ,on loc ,@md)))
                                 (else x))))))
               (nsubs0
                (append nsubsa nsubsb))
               (nsubs (if newrecname (append nsubsa (cons newrecname nsubsb))
                          nsubs0))
               (nrn (if newrecname (cadr newrecname) nil))
               )
          `(Fun ,nrn ,(foreach-map (n newnames) `(var ,n)) ;; losing metadata here
                ,(scopeloop body nsubs nil nil))))
;= [[Var]] is referencing to a local variable ([[SLet]]--defined), an
;= argument or a global variable, which is (by definition) any variable not in the local
;= substitutions list. No environment is available at this stage, and we can't
;= actually check if the global name is defined.
       (Var
        (let* ((fnd (find (fmt (onm) (eqv? onm id))
                          substs)))
          (if fnd
           (format fnd (_ nnm tp . srcmd)
             (let* ((vtp (case tp ((loc) 'Var)
                               ((arg) 'Arg)
                               ((rec) 'Recref)
                               )))
               (if (and srcmd md) ((deref cc:process-local-variable-metadata) vtp id md srcmd))
               `(,vtp ,nnm)))
           (begin
             (if md ((deref cc:process-variable-metadata) 'glob id md))
             `(Glob ,(core:lookup-global  id))))))
       (FixLocal
        (let* ((fnd (find (fmt (onm) (eqv? onm id))
                          substs)))
          (if fnd
           (format fnd (_ nnm tp . srcmd)
              `(FixLocal ,nnm ,oldid))
           `(FixLocal ,id ,id))))
;= [[BackendAsm]] also can contain some variable references.
       (BackendAsm
        (let ((ns1
               (foreach-map (id (map car use))
                 (let* ((fnd (find (fmt (onm) (eqv? onm id))
                                   substs)))
                   (list id
                         (if fnd
                             (format fnd (_ nnm tp . srcmd)
                                     `(,(case tp ((loc) 'Var)
                                              ((arg) 'Arg)
                                              ((rec) 'Var)
                                              ) ,nnm))
                             `(Glob ,id)))))))
          (ast:mknode (use ns1))))
;= Same conditions are checked for [[Set]], which can modify a local variable
;= or a global one. Trying to modify an argument is an error.
       (Set
        (let* ((fnd (find (fmt (onm) (eqv? onm nm)) substs))
               (ref
                (if fnd
                    (format fnd (_ nnm tp . srcmd)
                      `(,(case tp ((loc) 'Var)
                          ((arg)
                           (cc:comperror `(CC00-ARGM ,nnm))
                           )) ,nnm))
                    `(Glob ,nm))))
          (format ref (tp nm)
            `(XSet ,tp ,nm ,(scopeloop vl substs rrec nil)))))
       )
      ()
      )))

(function cc:count-refs (ast)
  (with-hash (cc)
    (cc:mbcoreast:iter expr ast
      (expr DEEP
        ((Var (cc! id 'Var))
         (Arg (cc! id 'Arg))
;         (Recref (cc! id 'Var))
         (XSet (if (not (eqv? tp 'Glob))
                   (cc! nm tp)))
         (else nil))))
    cc))

(function cc:intersection (lst ht)
  (filter (cut hashget ht <>) lst))

(function cc:sticky (pfx name)
  (if pfx
      (Sm<< pfx "+" name)
      name))

(include "./cc-letsrec.al")

(function cc:lift-lambdas ( ast )
  (alet srec? (mkref)
  (collector (add get)
;= This function returns the result as a list of transformed expression and all the
;= lifted definitions.
   (do `((Init retexpression ,rv) ,@(get))
    (where
     (rv
      (let liftloop ((expr ast) (bound nil) (rrec nil) (stickyname nil))
        (cc:mbcoreast:visit expr expr
         (expr _
;= [[SLet]] is the only mean of local variables definition, adding new bound names
;= to the current list of bound variables.
         ((SLet
           (let* ((nbound (append (map car defs) bound))
                  (ndefs
                   (map-over defs
                    (fmt (nm vl)
                     `(,nm ,(liftloop vl bound nil stickyname))))))
             (ast:mknode
              (defs ndefs)
              (body (liftloop body nbound nil stickyname)))))
;= [[GSetFn]] also may introduce recursive names:
          (GSetFn
           (alet newvl (liftloop vl bound (if (eqv? recp 'rec)
                                              nm nil) nm)
             (add
              `(Init funref
                     ,(ast:mknode (vl newvl))))
             (add
              `(Funref ,nm ,(p:match newvl
                              ((Funref $nmx) nmx)
                              (else (cc:comperror `(CC03:WRONG ,node))))))
             '(Dummy)))
;= [[GSet]] lifts a global entity.
          (GSet
           (begin
             (add `(Global ,nm ,nm
                           (GSet ,nm
                                 ,(liftloop vl bound nil
                                            (Sm<< "glob+" nm)
                                            ))))
             '(Dummy)))
;= [[GSetM]] defines a macro --- must be in ctime scope expression
          (GSetM
           (begin
             (add `(Init ctime
                         (GSetM ,nm
                                ,(liftloop vl bound
                                           nil (Sm<< "macro+" nm)))))
             '(Dummy)
             ))
;= SLetRec is a little bit to complex to inline the whole code here
          (SLetRec
           (begin
             (set-car! srec? #t)
             (cc:doletsrec liftloop bound rrec stickyname get defs body)))
;= All [Fun] instances must be lifted, either as a direct reference to a new
;= globally defined function or as a closure generator application.
          (Fun
           (let* ((vars
                   (cc:count-refs body))
                  (env
                   (cc:intersection bound vars))
                  (nbody (liftloop body
                                   (append
                                    (if recname (list recname) nil)
                                    (append (bootlib:filter-args args) bound))
                                   recname
                                   stickyname
                                   ))
                  (newname
                   (if recname recname (gensym)))
                  )
             (if (null? env) ; Simple function
                 (begin
                   (add
                    `(Simple ,newname ,(cc:sticky stickyname newname)
                             (Fun ,newname ,args ,nbody)))
                   `(Funref ,newname))
                 (begin      ; Otherwise it is a closure
                   (add
                    `(Closure ,newname ,(cc:sticky stickyname newname)
                              ,env (Fun ,(Sm<< newname "_closure") ,args ,nbody)))
                   `(App (Glob ,newname)
                      ,@(foreach-map (e env)
                          (alet tp (hashget vars e)
                                `(,tp ,e)))))
                 )))
          (else-deep ((else node)))

          ))))))))))


(function cc:clean-dummy ( lifted )
  (cc:mbcoreast:visit lifted lifted
    (expr DEEP
       ((Begin
         (if (null? es) '(Nil)
             (alet iner
               (foreach-mappend (e es)
                                (p:match e
                                  ((Dummy) nil)
                                  (else (list e))))
               (if iner `(Begin ,@iner) '(Nil)))))
        (else node)))))

(function cc:fix-closures ( lifted )
  (cc:mbcoreast:visit lifted lifted
    (liftop _
      ((Closure
        (alet refname name
         (with-hash (h)
           (foreach (i args) (h! i #t))
           (ast:mknode
            (e
             (cc:convert-tail-helper refname
               (cc:mbcoreast:visit expr e
                 (expr DEEP
                   (
                    (Var (if (h> id) `(Clenv ,id)
                             (if (eqv? id refname) `(Recref ,id) node)))
                    (Arg (if (h> id) `(Clenv ,id)
                             node))
                    (XSet (if (h> nm)
                              (cc:comperror `(CC01:CAPT ,nm)))
                          node)
                    (else node))))))))))
       (Simple
        (alet refname name
          (ast:mknode
           (e
            (cc:convert-tail-helper
             name
             (cc:mbcoreast:visit expr e
               (expr DEEP
                (
                 (Var (if (eqv? id refname) `(Recref ,id) node))
                 (else node)
                 )
               )))))))
       (else node)
       ))))

