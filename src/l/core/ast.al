;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(Section "Basic AST support")

; Usage:
; (def:ast NAME ( INHERITANCE )
;   (*TOP* TOPNODENAME)
;   (NODENAME PATTERN)...)
;
; where PATTERN is:
;  (| (TAG PTN)...) 
;    or
;  PTN
;
; and PTN is an S-expression with substitutions in form of:
;  <NODENAME:VARIABLE> 
;  <*NODENAME:VARIABLE>
;  VARIABLE
;

;;; AST specification mini-language.

; TODO:
; - name capturing problem (optimization showstopper) (done?)
; - parametric node types (?)
; - ast:find macro
; - unused paths elemination

(function _ast_var? (ptn)
  (and (list? ptn) (eq? '| (car ptn))))

(macro __ast:check_cons_inner (a b oa ob ox)
  (with-syms (na nb)
    `(let ((,na ,a)
           (,nb ,b))
       (if (and (eqv? ,na ,oa)
                (eqv? ,nb ,ob))
           ,ox (cons ,na ,nb)))))

(macro __ast:check_cons_fake (a b oa ob ox)
  `(cons ,a ,b))

(function _ast_one_pattern (buld? pmask srcmask)
  (with-syms (arg)
   `(fun (,arg)
      ,@(if (shashget (getfuncenv) 'debug-compiler-ast)
            `((writeline (list 'DEBUG-AST: ,arg 'IN (quote ,srcmask))))
            nil)
      ,(let loop ((m pmask) (c arg))
          (if (null? m) 'nil
            (if (not m) c
                (if (list? m)
                  (fccase m
                    ((E) (fn) `(,fn ,c))
                    ((_) c) ;; same as #t
                    (else
                     (with-syms (ca cd)
                      `(let* ((,ca (car ,c))
                              (,cd (cdr ,c)))
                         (,(if buld? '__ast:check_cons 'begin) 
                          ,(loop (car m) ca)
                          ,(loop (cdr m) cd)
                          ,@(if buld? `(,ca ,cd ,c) nil)
                          )
                         ))))
                  c
            )))))))

;; <formt>:
;;   (CONST) | (NODE <name> <defn>) | (CONS <formt> <formt>)
(function _ast_prepare_mask (formt nodehash)
   (let loop ((f formt))
      (fccase f
        ((CONST) _ #t)
        ((NODE) (nm)  
         (if (hashget nodehash nm)
             `(E (-process-the-node 
                  (cons nodebody stack) 
                  (quote ,nm))) #t))
        ((NODES) (nm)
         (if (hashget nodehash nm)
             `(E (fun (vl)
                   (map (-process-the-node
                         (cons nodebody stack)
                         (quote ,nm)) 
                        vl)))
             #t))
        ((CONS) (hd tl)
         (let ((res0 (cons (loop hd) (loop tl))))
           (if (let iloop ((r res0))
                    (cond
                      ((null? r) #t)
                      ((and (list? r)
                            (eq? (car r) 'E)) nil)
                      ((list? r) 
                       (and (iloop (car r)) (iloop (cdr r))))
                      (else #t)))
               #t res0)))
        (else (ccerror '(what?)))
        )))

(function _ast_add_deps (ht nodnm mask)
  (let ((all 
          (let loop ((m mask))
            (fccase m
              ((CONST) _ nil)
              ((NODE NODES) (nm) (list nm))
              ((CONS) (hd tl)
               (append (loop hd) (loop tl))))))
        (vv (hashget ht nodnm)))
    (hashput ht nodnm 
             (unifiq (append (if vv vv nil) all)))
))
     
;; Build dependency and reverse dependency graphs for a given AST definition
(function _ast_build_depgraph (defntn)
  (let ((ht (mkhash))
        (rht0 (mkhash))
        (rht (mkhash)))
    (iter-over defntn
      (fmt (nodnm ndef)
         (fccase ndef
           ((VAR) opts 
            (iter-over opts 
               (fmt (tag val) (_ast_add_deps ht nodnm val))))
           ((VAL) (val)
            (_ast_add_deps ht nodnm val)))))
    (hashiter 
     (fun (a b) 
       (foreach (x b) 
           (hashput rht0 x
              (unifiq (cons (string->symbol a)
                            (hashget rht0 x))))))
     ht
    )
    (foreach (a (map car defntn))
      (let loop ((t a) (vis nil) (tovis nil))
        (if (find (fun (x) (eq? t x)) vis)
         (if (null? tovis) nil
          (loop (car tovis) vis (cdr tovis)))
         (let ((rev (hashget rht0 t)))
           (hashput rht a (unifiq (cons t (hashget rht a)) ))
           (if (null? rev) 
               (if (null? tovis) nil
                   (loop (car tovis) 
                         (cons t vis)
                         (cdr tovis)))
               (loop (car rev) 
                     (cons t vis)
                     (append rev tovis)))))))
    (cons rht0 rht)))
        
 

;; Get a list of nodes affected by a given list of modified nodes.
(function _ast_build_corelist (depgraph mnodes)
  (let ((cl (cons 1 nil)) (g (cdr depgraph)))
    (foreach (m mnodes)
      (let ((n (hashget g m)))
        (set-cdr! cl (append (cons m n) (cdr cl)))))
    (unifiq (cdr cl))))

(function _ast_comp_visitornode (buld? vl ch)
  (let* ((mask (_ast_prepare_mask vl ch)))
    `(,(_ast_one_pattern buld? mask vl) nodebody)))

(function _ast_unvar_error (nn expect)
  (ccerror (list 'UNVAR nn 'EXPACT expect)))

(function _ast_comp_visitorptn (buld? dfn ch)
 (fccase dfn
   ((VAR) opts
    `(fun (node)
       (fccase node
         ,@(map-over opts
             (fmt (tag vl)
              `((,tag) nodebody 
                ,(if buld?
                     `(cons (quote ,tag) ,(_ast_comp_visitornode #t vl ch))
                     (_ast_comp_visitornode nil vl ch)
                     ))))
           (else (_ast_unvar_error (car node)
                                   (quote ,(map car opts))
                                   ))
           )))
   ((VAL) (node)
    `(fun (nodebody)
       ,(_ast_comp_visitornode buld? node ch)))))


(function _list->hash (lst)
  (let ((hh (mkhash)))
   (foreach (l lst) (hashput hh l #t))
   hh))

;; Makes a list of transitional visitor entries (no terminals yet!)
(function _ast_make_visitor0 (buld? depgraph defs mnods)
  (let* ((hh (_list->hash mnods))
         (core  (_ast_build_corelist depgraph mnods))
         (ch (_list->hash core))
         (transcore (filter (fun (x) (not (hashget hh x))) core)))
     (cons ch 
      (foreach-map (t transcore)
          (let ((defx (hashget defs t))) 
            `(,t ,(_ast_comp_visitorptn buld? defx ch)))))))

(function _ast_make_accessmacros0 (name fdefz defz)
  (collector (vars gvars)
    (let collect ((d defz) (f fdefz))
      (p:match d
        ($$M 
         (p:match f
           ((NODE $n . $_)
            (vars (list d n)))
           (else nil)))
        (($a . $b) 
         (p:match f
           ((CONS $fa $fb)
            (collect a fa) (collect b fb))
           (else (ccerror `(AST:format ,d ,f)))))
        (else nil)))
    (foreach-map (v (gvars))
      `(,(Sm<< (car v) ".>") (fun (ll) 
             (list 'ast:access:element ,name
                   (quote ,(cadr v)) (quote ,(car v)) (cdr ll)))))))

(function _ast_make_accessmacros (tp defs node)
  (let* ((name (hashget defs "  (DEFNAME)"))
         (defx (hashget defs node))
         (fdef0 (hashget defs (S<< "  " node ":format")))
         )
    (if (null? defx) nil
     (fccase defx
      ((VAR) defs
       (if (not tp)
           nil
           (let* ((en (find (fun (x) (eqv? (car x) tp)) defs))
                  (fdefz (cadr (find
                               (fun (x) (eqv? (car x) tp))
                               (cdr fdef0))))
                  (defz (cadr en)))
             (_ast_make_accessmacros0 name defz fdefz))))
      ((VAL) (defz)
       (if tp
           nil
           (_ast_make_accessmacros0 name defz (cadr fdef0))))
      (else nil)
      ))))

;; compiles the term processing function
(recfunction _ast_term_code (defs entry code buld? ch)
  (let ((entr (hashget defs (buildstring "  " entry ":format")))) ;; named format spec
    (fccase entr
      ((VAR) opts
       (if (eq? 'forall (car code))
        `(fun (node) ,@(cdr code))
        (let* ((d1 (map car code))
               (d2 (map car opts))
               (dif (if (not (or (memq 'else d1) (memq 'else-deep d1)))
                     (foldl (fun (acc v) (filter (fun (av) (not (eqv? av v))) acc)) d2 d1))))
         (if dif 
             (ccwarning `(COVERAGE: ,entry ,dif)))
         
         `(fun (node)
           (fccase node
            ,@(map-over code
                (fmt (cent . cde)
                  (case cent
                    ((else)
                     `(else (begin ,@cde)))
                    ((else-deep)
                     `(else (,(_ast_term_code defs entry (car cde) buld? ch)
                             (,(_ast_comp_visitorptn buld? ;; TODO!
                                                    (hashget defs entry)
                                                    ch)
                              node))))
                    (else
                     (let ((fent (cadr (car (filter (fun (x) (eq? (car x) cent)) opts)))))
                       `((,cent) ,fent 
                         (with-macros
                          ((this-node-var (fun (_) '(quote ,cent)))
                           ,@(_ast_make_accessmacros cent defs entry)
                           )
                          (begin ,@cde))
                         )))))))
           ))))
      ((VAL) (fomt)
       `(fun (node)
          (format node ,fomt ,code))))))

;; Compile all the patterns
(function _ast_make_visitor1 (buld? depgraph defs patns)
  (let* ((mnods (map-over patns (fmt (x) (if (list? x) (cadr x) x))))
         (chhh (_ast_make_visitor0 buld? depgraph defs mnods))
         (ch (car chhh))
         (rest (cdr chhh))
         )
   (append rest
    (map-over patns
      (fmt (entry type code)
        (case type
          ((TERM _ T) `(,entry ,(_ast_term_code defs entry code buld? ch)))
          ((DEREFT) `(,entry
                      ,(_ast_term_code defs (cadr entry) code buld? ch)))
          ((DEREF)
           (let* ((aa (_ast_term_code defs (cadr entry) code buld? ch))
                  (bb (_ast_comp_visitorptn buld?
                                            (hashget defs (cadr entry)) ch)))
             `(,entry
               (fun (nodebody) 
                 ,(if buld?
                    `(,aa (,bb nodebody))
                    `(begin (,bb nodebody) (,aa nodebody)))))))
          ((AFIRST A DEEP) 
           (let* ((aa (_ast_term_code defs entry code buld? ch))
                  (bb (_ast_comp_visitorptn buld?
                                            (hashget defs entry) ch)))
           `(,entry (fun (nodebody)
                      ,(if buld?
                           `(,aa (,bb nodebody))
                           `(begin (,bb nodebody) (,aa nodebody)))))))
          (else (ccerror `(AST: unknown mode ,type)))
          ))))))

;; Make the visitor function.
(function _ast_unnode_error (n whre)
  (ccerror (list 'UNNODE n 'IN whre)))

(function _ast_make_visitor2 (buld? defs toph patns)
   (let* ((depgraph (hashget defs "  (DEPGRAPH)"))
          (code (_ast_make_visitor1 buld? depgraph defs patns))
          (efun 
           `(case ndn 
              ,@(map-over code 
                  (fmt (nnm0 cde)
                   (let ((nnm (if (list? nnm0) (car nnm0) nnm0))
                         (annm (if (list? nnm0) (cadr nnm0) nnm0)))
                    `((,nnm) 
                      (with-macros
                       ((outer-ast-node (fun (_)
                                          (this-ast-node)))
                        (this-ast-node (fun (_)
                                         (quote 
                                          (quote ,annm))))
                        (__ast:check_cons 
                         (fun (args)
                           (cons (quote 
                                  ,(if (shashget (getfuncenv)
                                                 'compiler-ast-cons)
                                       '__ast:check_cons_fake
                                       '__ast:check_cons_inner))
                                 (cdr args))))
                        ,@(_ast_make_accessmacros 
                           nil
                           defs
                           annm)
                        
                        )
                       ,cde)))))
              (else (_ast_unnode_error ndn (list (this-ast-name)))))))
  `(fun (top)
     ((let -process-the-node ((stack nil)
                              (ndn (quote ,toph)))
        ,@(if (shashget (getfuncenv) 'debug-compiler-ast)
            `((writeline (list 'DEBUG-AST-PTN: ndn)))
            nil)
        ,efun) top))))

(macro ast:visit (name toph . patns)
  ("Makes a visitor function for an AST 'name', starting from the node 'toph'."
   "See [AST] documentation section for details."
   )
  `(with-macros
    ((outer-ast-name (fun (_) (this-ast-name)))
     (this-ast-name  (fun (_) (quote (quote ,name)))))
    ,(_ast_make_visitor2 #t 
       (shashget (getfuncenv) 
                (Sm<< "AST:" name ":DEF")) toph patns)))

(macro this-ast-name ()
  '(quote ast-none))

(macro this-ast-node ()
  '(quote ast-node-none))

(macro ast:iter (name toph . patns)
  ("Makes an iterator function for an AST 'name', starting from the node 'toph'."
   "See [AST] documentation section for details."
   )
  `(with-macros
    ((outer-ast-name (fun (_) (this-ast-name)))
     (this-ast-name  (fun (_) (quote (quote ,name)))))
    ,(_ast_make_visitor2 nil (shashget (getfuncenv) (Sm<< "AST:" name ":DEF")) toph patns)))

;;; Compiles the AST definition into the defs hashtable

(define p.ast.ident (<r> (p.alpha | p.digit | (% ".-_/[]"))))

(define _ast_parse_sym 
   (let ((r 
          (<r> ( ("<"  (?? ("*" -> (fun (x) `(*))))
                  ( ( (p.ast.ident +*) :-> list->symbol )
                    ( ( ( (_ ":") ( p.ast.ident +*) ) :-> 
                        list->symbol )
                      | (p.any -> 
                               (fun (x) '(_)))
                      ))
                  ">") ->
                  (M@ (fun (x) (if (eqv? '* (car x)) 
                                   (cons 'NODES (cdr x)) 
                                   (cons 'NODE x))) cdr cuttail))
               | ( ( p.ast.ident +*) ->
                   (M@ (fun (x) (list 'CONST x)) list->symbol))
               | ( (p. *) -> (fun (x) '((CONST _))))
               )))
     (fun (sym)
       (p-result (r (string->list (any->string sym)))))))

(function _ast_process_onedef_struc (defntn)
  (let loop ((d defntn))
    (cond
      ((null? d) '(CONST))
      ((symbol? d) 
       (let ((p (_ast_parse_sym d)))
         (fccase p
           ((CONST) (name) '(CONST))       
           ((NODE)  (ref name) `(NODE ,ref ,name))
           ((NODES)  (ref name) `(NODES ,ref ,name))
           )))
      ((list? d) `(CONS ,(loop (car d)) ,(loop (cdr d)))))))

(function _ast_process_onedef_fmt (defntn)
  (let loop ((d defntn))
     (cond
       ((null? d) nil)
       ((symbol? d)
         (let ((p (_ast_parse_sym d)))
           (fccase p
             ((CONST) (name) name)
             ((NODE NODES) (ref name) name)
             )))
       ((list? d) (cons (loop (car d)) (loop (cdr d)))))))


(function _ast_comp_defs (defns)
  (let* ((def0 
           (map-over defns
            (fmt (nm dv)
              (if (_ast_var? dv)
                  (list nm
                     `(VAR 
                       ,@(map-over (cdr dv)
                           (fmt (enm . efmt)
                             `(,enm ,(_ast_process_onedef_struc efmt)))))
                      `(VAR 
                        ,@(map-over (cdr dv)
                            (fmt (enm . efmt)
                             `(,enm ,(_ast_process_onedef_fmt efmt))))))
                  (list nm
                    `(VAL ,(_ast_process_onedef_struc dv))
                    `(VAL ,(_ast_process_onedef_fmt dv)))))))
          (def1 (map-over
                 def0 (fmt (nm str _) (list nm str))))
          (def2 (map-over 
                 def0 
                 (fmt (nm _ fm) (list (buildstring "  " nm ":format") fm)))))
     (list def1 def2)))

(define _ast_xnparse 
   (let ((p (<r> ((_ "::" ) ((p. *) -> (@ wrap list->symbol)))
                | (_ (p. *)))))
     (fun (s) (p-result (p (string->list (symbol->string s)))))))

(function _ast_uniappend (l1 l2)
  (let ((ht (mkhash)))     
    (iter-over l1 (fmt (a b) (hashput ht a b)))
    (iter-over l2 (fmt (a b) 
                   (let ((xa (_ast_xnparse a)))
                    (if (and xa (list? xa))
                      (let* ( (a1 (car xa))
                              (olt (hashget ht a1)))
                         (if (and olt (list? olt) (eqv? '| (car olt)))
                            (let ((nw (append olt (cdr b))))
                               (hashput ht a1 nw))
                            (hashput ht a1 b)))
                      (hashput ht a b)
                      ))))
    (hashmap (fun (a b) (list (string->symbol a) b)) ht)))

(function _ast_renames (substs l)
  (if (null? substs) l
    (foreach-map (i l)
     (let ((hd (car i)))
      (let ((ff (find (fun (x) (eq? (car x) hd)) substs)))
        (if ff (cons (cadr ff) (cdr i)) i))))))

(function makeitervisit (iv name topnode arg code)
  `(<> ,arg (,iv ,name ,topnode ,@code)))
                     
(macro def:ast (name incls . defns)
  ("Defines a named AST, inheriting properties from 'incls' and adding new 'defns'."
   "The definition is interpreted and exists in compilation time only."
   )

  (let* ((incls1 (foreach-map (i incls)
                    (cond
                      ((symbol? i) (list i))
                      ((list? i)   i))))
         (defns1 (_ast_uniappend 
                  (foldl (fun (acc dn) 
                           (_ast_uniappend acc 
                             (_ast_renames (cdr dn)
                               (hashget (shashget (getfuncenv) 
                                                  (Sm<< "AST:" (car dn) ":DEF"))
                                       "  (DEFSRC)"))))
                         nil incls1)
                  defns)))
   (format (_ast_comp_defs defns1) (d1 d2)                
    (read-int-eval 
     `(macro ,(Sm<< name ":visit") (topnode arg . rest)
            (makeitervisit 'ast:visit (quote ,name) topnode arg rest)))
    (read-int-eval
     `(macro ,(Sm<< name ":iter") (topnode arg . rest)
             (makeitervisit 'ast:iter (quote ,name) topnode arg rest)))

    `(ctimex (define ,(string->symbol (buildstring "AST:" name ":DEF"))
                (let ((ht (mkhash))
                     (d1 (quote ,d1))
                     (d2 (quote ,d2)))
                  (foreach (d d1) (hashput ht (car d) (cadr d))) 
                  (foreach (d d2) (hashput ht (car d) (cadr d))) 
                  (hashput ht "  (DEPGRAPH)" (_ast_build_depgraph d1))
                  (hashput ht "  (DEFSRC)" (quote ,defns1))
                  (hashput ht "  (DEFNAME)" (symbol->string (quote ,name)))
                  ht
                  )))
    )))

(function __drawgraph (ht fo)
  (call-with-output-file fo
    (fun (ff)
      (iter (cut fprintln ff <>)
            '("digraph ast {"
              "node [shape=record];"
              ))
      (hashiter (fun (nd ndx)
                  (foreach (n ndx)
                    (fprintln ff (S<< "\"" n "\" -> \"" nd "\";"))))
                ht)
      (fprintln ff "}"))))

(macro draw:ast:graph (name fo)
  `(ctimex
    (__drawgraph (car
                  (hashget ,(string->symbol (S<< "AST:" name ":DEF"))
                           "  (DEPGRAPH)"))
                 ,fo)))


(macro ast:revisit (name xargs src top options ooptions)
 (with-syms (arg iarg xderef)
  `(fun (,arg)
     (let ,name ((,iarg ,arg) ,@xargs)
       (<> ,iarg
         (ast:visit ,src ,top
           (,top TERM
             (,@options
              (else
               (<> node
                 (ast:visit ,src ,xderef
                   ((,xderef ,top) DEREF (forall node))
                   ,@ooptions
                   (,top TERM
                     (forall
                      (,name node ,@(map car xargs)))))))))))))))


(function build-c-body (ndef)
  (let loop ((n ndef))
    (p:match n
      (() 'nil)
      (($a . $b) `(cons ,(loop a) ,(loop b)))
      ($$M:s
       (let* ((p (_ast_parse_sym s)))
         (fccase p
           ((CONST) (nme) p)
           ((NODE NODES) (ref nme) (if (eqv? nme '_) ref nme)))))
      (else '(quote ,n)))))

(function build-constructor (ndef varname values)
  (let ((nxdef (if varname
                   (cdr (find (fmt (nm . _) (eqv? nm varname)) (cdr ndef)))
                   ndef)))
    `(let ,values
       ,(if varname
           `(cons (quote ,varname)
                  ,(build-c-body nxdef))
           (build-c-body nxdef)))))

(macro ast:mknode:inner (astnameq nodenameq varnameq qvalues)
  (let* ((astname (cadr astnameq))
         (nodename (cadr nodenameq))
         (varname (p:match varnameq ((this-node-var) nil)
                           ((quote $x) x)))
         (astdef (shashget (getfuncenv) 
                          (Sm<< "AST:" astname ":DEF")))
         (src (hashget astdef "  (DEFSRC)"))
         (values (cadr qvalues))
         (nodedef (cadr (find (fmt (a . _) (eqv? nodename a)) src))))
    (build-constructor nodedef varname values)))
  
(macro ast:mknode values
  (
   "Make a node of a current format. To be used within a visitor or revisitor only."
   )
  `(inner-expand-first
    ast:mknode:inner (this-ast-name) (this-ast-node)
    (this-node-var)
    (quote ,values)))

;;; TODO: path
(macro ast:access:element (ast node varname xrest)
  (let* ((defn (shashget (getfuncenv) (Sm<< "AST:" ast ":DEF")))
         (ndedef (hashget defn (S<< "  " node ":format"))))
    (p:match ndedef
      ((VAR . $xxx)
       (ccerror `(AST: variant accessor ,node ,(car xrest))))
      ;; TODO: optimise, do not use a generic format.
      ((VAL $fomt)
       `(format ,varname ,fomt
                (with-macros (,@(_ast_make_accessmacros nil defn node))
                             ,(car xrest))))
      (else (ccerror `(A:A:E ,ndedef))))))

;;; Documentation builder for ASTs

(function docstring-frmt (frmt)
  (doc.texnic (to-string frmt))
  )

(function docstring-single (nm frmt)
  (S<< "\n{\\bf " (doc.texnic (any->string nm)) "} $\\to${} "  
       (docstring-frmt frmt)
       "{}~\\\\"))

(function docstring-varying (nm rest)
  (S<< "\n{\\bf " (doc.texnic (any->string nm)) "} $\\to${} "
       "\n"
       (strinterleave
        (foreach-map (r rest)
          (S<< "\\hskip 1cm {}~ " (docstring-frmt r)))
        "\n")
       "~\\\\"))

(function ast-docify (name opts src)
  (foldl string-append 
    (S<< "AST " name " of " (to-string opts) "\\\\")
    (foreach-map (s src)
      (p:match s
        (($nm ([|] . $rest))
         (docstring-varying nm rest))
        (($nm $frmt)
         (docstring-single nm frmt))
        (else "")
        ))))

(macro def:ast:doc (name opts . src)
  (let* ((doctext (ast-docify name opts src)))
    `(top-begin
       (Par ,doctext)
       (def:ast ,name ,opts ,@src))))

