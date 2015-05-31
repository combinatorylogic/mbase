;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; PEG compilation pipeline:
;;
;; - Environment handling (loading the inherited entries)
;;    packrat-ast-f0, calls the rest of the pipeline
;;
;; - packrat-expand-macros: lift all the macro application terms
;; - packrat-preprocess-top
;;    - packrat-preprocess-binaries: detect Pratt declarations
;;    - packrat-preprocess-top-tokens:  lift tokens, applying a token rule
;;    - packrat-preprocess-top-ignore:  apply the ignorance rules to the tokens and lexicals
;;    - packrat-preprocess-autonames:  expand the "..." constructors
;;    - packrat-preprocess-top-0:  lift the embedded terms
;;    - packrat-detect-recursion:  find left-recursive clusters of terms
;;    - packrat-analyse-ast: try to infer AST, match with declaration if needed
;; - From 'translate-all': for each top-level entry:
;;   - translate-binds
;;   - translate-binds-2
;;   - 'translate-term' or 'translate-binaries'
;;   - 'translate-term':
;;      - translate-expr: each elementary Peg node is translated into the backend AST
;;      - packrat-optimise-intermediate: trivial peephole optimisations
;;
;;  See backend-ast.al for the backend language specification.
;; 

(function extract-auto (expr tag)
  (alet res
   (packrat:visit expr expr
    (expr DEEP
          ((seq (foreach-mappend (e es) e))
           (palt (foreach-mappend (e es) e))
           (withignore e)
           (ordie e)

           (pdalt (foreach-mappend (e es) e))
           
           (plus e)
           (star e)
           (maybe e)
           (withfilter e)
          
           (bind `((set ,name (var ,name))))
           (bind-terminal `((set ,fname (var ,fname))))
          
           (else nil))))
   `(constr ,tag ,@res)))

(function fix-auto-dcode (tname expr dcode)
  (if (cadr dcode)
     (packrat:visit dualcode dcode
       (code _
             ((auto (if tagname 
                        (extract-auto expr (car tagname))
                        (extract-auto expr tname)))
              (else node))))
     `(,(car dcode) (nop))))

(function translate-binds (expr)
  (packrat:visit expr expr
    (expr DEEP
          ((bind
            (p:match e
              ((terminal $id . $rec)
               `(bind-terminal ,name ,id ,@rec))
              ((highorder . $_) node)
              (else (ccerror `(TRYING-TO-BIND-NONTERMINAL ,node)))))
           (else node)))))

(function translate-binds-2 (code)
  (<> code
      (ast:revisit loop ((topdown #t) (topmaybe 1))  packrat expr
          ((palt
            (alet inner
              (foreach-map (e es)
                (loop e nil (if (eq? topmaybe 1) 2
                                (if (eq? topmaybe 2) 2
                                    0))))
              `(palt
                ,@inner)))
           (pdalt 
            (alet inner
              (foreach-map (e es)
                (loop e nil (if (eq? topmaybe 1) 2
                                (if (eq? topmaybe 2) 2
                                    0))))
              `(pdalt
                ,@inner)))
           (merge 
            (alet inner
              (foreach-map (e es)
                (loop e nil (if (eq? topmaybe 1) 2
                                (if (eq? topmaybe 2) 2
                                    0))))
              `(merge
                ,@inner)))
           (notp node)
           (withignore (ast:mknode (e (loop e topdown topmaybe))))
           (ordie (ast:mknode (e (loop e topdown topmaybe))))
           (star  `(star ,(loop e topdown 0)))
           (plus  `(plus ,(loop e topdown 0)))
           (maybe  `(maybe ,(loop e topdown 0)))
           (withfilter (ast:mknode (e (loop e topdown topmaybe))))
           (bind node)
           (bind-terminal node)
           (seq node)
           (trivial node)
           (simple node) ;; should not be here
           (terminal
            (cond
             ((eq? topmaybe 2)
              `(bind-terminal *val* ,name ,@rec))
             (topdown
              `(bind-terminal *val* ,name ,@rec))
             (else node))))
          ())))

;; Get rid of the excessive peg-nodes

(function packrat-optimise-intermediate (code)
  (let loop ((c code))
    (p:match c
      ((peg-node (peg-if (peg-node $a) (peg-node $b) (peg-node $c)))
       (loop `(peg-node (peg-if ,(loop a) ,(loop b) ,(loop c)))))
      ((peg-node (peg-if (peg-node $a) (peg-node $b) (peg-fail)))
       (loop `(peg-node (peg-if ,(loop a) ,(loop b) (peg-fail)))))
                        ((peg-node (peg-check (peg-node $a) (peg-node $b)))
                         (loop `(peg-check ,(loop `(peg-node ,a))
                                           ,(loop `(peg-node ,b)))))
      ((peg-loop $x) `(peg-loop ,(loop x)))
      ((peg-ignore $a $b) `(peg-ignore ,a ,(loop b)))
      ((peg-bind $a $b $c) `(peg-bind ,a ,b ,(loop c)))
      ((peg-node (peg-bind $a $b (peg-node $c))) `(peg-node (peg-bind ,a ,b ,(loop c))))
      ((peg-node $x) `(peg-node ,(loop x)))
                        ((peg-check (peg-node (peg-trivial $a))
                                    (peg-node (peg-trivial $b)))
                         (loop `(peg-node (peg-trivial (or ,a ,b)))))
      ((peg-check $x $y) `(peg-check ,(loop x) ,(loop y)))
      ((peg-check $x) (loop x))
      (else c))))

(function translate-expr (expr mults bin)
  (packrat:visit expr expr
             (expr DEEP
                ((seq
                  `(peg-node
                    ,(let loop ((e es))
                       (p:match e
                         (($a) a)
                         (($a . $rest)
                          `(peg-if ,a
                                   ,(loop rest)
                                   (peg-fail)))
                         (() `(peg-dummy))))))
                 (palt 
                  `(peg-node
                    ,(let loop ((e es))
                       (p:match e
                         (($a) a)
                         (($a . $rest)
                          `(peg-check ,a
                                      ,(loop rest)))
                         (() `(peg-dummy))))))
                 (pdalt 
                  `(peg-node
                    ,(let loop ((e es))
                       (p:match e
                         (($a) a)
                         (($a . $rest)
                          `(peg-check-collect
                            ,a
                            ,(loop rest)))
                         (() `(peg-dummy))))))
                 (merge
                  `(peg-node
                    (peg-merge-altbranches ,es)))
                 (star
                  `(peg-node
                    (peg-loop ,e)))
                 (plus
                  `(peg-node
                    (peg-if ,e 
                            (peg-loop ,e)
                            (peg-fail))))
                 (maybe
                  `(peg-node
                    (peg-check ,e
                               (peg-dummy))))
                 (withfilter
                  `(peg-filter-stream ,f ,e))
                 (notp
                  `(peg-node
                    (peg-if ,e
                            (peg-fail)
                            (peg-dummy))))
                 (andp
                  `(peg-node
                    (peg-if ,e
                            (peg-dummy-back)
                            (peg-fail))))
                 (withignore
                  `(peg-ignore ,terms ,e))

                 (ordie
                  `(peg-ordie ,e ,m ,@args))

                 (bind-terminal
                  `(peg-bind ,(hashget mults fname)
                             ,fname (peg-call-terminal-gen ,tname ,rec)))
                 (bind
                  `(peg-bind ,(hashget mults name)
                             ,name
                             ,e))
                 (terminal
                  (if bin
                      `(peg-call-terminal-gen ,name ,rec)
                      `(peg-call-gen ,name ,rec)))
                 (trivial
                  `(peg-node
                    (peg-trivial ,p)))
                 (simple
                  `(peg-call-simple ,name))
                 (check
                  `(peg-call-checker ,name))

                 (action
                  `(peg-call-action ,name ,args))
                 
                 (hint
                  `(peg-backend-hint ,name ,args))

                 (highorder
                  `(peg-call-highorder ,args ,maker))
                 (else
                  (ccerror `(SHOULD-NOT-BE-HERE ,node)))
                 )
                ))
  )

(function translate-term (name ttype dcode expr report rngs dtype)
  (let* ((tstruct (peg-terminal-structure expr))
         (mults (with-hash (m)
                  (foreach (a tstruct)
                    (format a (nm mlt . _)
                            (m! nm mlt)))
                  m))
         (res (translate-expr expr mults nil)))
    `(peg-term-function
                        ,(alet chkx (hashget rngs name)
                            (if chkx
                                (if (eqv? chkx 'undef)
                                    nil
                                    chkx)
                                nil))
                        ,name ,ttype 
                        ,(cons tstruct (fix-auto-dcode name expr dcode)) 
                        ,(packrat-optimise-intermediate res) ,report
                        ,dtype
                        )
    ))

;; See http://publications.csail.mit.edu/lcs/pubs/pdf/MIT-LCS-TR-147.pdf
;;   or http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
;;      https://github.com/munificent/bantam
(function translate-binaries (name ttype lr vs rngs dtype)
  (alet chkx (hashget rngs name)
        `(peg-term-function-pratt
          ,(if chkx
               (if (eqv? chkx 'undef) nil chkx)
               nil)
          ,name ,ttype
          ,(packrat-optimise-intermediate
            (translate-expr lr (mkhash) 1)) ; mults are not allowed
          ,(foreach-map (v vs)
             (packrat:visit binvar v
                            (binvar _ `(binary ,prec ,assoc
                                               ,(packrat-optimise-intermediate
                                                 (translate-expr op (mkhash) 1))
                                               ,(cons '((L nil)
                                                        (op nil)
                                                        (R nil))
                                                      (fix-auto-dcode name
                                                                      op constr))))))
          ,dtype
          )))

(function packrat-find-target-ast (code)
 (collector (add get)
  (packrat:iter ntexprs code
    (ntexpr _
      ((targetast (add name))
       (else nil))))
  (let* ((r (get)))
    (if r (car r) nil))))

(function translate-all (entry borrow code localmacros dhooks exp?)
  (let* ((rngs (packrat-reckon-ranges (packrat-borrow-ranges borrow) code))
         (targetast (packrat-find-target-ast code))
         )
  `(peg-parser ,exp? ,entry ,borrow
               ,(foreach-mappend (c code)
                  (p:match c
                    ((terminal $dtype $ttype $name $v $dc . $rep)
                     `((,name ,(translate-term 
                                name ttype dc 
                                (translate-binds-2 (translate-binds v))
                                (if rep (car rep) nil)
                                rngs dtype
                                ))))
                    ((binaries $dtype $ttype $name $lr $vs . $rep) ;; todo: respect rep
                     `((,name ,(translate-binaries name ttype lr vs rngs dtype))))
                    (else nil)))
               ,localmacros
               ,dhooks
               ,targetast
               )))

;;; (debugmacro 'peg-parser)

;;;;;;;--------------------------

(function packrat-get-leftcalls (expr)
  (collector (add get)
   (<> expr
    (ast:revisit loop ((left? #t)) packrat expr
       ((palt
         (foreach (e es)
           (loop e left?)))
        (withignore
           (loop e left?))
        (ordie
           (loop e left?))
        (pdalt
         (foreach (e es)
           (loop e left?)))
        (merge
         (foreach (e es) (loop e left?)))
        (seq
         (p:match es 
           (($fst . $nxt)
            (begin
              (loop fst left?)
           
              (foreach (n nxt)
                (loop n nil))))))
        (terminal
         (if left?
             (add name)))
        (bind-terminal
         (if left?
             (add tname)))
        ) ()))
   (unifiq (get))
   ))

(function packrat-collect-recs (graph)
  (with-hash (res)
  (alet tmp (graph2graph graph)
  (use-hash (tmp)
    (foreach (bl (map car graph))
     (when (not (res> bl))
      (foreach (ku (trajan tmp bl))
        (p:match ku
          (($one) (if (memq one (tmp> one)) (res! one one)))
          (else (foreach (k ku) (res! k k)))))))))
  (return res)
  ))
 
(function packrat-mark-leftrec (ttype uh norms expr)
  (use-hash (uh norms)
  (<> expr
    (ast:revisit loop ((left? #t)) packrat expr
       ((palt
         `(palt ,@(foreach-map (e es)
                    (loop e left?))))
        (withignore (ast:mknode (e (loop e left?))))
        (ordie (ast:mknode (e (loop e left?))))
        (pdalt
         `(pdalt ,@(foreach-map (e es)
                    (loop e left?))))
        (merge
         `(merge ,@(foreach-map (e es) (loop e left?))))
        (seq
         (p:match es 
           (($fst . $nxt)
            `(seq ,(loop fst left?) ,@(foreach-map (n nxt)
                                       (loop n nil))))))
        (terminal
         (if (and left? (uh> name))
             (begin
               (if (or (not (eqv? ttype 'term))
                       (norms> name))
                   (ccerror `(PEG:RECURSIVE-NON-TERMINAL: ,name IN ,expr)))
               `(terminal ,name #t))
             (if (norms> name)
                 `(simple ,name)
                 node)))
        (bind-terminal
         (if (and left? (uh> tname))
             `(bind-terminal ,fname ,tname #t)
             node))
        ) ()))))

(function packrat-detect-recursion (code)
  (with-hash (norms)
  (alet uh
   (collector (add get)
      (packrat:iter ntexprs code
       (ntexpr DEEP
               ((terminal
                 (begin
                   (if (eqv? ttype 'normal) (norms! name #t))
                   (add (cons name (packrat-get-leftcalls value)))))
                (binaries
                 (add (cons name (packrat-get-leftcalls lr))))
                (else nil))))
      (packrat-collect-recs (get)))
   (packrat:visit ntexprs code
    (ntexpr DEEP
      ((terminal
        (ast:mknode (value (packrat-mark-leftrec ttype uh norms value))))
       (binaries
        (ast:mknode (lr (packrat-mark-leftrec ttype uh norms lr))))
       (else node)))))))

(function packrat-collect-terminal-ctors (termname add dtype value constr)
  (let* ((rt (mkhash))
         (associate
          (fun (es)
            (let* ((vs
              (foreach-mappend (e es)
                (packrat:visit expr e
                   (expr DEEP 
                     ((simple (wrap name))
                      (terminal (wrap name))
                      (bind-terminal (wrap tname))
                      (else nil)))))))
              (foreach (v vs)
                (add `(associate ,termname ,v)))))))
    ;; collect named bindings
    (packrat:visit expr value
       (expr DEEP
          ((bind-terminal (ohashput rt fname tname) tname)
           (bind (if e (ohashput rt name e)) e)
           (terminal name)
           (else nil))))
    ;; Collect alternatives
    (let* ((stripvalue
            (packrat:visit expr value
             (expr DEEP
               ((withignore e)
                (else node))))))
      (packrat:visit expr stripvalue
         (expr _
            ((palt (associate es))                  
             (else nil)))))
    ;; Collect ctor tags
    (if (cadr constr)
     (packrat:iter code (cadr constr)
       (code _      
         ((constr (add `(tag ,termname ,cname)))
          (var    (add `(associate ,termname ,(ohashget rt name))))
          (dconstr (add `(nodetype ,termname ,nname)))
          (else nil)))))
    ;; No need to infer anything
    (p:match dtype
      (((target $nm)) ;; TODO!
       (add `(nodetype ,termname ,nm))))
    ))

(def:ast asteqns ()
  (eqns <*eqn:es>)
  (eqn
   (| (tag <termid:id> <tagid:tg>)
      (associate <termid:a> <termid:b>)
      (nodetype <termid:id> <nodeid:nd>)
      )))

;; AST inference steps:
;; - flatten associations
;; - bind all the tags to associations
(function packrat-asteqns-clusters (eqns)
  (let* ((ht (mkhash))
         (cht (mkhash))
         (tht (mkhash))
         )
    ;; fill the cluster table
    (asteqns:iter eqns eqns
        (eqn _ ((associate (ohashput ht a (unifiq (cons b (ohashget ht a))))
                           (ohashput ht b (unifiq (cons a (ohashget ht b)))))
                (tag (ohashput ht id (unifiq (cons id (ohashget ht id)))))
                (else nil))))
    ;; pick up random cluster entries and associate all the other
    ;; linked nodes with them
    (let* ((fillin
            (fun (id)
              (if (ohashget cht id) nil
                  (begin
                    (ohashput cht id id)
                    (let loop ((vs (ohashget ht id)))
                      (foreach (v vs)
                        (if (not (ohashget cht v))
                            (begin
                              (ohashput cht v id)
                              (loop (ohashget ht v)))))))))))
      (asteqns:iter eqns eqns
        (eqn _ ((associate (fillin a))
                (tag (fillin id))
                (else nil)))))
    ;; associate tags with clusters
    (asteqns:iter eqns eqns
       (eqn _ ((tag (let* ((cid (ohashget cht id)))
                      (ohashput tht cid (unifiq (cons tg (ohashget tht cid))))))
               (else nil))))
    ;; Return the resulting association
    (cons cht tht)
    ))

(function packrat-ast-match (astc tgsh src eqns)
  (format tgsh (cht . tht)
  (let* (
         (implct (mkhash))
         (revcht (mkhash))
         (report (fun (k v) (writeline `(MAYBE-ERROR ,k ,v)))) ;; TODO!
         (set-intersect (fun (a b)
                          (let* ((ah (mkhash)))
                            (foreach (a a) (ohashput ah a a))
                            (foreach-mappend (b b) (if (ohashget ah b) (wrap b) nil)))))
         (hs 
          (format astc (nexts-map
                        prevs-map
                        nodes-map
                        vars-map)
                  vars-map))
         ;; For each tag, list all nodes the tag can belong to
         (hpths1
          (hashmap (fun (k vs)
             `(,k
               ,@(foreach-map (v vs)
                   (ohashget hs v)))) tht))
         ;; Find the common nodes for all tags for each of the clusters
         (hpths2
          (foreach-map (h hpths1)
            (format h (k . vs)
              (cons k (if vs (foldl set-intersect (car vs) (cdr vs)))))))
         (ht (mkhash))
         )
    ;; Reverse cht
    (hashiter (fun (k v)
                (ohashput revcht v (cons k (ohashget revcht v)))) cht)
    ;; Mark implicit nodes
    (asteqns:iter eqns eqns
      (eqn _ ((nodetype (let* ((cl (ohashget cht id))
                               (rev (if cl (ohashget revcht cl))))
                          (foreach (r rev)
                            (ohashput implct r nd))))
              (else nil))))
    ;; Report errors
    (foreach (h hpths2)
      (format h (k . vs)
        (p:match vs
          (($a $b . $_)
           (alet chk (ohashget implct k)
                 (if chk (ohashput ht k chk)
                     (report k `(MANY-NODES ,(ohashget tht k) ::: ,@vs)))))
          (($a) (ohashput ht k a))
          (()
           (alet chk (ohashget implct k)
                 (if chk (ohashput ht k chk))
                 (report k `(NO-NODES ,(ohashget tht k) :::
                                ,(foreach-map (v (ohashget tht k))
                                   `(,v : ,@(ohashget hs v))))))))))
    ;; Apply
    (let* ((rht (mkhash)))
      (packrat:iter ntexprs src
        (ntexpr _
          ((terminal
            (let* ((chk (ohashget cht name)))
              (if chk (let* ((v (ohashget ht chk)))
                        (if v (begin
                                (ohashput rht name v)))))))
           (else nil))))
      rht))))

(function packrat-analyse-nobinaries (src)
  (collector (add get)
  (alet make_or
    (fun (l v0)
      (alet vs (foreach-map (vctr v0)
                 (format vctr (v ctr)
                   (with-syms (vn)
                      (alet nt 
                        `(terminal () term ,vn
                              (seq (bind L ,l)
                                   (bind op ,v)
                                   (bind R ,l))
                              ,ctr)
                        (add nt)
                        (return `(terminal ,vn))))))
            `(palt ,@vs ,l)))
  (alet ret
     (packrat:visit ntexprs src
      (ntexpr DEEP
        ((binaries
          (alet orexpr (make_or lr vs)
                `(terminal ,dtype term ,name ,orexpr (()()))))
         (else node)))
      (binvar DEEP (list op constr)))
     (return `(,@(get) ,@ret))))))
          
(function packrat-analyse-ast (src)
  (collector (add get)
  (alet nbsrc (packrat-analyse-nobinaries src)
   (packrat:iter ntexprs nbsrc
    (ntexpr _
       ((terminal (packrat-collect-terminal-ctors name add dtype value constr))
        (else nil))))
   (let* ((eqns (get))
          (tgs (packrat-asteqns-clusters eqns))
          (astdef (packrat-find-target-ast src))
          (astv (if astdef (ast2:default-ifun astdef))))
     (if astv
         (let* ((astc (ast-make-cache astv))
                (astm (packrat-ast-match astc tgs nbsrc eqns)))
           (packrat:visit ntexprs src
             (ntexpr _
               ((terminal
                 (if dtype node
                     (let* ((chk (ohashget astm name)))
                       (if chk (ast:mknode (dtype `((target ,chk)))) node))))
                (binaries
                 (if dtype node
                     (let* ((chk (ohashget astm name)))
                       (if chk (ast:mknode (dtype `((target ,chk)))) node))))
                (else node)))))
         src)))))
                  
;;;;;;;---------------------------------------

(function packrat-specialise-macro (loop defn args)
  (format defn (_ nm anms body ctor . reports)
     (with-hash (renm)
      (let* ((newnm (gensym)))
        (renm! nm `(terminal ,newnm))
        (foreach (a (zip anms args))
          (renm! (car a) (cadr a)))
        (cons newnm
              `(terminal () term
                         ,newnm
                         ,(loop
                          (packrat:visit expr body
                                          (expr DEEP 
                                                ((terminal
                                                  (alet t (renm> name)
                                                        (if t t node)))
                                                 (else node)))))
                         ,ctor ,@reports))))))

(function packrat-expand-macros (code) ;; after with-ignore expansion
  (with-hash (ahash ihash)
    (alet ncode 
      (foreach-mappend (c code)
          (p:match c
            ((define $nm $as $p $ct . $rl)
             (ahash! nm c)
             nil)
            (else (list c))))
      (collector (add get)
       (alet res
         (packrat:visit ntexprs ncode
          (expr _ (forall
                   (let loop ((c node))
                     (packrat:visit expr c
                       (expr DEEP 
                             ((macroapp
                               (let* ((sign (to-string `(,name ,@args)))
                                      (h (ihash> sign)))
                                 (if h h
                                     (let* ((xcode (packrat-specialise-macro loop (ahash> name)
                                                                             args))
                                            (nname (car xcode))
                                            (raised (cdr xcode)))
                                       (ihash! sign `(terminal ,nname))
                                       (add raised)
                                       `(terminal ,nname)))))
                              (else node))))))))
         (append (get) res)
         )))))

(function packrat-addignore-terminal (withignore cnode)
  (packrat:visit ntexpr cnode
      (expr _ (forall
               `(withignore ,withignore ,node)))))

(function packrat-preprocess-top-ignore (code)
  (collector (iadd iget)
     (packrat:iter ntexprs code
        (ntexpr _
          ((with-ignore (iadd igname))
           (else nil))))
     (alet igno (iget)
      (if (not igno) code
       (foreach-mappend (c code)
        (packrat:visit ntexpr c
           (ntexpr _
             ((terminal
               (cond 
                ((or (eqv? ttype 'token)
                     (eqv? ttype 'term)
                     )
                 (list (packrat-addignore-terminal igno node)))
                (else (list node))))
              (binaries (list (packrat-addignore-terminal igno node)))
              (define (list node))
              (targetast (list node))
              (else nil)))))))))

(function packrat-rule-subst (nm rn body)
  (let ((seed (if (string? body)
                  `(trivial (string ,@(map ascii (string->list body))))
                  body
                  )))
    (if (not rn) seed
        (packrat:visit expr rn
          (expr DEEP
                ((terminal
                  (if (eqv? name nm) seed node))
                 (else node))))
        )))

(function packrat-tokenreport (node)
  (p:match node
    ((withignore $ts $e) (packrat-tokenreport e))
    ((trivial (string . $s))
     (list (list->string (map n2s s))))
    ((trivial (sstring $s))
     (list s))
    ((trivial (char $s))
     (list (list->string (list (n2s s)))))
    (else nil)))

(function packrat-raise-tokens (rcache cache tadd value)
  (packrat:visit expr value
   (expr DEEP
     ((trivial 
       (let* ((s (to-string node))
              (atmpt (hashget cache s)))
         (if atmpt
             `(terminal ,atmpt)
             (let* ((tnm (gensym))
                    (rn (hashget rcache "lexical"))
                    (rb (packrat-rule-subst 'lexical (if rn (car rn) nil) node))
                    )
               (hashput cache s tnm)
               (tadd `(terminal () token ,tnm ,rb ,(if rn (cadr rn) '(() ()))
                                ,@(packrat-tokenreport node)))
               `(terminal ,tnm)))))
      (rule
       (let* ((rn (hashget rcache name))
              (rb (packrat-rule-subst name (if rn (car rn) nil) body))
              (s (to-string rb))
              (atmpt (hashget cache s)))
         (if atmpt
             `(terminal ,atmpt)
             (let ((tnm (gensym)))
               (hashput cache s tnm)
               (tadd `(terminal () token ,tnm ,rb ,(if rn (cadr rn) '(() ()))
                                ,@(packrat-tokenreport node)))
               `(terminal ,tnm)))))
      (else node)))))

(function packrat-preprocess-top-tokens-2 (rcache cache code) ;; Lift tokens
  (collector (tadd tget)
   (alet res
     (packrat:visit ntexprs code
       (binvar _ (ast:mknode (op (packrat-raise-tokens rcache cache tadd op))))
       (ntexpr DEEP
        ((terminal
          (cond
           ((eqv? ttype 'term)
            (ast:mknode (value (packrat-raise-tokens rcache cache tadd value))))
           (else node)))
         (binaries
          (ast:mknode (lr (packrat-raise-tokens rcache cache tadd lr))))
         (else node))))
     (append (tget) res))))

(function packrat-preprocess-top-tokens (code)
  (let ((cache (mkhash))
        (rcache (mkhash)))
    (packrat:iter ntexprs code
      (ntexpr _
        ((terminal
          (cond 
           ((eqv? ttype 'token)
            (hashput cache (to-string value) name))))
         (rule
          (hashput rcache name (list e constr)))
         (else nil))))
    (packrat-preprocess-top-tokens-2 rcache cache code)))

(function packrat-preprocess-autonames (code)
  (alet terminner (fun (c name)
                    (packrat:visit ntexpr c
                         (dualcode _ 
                             `(,a ,(p:match c
                                     ((auto . $tagname)
                                      (if (null? tagname) `(auto ,name)
                                          c))
                                     (else c)
                                     )))))
    (packrat:visit ntexprs code
        (ntexpr DEEP
           ((terminal (terminner node name))
            (define (terminner node name))

            (else node))))))

(function __packrat-p0-top-expr (liftadd tname e dtype0)
  (packrat:visit expr e
    (expr DEEP
          ((lift 
            (alet t (Sm<< (if tname (S<< tname "_") "") (gensym))
                  (liftadd `(terminal ,(if dtype dtype dtype0)
                                      term ,t ,e ,c ,@r))
                  `(terminal ,t)))
           (else node)))
    ))

(function packrat-preprocess-top-0 (code)
  (collector (liftadd liftget)
     (alet res
       (packrat:visit ntexprs code
         (ntexpr _
           ((terminal
             (ast:mknode (value (__packrat-p0-top-expr liftadd name value dtype))))
            (binaries
             (ast:mknode (lr (__packrat-p0-top-expr liftadd name lr dtype))
                         (vs (foreach-map (v vs)
                               (format v (prec assoc op constr . rep)
                                       `(,prec ,assoc ,(__packrat-p0-top-expr liftadd name op dtype)
                                               ,constr ,@rep))))))
            (define
             (ast:mknode (e (__packrat-p0-top-expr liftadd name e nil))))
            (rule 
             (ast:mknode (e (__packrat-p0-top-expr liftadd name e nil))))
            (else node))))
       (append (liftget) res))))

(function packrat-binaries-extract (vs rep)
  (let loop ((se nil)
             (e0 nil)
             (v vs)
             (c nil))
    (p:match v
      (((binary $prec $assoc (seq $l $o $r) $ctor) . $rest)
       (let ((e (if (null? e0) l e0)))
         (if (not (iso l r))
             (ccerror `(PACKRAT-PRATT-NOT-MATCHING ,l ,r)))
         (if (not (iso e r))
             (ccerror `(PACKRAT-PRATT-NOT-MATCHING ,e ,r)))
         (loop se l rest (cons `(,prec ,assoc ,o ,ctor ,@rep) c))))
      (((simple $e))
       (loop e e0 nil c))
      (()
       (if se
           (cons se (reverse c))
           (ccerror `(PACKRAT-PRATT-NO-EXIT))))
      (else (ccerror `(PACKRAT-PRATT-FORMAT ,v))))))

(function packrat-preprocess-binaries (code)
  (packrat:visit ntexprs code
     (ntexpr DEEP
             ((src-binaries
               (let* ((ff (packrat-binaries-extract vs r)))
                 `(binaries ,dtype ,ttype ,name ,(car ff) ,(cdr ff))))
              (else node)))))

(function packrat-preprocess-top (code)
 (packrat-analyse-ast
  (packrat-detect-recursion
   (packrat-preprocess-top-0
    (packrat-preprocess-autonames
     (packrat-preprocess-top-ignore
      (packrat-preprocess-top-tokens
       (packrat-preprocess-binaries
        code))))))))

(function packrat-borrow-macros (lst)
  (foreach-mappend (l lst)
    (shashget (getfuncenv)
              (Sm<< "peg-exportmacros-" l "-src"))))

(function packrat-borrow-ranges (lst)
  (foreach-mappend (l lst)
    (shashget (getfuncenv)
              (Sm<< "peg_" l "_ranges"))))

(function packrat-get-deps (nm)
  (shashget (getfuncenv)
            (Sm<< "peg-exportdepends-" nm)))

(function packrat-expand-borrow (borrow0)
  (collector (dadd dget)
     (foreach (i borrow0)
       (foreach (v (packrat-get-deps i)) (dadd v)))
     (unifiq (append (dget) borrow0))))

(include "./backend-ast.al");; 

(define packrat-prep-plugins (mkref nil))

(function packrat-apply-plugins (entry prep)
  (foldl (fun (l r) 
           (r entry l)) prep (deref packrat-prep-plugins)))

(function packrat-ast-f0 (exp? entry borrow0 code)
  (let* ((borrow (packrat-expand-borrow borrow0))
         (localmacros 
          (collector (madd mget)
                     (packrat:iter ntexprs code
                                   (ntexpr DEEP
                                           ((define (madd node))
                                            (else nil))))
                     (mget)))
         (borrowmacros (packrat-borrow-macros borrow))
         (prep0  
              (packrat-preprocess-top 
                (packrat-expand-macros
                 (append borrowmacros code))))
         (prep (packrat-apply-plugins entry prep0))
         (dynhooks (filter (fmt (a . r) (eqv? a 'dynahook)) code))
         (result 
          (translate-all entry borrow prep localmacros
                         dynhooks exp?)))
    ;;;;;;;;;;;;
    result))

(function packrat-ast-f (entry borrow0 code)
  (packrat-ast-f0 #t entry borrow0 code))

(macro packrat-ast (entry borrow . code)
  (packrat-ast-f entry borrow code))

(macro packrat-ast-ne (entry borrow . code)
  (packrat-ast-f0 nil entry borrow code))
