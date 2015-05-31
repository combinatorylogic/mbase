;;;;; TMP: move to the core

(function common-check-ident (id)
  (if (not (symbol? id))
      (ccerror `(MACRO-EXPECTED-A-SYMBOL ,id))
      id))

(function common-check-list (p)
  (let loop ((l p))
    (p:match l
      (($hd . $tl) (loop tl))
      (() p)
      (else (ccerror `(MACRO-EXPECTED-A-LIST ,p))))))

(function ast-front-lower (src)
  "Transform the frontend def:ast format into astlang"
  (let* (;; Lower the inheritance syntax
         (translate-includes
          (fun (incl)
            (p:match incl
              ($$M:id `(incl ,(common-check-ident id)))
              (($id . $rewrites)
               `(incl ,id ,@(foreach-map (r rewrites)
                              (p:match r
                                (($f $t) `(rename ,(common-check-ident f)
                                                  ,(common-check-ident t)))
                                (else (ccerror `(AST-FRONT-REWRITE-FORMAT ,r))))
                              )))
              (else (ccerror `(AST-FRONT-INHERITANCE-FORMAT ,incl))))))
         ;; (<p1> <p2> ... . <p3>) ->
         ;;    (tuple <p1> <p2> ... (append <p3>))
         ;; <node:id> => (entry node id 'ref)
         ;; <*node:id> => (entry node id 'mul)
         ;; <?node:id> => (entry node id 'opt)
         ;; () -> (nil)

         ;; Translate a single symbol pattern
         (translate-single-pattern
          (fun (s)
            (let* ((p (_ast_parse_sym s)))
              (p:match p
                ((CONST $id) `(entry ,id ,id ref))
                ((NODE $ref $n) `(entry ,ref ,n ref))
                ((NODES $ref $n) `(entry ,ref ,n mul))
                ;; TODO: parse <?node:id>
                (else (ccerror `(AST-FRONT-SINGLE-PATTERN ,s)))
                ))))

         ;; Translate a compound pattern.
         (translate-pattern
          (fun (p0)
            (let loop ((p p0) (mode 'none) (ctx nil))
              (cond
               ((eqv? mode 'none)
                (p:match p
                  (($hd . $tl) (loop tl 'tuple (list (loop hd 'none nil))))
                  ($$M:v (translate-single-pattern v))
                  (() '(nil))
                  ))
               ((eqv? mode 'tuple)
                (p:match p
                  (($hd . $tl) (loop tl 'tuple (append ctx (list (loop hd 'none nil)))))
                  ($$M:v `(tuple ,@ctx (append ,(loop v 'none nil))))
                  (() `(tuple ,@ctx))
                  ))
               ))))
         
         ;; Lower non-variant node definition
         (translate-simple-def
          (fun (id ptn)
            `(simple ,id ,(translate-pattern ptn))))
         
         ;; Lower variant node definiton 
         (translate-var-def
          (fun (id vars)
            `(varnode ,id
                      ,@(foreach-map (v vars)
                          (p:match v
                            (($tag . $ptn)
                             `(v ,tag ,(translate-pattern ptn)))
                            (else (ccerror `(AST-FRONT-VARIANT-FORMAT ,v))))))))
         
         ;; Lower extension node definition
         (translate-extend-def
          (fun (id vars)
            `(extend ,id
                      ,@(foreach-map (v vars)
                          (p:match v
                            (($tag . $ptn)
                             `(add (v ,tag ,(translate-pattern ptn))))
                            (else (ccerror `(AST-FRONT-VARIANT-FORMAT ,v))))))))
         
         ;; Lower the node definitions
         (translate-defs
          (fun (defn)
            (p:match defn
              (($id (| . $vars))
               (let* ((p (_ast_xnparse (common-check-ident id))))
                 (p:match p
                   (($xid) (translate-extend-def xid (common-check-list vars)))
                   ($xid (translate-var-def id (common-check-list vars))))))
              (($id $ptn)
               (translate-simple-def (common-check-ident id) ptn))
              (else (ccerror `(AST-FRONT-NODE-FORMAT ,defn))))))
         )
    
    ;; Lower the def:ast macro body
    (p:match src
      ((def:ast:new $id $incl
         . $defs)
       `(defast ,(common-check-ident id)
          ,(map translate-includes (common-check-list incl))
          () ;; tags
          () ;; options
          ,@(map translate-defs (common-check-list defs))))
      (else (ccerror `(AST-FRONT-FORMAT ,src)))
      )))


(function ast-variants-hash (src)
  (let* ((ht (mkhash)))
    (astlang:visit topdef src
                   (astnode DEEP
                            ((varnode (ohashput ht id id))
                             (else node))))
    ht))

;; Lower the visitor
(function ast-visitor-lower (varfunc src)
  (let* (
         (getopt (fun (o)
                   (p:match o
                     (listform_src '(listform_src))
                     (listform_dst '(listform_dst))
                     (fullcopy '(fullcopy))
                     (else (ccerror `(AST-VISITOR-UNKNOWN-OPTION ,o))))))
         ;; Transform a list of variants
         ;; which may end with else-deep or else
         ;; into separate lists of normal variants,
         ;; else-deep variants and an else node.
         (translate-split-variants
          (fun (code)
            (collector (addvar getvars)
            (collector (adddeep getdeeps)
            (let ((el (mkref nil)))                     
              (let loop ((cx code)
                         (add addvar))
                (common-check-list cx)
                (foreach (c cx)
                  (p:match c
                    ((else-deep . $cx)
                     (loop cx adddeep))
                    ((else . $x) (r! el x))
                    (($tag . $c)
                     (add `(v ,(common-check-ident tag) (begin ,@c))))
                    (else (ccerror `(AST-VISITOR-VARIANT-FORMAT ,c))))))
              `(vars
                ,(getvars)
                ,(getdeeps)
                ,(if (deref el)
                     `(velse (begin ,@(deref el)))
                     `(none))))))))
         
         ;; Translate a single entry, taking its nature (variant vs. simple)
         ;; into account.
         (translate-entry-body
          (fun (srcid os ntag nref code tp)
            (let* ((n (if (eqv? ntag nref) `(id ,ntag) `(as ,ntag ,nref))))
              (if (varfunc srcid ntag)
                  `(,tp ,n ,os ,(p:match code
                               ((forall . $rest)
                                `(forall (begin ,@rest)))
                               (else (translate-split-variants code))))
                  `(,tp ,n ,os (simple ,code))))))

         ;; Translate a single entry, differentiate by visiting order
         ;; (once vs. deep), take possible dereferencing into account.
         (translate-entry-hl
          (fun (srcid ntag opts tp code)
               (let* ((tagtype (p:match ntag
                                 (($a $b) b)
                                 (else ntag))))
                 (case tp
                   ((TERM _ T)
                    ;; Once
                    (translate-entry-body srcid opts ntag ntag code 'once)
                    )
                   ((DEREFT)
                    ;; Deref-once
                    (translate-entry-body srcid opts tagtype (car ntag) code 'once)
                    )
                   ((DEREF)
                    ;; Deref-deep
                    (translate-entry-body srcid opts tagtype (car ntag) code 'deep)
                    )
                   ((AFIRST A DEEP)
                    ;; Deep
                    (translate-entry-body srcid opts ntag ntag code 'deep)
                    )))))
         (translate-entry
          (fun (srcid e)
            (p:match e
              (($ntag -> $dtag $tp $code)
               (translate-entry-hl srcid ntag `((dstnode ,dtag)) tp code))
              (($ntag $tp $code)
               (translate-entry-hl srcid ntag nil tp code))
              (else (ccerror `(VISITOR-ENTRY ,e))))))
         )
  
  ;; Translate a visitor DSL macro body.
  (p:match src
    ((ast:visit:new $asttp $opts $top $e . $entries)
     (let* ((dstopts
             (let loop ((o opts) (dst nil) (os nil))
               (p:match o
                 (((dst $astdst) . $rest)
                  (loop rest `(,astdst) os))
                 (($hd . $rest)
                  (loop rest dst (cons (getopt hd) os)))
                 (else
                  `(,dst ,os))))))
     `(visitor visit
               ,(common-check-ident asttp)
               ,@dstopts
               ,e
               ,top

               ,@(map (fun (e) (translate-entry asttp e))
                   (common-check-list entries))))))
  ))

(function ast2:get-dstid (src)
  (visitorlang:visit visitorexpr src
    (visitorexpr DEEP
      ((visitor
        (if to (car to) from))))))


                   