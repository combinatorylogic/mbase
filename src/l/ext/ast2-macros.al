
(macro def:ast:new (id . rest)
  (let* ((l1 (ast-front-lower `(def:ast:new ,id ,@rest)))
         (l2 (ast-merge-inherited ast2:default-ifun l1)))
    (shashput (getfuncenv)
              (ast2:ast-var-name id)
              l2)
    `(ctime (begin
              ))))

(macro ast:visit:new (srcid . rest)
  (let* ((srch   (mkhash))
         (gettag (fun (h tg) (ohashget h tg)))
         (tgfun  (fun (id tg)
                   (let* ((chk (ohashget srch id)))
                     (if chk
                         (gettag chk tg)
                         (let* ((asrc (ast2:default-ifun id))
                                (h (ast-variants-hash asrc)))
                           (ohashput srch id h)
                           (gettag h tg))))))
         (l1 (ast-visitor-lower tgfun
                                `(ast:visit:new ,srcid ,@rest)))
         (dstid (ast2:get-dstid l1))
         (srcast (ast2:default-ifun srcid))
         (dstast (ast2:default-ifun dstid))
         (l2 (visitor-fuse srcast dstast l1)) ;; TODO!!!!
         (l3 (visitor-inject-listnodes l2))
         (l4 (visitor-refine-else (visitor-backend-prepare l3)))
         (l5 (visitor-backend-lowering l4))
         (opts (visitor-get-options l5))
         (lfsrc (ohashget opts 'listform_src))
         (lfdst (ohashget opts 'listform_dst))
         (l6 (visitor-lower-further lfsrc (visitor-populate-varids l5)))
         (l7 (visitor-backend-mbase lfsrc lfdst l6)))
    (return l7)))


(macro ast2:from-listform (srcid entry val)
  (let* ((src (ast2:default-ifun srcid))
         (nodes (astlang:visit topdef src
                  (topdef DEEP ((defast ns)))
                  (astnode DEEP
                    ((simple `(S ,id))
                     (varnode `(V ,id))
                     (extend (ccerror `(AST-IMPOSSIBLE ,srcid)))))))
         (body (foreach-map (n nodes)
                 (p:match n
                   ((V $id)
                    `(,id DEEP ((else (thisnode)))))
                   ((S $id)
                    `(,id DEEP (thisnode)))))))
    `(ast:visit:new ,srcid ((dst ,srcid)
                            listform_src)
        ,entry
        ,val
        ,@body)))


(macro ast2:to-listform (srcid entry val)
  (let* ((src (ast2:default-ifun srcid))
         (nodes (astlang:visit topdef src
                  (topdef DEEP ((defast ns)))
                  (astnode DEEP
                    ((simple `(S ,id))
                     (varnode `(V ,id))
                     (extend (ccerror `(AST-IMPOSSIBLE ,srcid)))))))
         (body (foreach-map (n nodes)
                 (p:match n
                   ((V $id)
                    `(,id DEEP ((else (thisnode)))))
                   ((S $id)
                    `(,id DEEP (thisnode)))))))
    `(ast:visit:new ,srcid ((dst ,srcid)
                            listform_dst)
                    ,entry
                    ,val
                    ,@body)))


(macro ast2:visit-all (srcid entry val fn)
  (let* ((src (ast2:default-ifun srcid))
         (nodes (astlang:visit topdef src
                  (topdef DEEP ((defast ns)))
                  (astnode DEEP
                    ((simple `(S ,id))
                     (varnode `(V ,id))
                     (extend (ccerror `(AST-IMPOSSIBLE ,srcid)))))))
         (body (foreach-map (n nodes)
                 (p:match n
                   ((V $id)
                    `(,id DEEP ((else (,fn (thisnode))))))
                   ((S $id)
                    `(,id DEEP (,fn (thisnode))))))))
    `(ast:visit:new ,srcid ((dst ,srcid))
        ,entry
        ,val
        ,@body)))

;; TODO: error checks
(macro ast2:ctr-inner (args nd dsttags astdst)
  (let* ((ast (ast2:default-ifun astdst))
         (h (ast-make-cache ast))
         (nf (ast-get-simple-node-pattern h nd))
         )
    `(ast2:mknode-inner ,args
                        ,nd
                        simple
                        ,nf
                        ()
                        ,dsttags
                        ,astdst
                        )))

(macro ast2:ctr (nd . args)
  `(inner-expand-first
    ast2:ctr-inner
    (quote ,args)
    ,nd
    (ast-dst-tags)
    (ast-visitor-to)
    ))

(macro ast2:vctr-inner (args nd vrnt dsttags astdst)
  (let* ((ast (ast2:default-ifun astdst))
         (_ (if (not ast)
                (ccerror `(CANNOT-FIND-AST ,astdst))))
         (h (ast-make-cache ast))
         (nf (ast-get-variant-pattern h nd vrnt))
         )
    `(ast2:mknode-inner ,args
                        ,nd
                        variant
                        ,nf
                        ,vrnt
                        ,dsttags
                        ,astdst
                        )))

(macro ast2:vctr (nd vrnt . args)
  `(inner-expand-first
    ast2:vctr-inner
    (quote ,args)
    ,nd ,vrnt
    (ast-dst-tags)
    (ast-visitor-to)
    ))

(macro ast2:xctr (vrnt . args)
  `(inner-expand-first
    ast2:vctr-inner
    (quote ,args)
    (ast-node)
    ,vrnt
    (ast-dst-tags)
    (ast-visitor-to)
    ))


(macro ast2:with-ast (id . body)
  (let* ((ast (ast2:default-ifun id))
         (tags (astlang:visit topdef ast (topdef _ ((defast taglist))))))
    `(ast2:setup_macros ((ast-dst-tags ,tags)
                         (ast-visitor-to ,id)
                         ;; todo: ...
                         )
       (begin ,@body))))


;; Construct an ast node in a free form, knowing the destination AST structure and
;;  topmost target node type.
;; This is similar to what a recform peg constructor is doing, but suitable for
;; pfront.

;(macro ast2:freeform-ctr (ast body)





(macro ast:visit:r2 (src0 dst0 srctop . entries)
   (let* ((src (cadr src0)) (dst (cadr dst0))
          (newsrcentry (format src (_ a e) e))
          (newsrcid (format src (_ a e) a))
          (newdstid (format dst (_ a x) a)))
     `(ast:visit:new ,newsrcid ((dst ,newdstid)) ,newsrcentry ,srctop ,@entries)))

(macro ast2:llctor (astnm nodenm tag . args)
  (let* ((ast (ast2:default-ifun astnm))
         (dsttags (astlang:visit topdef ast (topdef _ ((defast taglist)))))
         (_ (if (not ast)
                (ccerror `(CANNOT-FIND-AST ,astnm))))
         (h (ast-make-cache ast))
         (nf (ast-get-variant-pattern h nodenm tag))
         (nf1 (ast-pattern-entries nf))
         (newargs (foreach-map (x (zip args nf1))
                      (format x (a1 (xtp xnm xpos))
                       `(,xnm ,a1)))))
    `(ast2:setup_macros ((ast-node-is-top ()))
      (ast2:mknode-inner (quote ,newargs)
                        ,nodenm
                        variant
                        ,nf
                        ,tag
                        ,dsttags
                        ,astnm
                        ))))
