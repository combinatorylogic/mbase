
(function ast0-apply-hints (hints nd)
  (astlang:visit astnode nd
     (astnode DEEP
       ((simple node)
        (varnode (let* ((nodehints (ohashget hints id)))
                   (ast:mknode (vs (foreach-map (v vs)
                                     (v id nodehints))))))
        (else node)))
     (variant DEEP
       ((v (fun (nodeid nodehints)
             (let* ((varianthints (ohashget hints (Sm<< nodeid ":" tag))))
               (ast:mknode (hs (append varianthints (append nodehints hs)))))))))))

(function ast-ns-cleanup (lst)
  (let ((ht (mkhash)))
    (foreach-mappend (l lst)
       (let* ((nm (cadr l))) ;; Expect id to always be a cadr
         (if (ohashget ht nm) nil
             (begin
               (ohashput ht nm nm)
               (wrap l)))))))
              
;; Lower an AST definition, merging all the inherited ASTs.
;; (ifun <id>) is used to fetch a definition for the current context.
(function ast-merge-inherited (ifun src)
  "Merge the inherited AST bodies, use ifun to fetch the referenced ASTs"
  (collector (add get)
  (collector (tagsadd tagsget)
  (let* ((rewriters (mkhash))
         (hints (mkhash))
         (addvarhint (fun (nd vr hs)
                       (alet nm (Sm<< nd ":" vr)
                             (ohashput hints nm (append hs (ohashget hints nm))))))
         (addnodehint (fun (nd hs)
                        (ohashput hints nd (append hs (ohashget hints nd))))))
         
    (astlang:visit topdef src
      (extvariant DEEP
        ((remove `(R ,tag)) (add v)))
      (astnode DEEP
         ((extend
           (let* ((delh (mkhash))
                  (exts (foreach-mappend (v vs)
                           (p:match v
                             ((R $tag) (begin
                                         (ohashput delh tag tag)
                                         nil))
                             (else (wrap v))))))
              (ohashput rewriters id
               (fun (n)
                 (astlang:visit astnode n
                   (astnode DEEP
                      ((varnode
                        (ast:mknode (vs (append (foreach-mappend (v vs) v)
                                                exts))))
                       (else node)))
                   (variant DEEP
                     ((v (let* ((t (ohashget delh tag)))
                           (if t nil (wrap node))))))))
               )
              node))
          (extendvarhint (addvarhint id tg hints))
          (extendnodehint (addnodehint id hints))
          (else node))))

    (astlang:visit topdef src
       (rewrite DEEP
         ((rename (fun (h1 h2)
                    (ohashput h1 f t)))
          (delete (fun (h1 h2)
                    (ohashput h2 t t)))))
       (astsrc DEEP
         ((incl
           (let* ((isrc (ifun name))
                  (rwh (mkhash))
                  (delh (let* ((h2 (mkhash)))
                          (foreach (r rs) (r rwh h2))
                          h2))
                  (check (fun (id n)
                           (if (ohashget delh id) nil
                               (let* ((rwtr (ohashget rewriters id)))
                                 (if rwtr (wrap (rwtr n))
                                     (wrap n))))))
                  (rw (hashmap (fun (k v) k) rwh))
                  (rwsrc
                   (astlang:visit topdef isrc
                         (topdef DEEP
                                 ((defast (foreach-mappend (n ns) n))))
                         (astnode DEEP
                                  ((simple (check id node))
                                   (varnode (check id node))
                                   (extend
                                    (ccerror `(AST-MERGE-EXTEND ,node)))))

                         (nodedefident DEEP
                           (let* ((chk (ohashget rwh node)))
                             (if chk chk node))))
                   ))
             (astlang:iter topdef isrc
               (topdef _
                 ((defast (tagsadd taglist)))))
             (foreach (n rwsrc) (add n)))))))
    (let* ((ntags (collector (addnewtag getnewtags)
                     (astlang:iter topdef src
                       (variant DEEP ((v (addnewtag tag)))))
                     (let* ((t0 (getnewtags))
                            (t1 (unifiq t0))
                            (t2 (qsort string<? (foreach-map (s t1) (S<< s)))))
                       (foreach-map (t t2) (Sm<< t)))))
           (tagsht (mkhash))
           (otags (tagsget))
           (mergetags (foreach-mappend (ts (append otags (list ntags)))
                         (foreach-mappend (t ts)
                            (if (ohashget tagsht t) nil
                                (begin
                                  (ohashput tagsht t t)
                                  (wrap t))))))
           (nopts (if (> (length otags) 1) '((multisrc)) '())))
     (astlang:visit topdef src
      (astnode DEEP
        ((extend nil)
         (extendvarhint nil)
         (extendnodehint nil)
         (else (wrap node))))
      (topdef DEEP
              ((defast
                 (ast:mknode (srcs nil)
                             (taglist mergetags)
                             (options (append nopts options))
                             (ns
                              (reverse
                              (ast-ns-cleanup
                              (reverse  
                              (foreach-map (n 
                                            (append (get)
                                                    (foreach-mappend (n ns) n)
                                                    ))
                                (ast0-apply-hints hints n))))))
                              ))))))))))




