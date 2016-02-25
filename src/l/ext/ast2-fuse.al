
;; AST definition need to be processed first in order to be useful.
;;     Cache structure is:
;;      ( <nexts-map>
;;        <prevs-map>
;;        <node-definitions-map>
;;        <variants-map> )

(function pattern-get-refs (p)
  "Get a list of entries mentioned in a pattern"
  (collector (add get)
    (astlang:visit pattern p
       (nodeident _ (add node)))
    (return (unifiq (get)))))


(function ast-make-cache (srcast)
  (let* ((nexts-map (mkhash))
         (prevs-map (mkhash))
         (nodes-map (mkhash))
         (node!     (fun (nm n)
                      (ohashput nodes-map nm n)))
         (vars-map  (mkhash))
         (variant+! (fun (id tag n)
                      (ohashput vars-map tag (cons id (ohashget vars-map tag)))
                      (ohashput vars-map (Sm<< id " : " tag) n)))
         (make-map  (fun (f ts)
                      (foreach (t ts)
                        (ohashput nexts-map f (unifiq (cons t (ohashget nexts-map f))))
                        (ohashput prevs-map t (unifiq (cons f (ohashget prevs-map t))))
                        )))
         )
    ;; 1. Build nexts and prevs cache
    (astlang:visit topdef srcast
       (astnode DEEP
          ((simple   (make-map id p))
           (varnode  (make-map id (foldl append nil vs)))
           (extend   (ccerror `(IMPOSSIBLE)))))
       (pattern _    (forall (pattern-get-refs node)))
       (variant DEEP ((v p)))
       )
    ;; 2. Build nodes cache
    (astlang:visit topdef srcast
       (astnode _
          ((simple  (node! id node))
           (varnode (node! id node))
           (extend  (ccerror `(IMPOSSIBLE))))))
    ;; 3. Build variants cache
    (astlang:visit topdef srcast
       (astnode DEEP
          ((varnode (foreach (v vs) (v id)))
           (else node)))
       (variant DEEP
          ((v (fun (id)
                (variant+! id tag node)))))
       )
    ;; Return the cache structure
    (return (list nexts-map
                  prevs-map
                  nodes-map
                  vars-map))
    ))

(function ast-node-get-refs (ast f)
  (format ast (n p . _)
     (ohashget n f)))

(function ast-node-get-prevs (ast f)
  (format ast (n p . _)
          (ohashget p f)))

(function ast-cache-nodedef (ast ni)
  (format ast (n p d v)
          (ohashget d ni)))


(function ast-get-variant-node-full-pattern (ast ni)
  (format ast (nm p d v)
     (let* ((n (ohashget d ni)))
       (if (not n) (ccerror `(MISSING-NODE1 ,ni)))
       (astlang:visit astnode n
          (astnode _
            ((varnode vs)
             (else (ccerror `(NOT-A-SIMPLE-NODE ,n)))))))))

(function ast-get-simple-node-pattern (ast ni)
  (format ast (nm p d v)
     (let* ((n (ohashget d ni)))
       (if (not n) (ccerror `(MISSING-NODE2 ,ni)))
       (astlang:visit astnode n
          (astnode _
            ((simple p)
             (else (ccerror `(NOT-A-SIMPLE-NODE ,n)))))))))

(function ast-get-variant-node-body (ast ni)
  (format ast (nm p d v)
    (let* ((n (ohashget d ni)))
       (if (not n) (ccerror `(MISSING-NODE3 ,ni)))
       (astlang:visit astnode n
          (astnode _
            ((varnode vs)
             (else (ccerror `(NOT-A-VARIANT-NODE ,n)))))))))

(function ast-get-variant-node-body-tmp (ast ni)
  (format ast (nm p d v)
    (let* ((n (ohashget d ni)))
      (if n
       (astlang:visit astnode n
          (astnode _
            ((varnode vs)
             (else (ccerror `(NOT-A-VARIANT-NODE ,n))))))
       '()))))

(function ast-get-variant-pattern (ast ni tag)
  (format ast (nm p d v)
    (let* ((n (ohashget v (Sm<< ni " : " tag))))
       (if (not n) (ccerror `(MISSING-TAG ,ni ,tag)))
       (astlang:visit variant n
          (variant _
            ((v p)))))))



(function ast-get-node (ast ni)
  (format ast (n p d v)
    (ohashget d ni)))

(function visitor-all-entries (vis)
  (collector (add get)
  (visitorlang:visit visitorexpr vis
     (visitorentry DEEP
        ((deep (add n)) (once (add n))))
     (noderef DEEP ((id id) (as id)))
     )
  (return (get))))

(function visitor-build-paths (ast entry vis)
  "Build a table of nodes lying on the paths reaching the explicit nodes"
  ;; Paths are marked by walking upwards from the explicit nodes, not repeating
  ;; the already visited nodes.
  ;; It may be an excessive list, but it's going to be cut down by the next, similar step,
  ;; this time walking down from the entry node and breaking in the 'once' nodes.
  (let* ((visited-hash (mkhash))
         (visited! (fun (v) (ohashput visited-hash v v)))
         (visited? (fun (v) (ohashget visited-hash v)))
         (visitor-entries (visitor-all-entries vis))
         (onpath (mkhash))
         (onpath! (fun (v)
                    (ohashput onpath v v)))
         )
    (let loop ((front visitor-entries))
      (collector (addnext getnext)
         (foreach (f front)
           (if (not (visited? f))
               (let* ((es (ast-node-get-prevs ast f)))
                 (visited! f) (onpath! f)
                 (foreach (e es)
                   (if (not (visited? e))
                       (begin
                         (onpath! e)
                         (addnext e)))))))
         (let* ((nexts (unifiq (getnext))))
           (if nexts (loop nexts)))))
    (return onpath)))

(function visitor-list-stop-entries (vis)
  "An utility function, returns a hash map with stop entries"
  ;; Entries with else-deep are not considered 'stop'
  (let* ((stop (mkhash))
         (stop! (fun (v) (ohashput stop v v))))
    (visitorlang:iter visitorexpr vis
       (noderef _ ((id id) (as rn)))
       (visitorentry DEEP
          ((once (if (not (eqv? p 'ELSE-DEEP)) (stop! n)))
           (else node)))
       (visitorptn DEEP
          ((vars (if ds 'ELSE-DEEP 'NONE))
           (else 'NONE))))
    (return stop)))

(function visitor-fuse-implicit-implnodes (ast entry vis)
  "Extract a list of imlicitly requirede nodes"
  ;; ast cache should already have a full dependency graph.
  ;; We have to start from the given entry (caller must make sure it's not a mock entry)
  ;; and follow all the paths from the entry to all the nodes, stopping at the 'once' nodes.
  (let* ((visited-hash (mkhash))
         (visited! (fun (v) (ohashput visited-hash v v)))
         (visited? (fun (v) (ohashget visited-hash v)))
         (paths (visitor-build-paths ast entry vis))
         (visitor-entries (visitor-all-entries vis))
         (visitor-entries-h (let ((h (mkhash)))
                              (foreach (v visitor-entries) (ohashput h v v))
                              (return h)))
         (in-a-path? (fun (v) (ohashget paths v)))
         (stop-nodes (visitor-list-stop-entries vis))
         (stop-here? (fun (n) (ohashget stop-nodes n))))
   (let loop ((front (list entry)))
     (collector (addnext getnext)
      (foreach (f front)
         (if (in-a-path? f)
             (begin
               (visited! f)
               (let* ((es (ast-node-get-refs ast f)))
                 (foreach (e es)
                   (if (and (not (visited? e))
                            (not (stop-here? e)))
                       ;; TODO: fine-grained analysis for the partially stop nodes, like
                       ;;       those with else-deep. Currently the else-deep nodes
                       ;;       are considered fully 'deep'
                       (addnext e)
                       )))
               )))
       (let ((ns (getnext)))
         (if ns (loop ns)))))
  ;; All the nodes visited by the loop above are supposed to be served by the visitor
   (return (foreach-mappend (k (hashmap (fun (k v) k) visited-hash))
             (if (ohashget visitor-entries-h k) nil (wrap k))))))

(function visitor-compile-nodedef (astS astD ndef)
  "Compile an implicit node matcher"
  ;; Actually, not really "compile", patterns remain unexpanded at this stage
  ;; We only have to construct an appropriate skeleton containing all the relevant
  ;; definitions.
  (astlang:visit astnode ndef
     (astnode _
       ((simple `(deep (id ,id)
                       () ; opts
                       (simple ,p ,(ast-get-simple-node-pattern astD id)
                               (implicit_ctor ,p))))
        (varnode
         `(deep (id ,id)
            () ; opts
            (vars ,vs
              ,(ast-get-variant-node-body-tmp astD id)
              ,(foreach-map (v vs)
                 (astlang:visit variant v
                    (variant _ ((v
                       `(v ,tag
                           (implicit_ctor_tag ,id ,tag ,p)
                           ))))))
              ,(foreach-map (v vs)
                 (astlang:visit variant v
                    (variant _ ((v
                       `(v ,tag
                           (implicit_ctor_tag ,id ,tag ,p)
                           ))))))
              (none))))
        (extend (ccerror `(IMPOSSIBLE))))))
  )

(function visitor-fuse-implicit (astS astD entry vis)
  "Fuse implicitly required nodes into a visitor"
  (let* ((implnodes (visitor-fuse-implicit-implnodes astS entry vis)))
    (foreach-map (n implnodes)
      (let* ((ndef (ast-cache-nodedef astS n)))
        (visitor-compile-nodedef astS astD ndef)))))

(function visitor-fuse (srcast dstast vis)
  "Fuse srcast and dstast into a visitor code"
  (let* ((get-id (fun (n)
                   (visitorlang:visit noderef n
                     (noderef _ ((id id) (as id))))))
         (astS (ast-make-cache srcast))
         (astD (ast-make-cache dstast))
         (tgtnode (fun (n os)
                    (let* ((chk (find (fun (k) (eqv? (car k) 'dstnode)) os)))
                      (if os (cadr chk) n))))
         )

    (visitorlang:visit visitorexpr vis
       ;; Pass node references to the patterns
       (visitorentry DEEP
         ((deep (ast:mknode (p (p (get-id n) os))))
          (once (ast:mknode (p (p (get-id n) os))))))
       ;; Insert the relevant patterns
       ;; The most interesting here is forall:
       ;; It needs the full variant node pattern spec
       ;; TODO:: specify dst node if different
       (visitorptn DEEP
         ((forall (fun (n os)
                    `(forall ,(ast-get-variant-node-full-pattern astS n)
                             ,(ast-get-variant-node-full-pattern astD (tgtnode n os))
                             ,e)))
          (simple (fun (n os)
                    `(simple ,(ast-get-simple-node-pattern astS n)
                             ,(ast-get-simple-node-pattern astD (tgtnode n os))
                             ,e)))
          (vars   (fun (n os)
                    `(vars
                      ,(ast-get-variant-node-body-tmp astS n)
                      ,(ast-get-variant-node-body-tmp astD (tgtnode n os))
                      ,vs ,ds ,e)))))
       (visitorexpr DEEP
         ((visitor
           `(visitor
             ,tp
             ,from
             ,to
             ,opts

             ,src
             ,srctp

             (,srcast)
             (,dstast)

             ,@(visitor-fuse-implicit astS astD srctp vis)
             ,@vs))))
       )))


