;; visitorlang1 -> visitorlang1 with list nodes
(function visitor-inject-listnodes (src) ;; TODO: opt support
;                                        ;; TODO: only inject for managed nodes
  "Inject additional nodes for lists"
  (let* ((lsts (mkhash))
         (mh (mkhash))
         (nodeadd (fun (id) (ohashput mh id id)))
         (managed? (fun (id) (ohashget mh id)))
         (addlst (fun (v)
                   (ohashput lsts v v))))
    (visitorlang1:visit visitorexpr src
           (visitorentry DEEP
             ((deep   (nodeadd n))
              (once   (nodeadd n))))
           (noderef DEEP ((id id) (as id))))
    (visitorlang1:iter visitorexpr src
      (topdef _ (forall node))
      (pattern DEEP
       ((entry (if (and (eqv? tp 'mul)
                        (managed? ref))
                   (addlst ref)))
        (else node)
        )))
    (let* ((listnodes (hashmap (fun (k v)
                                 `(deep (id ,(Sm<< k "--list")) () (list ,k)))
                               lsts)))
      (visitorlang1:visit visitorexpr src
        (pattern DEEP
          ((entry (if (eqv? tp 'mul)
                      (ast:mknode (tp 'ref)
                                  (ref (Sm<< ref "--list")))
                      node))
           (else node)))
        (visitorexpr DEEP
          ((visitor
            (ast:mknode
             (vs (append vs listnodes))))))))))

;; visitorlang1 -> visitorlang1x
(function visitor-backend-prepare (src)
  (let* (
        (to-code-env
         (fun (env x)
           (p:match env
             ((simple $p)
              `(update_thisnode (implicit_ctor ,p) ,x))
             ((var $nid $tag $p)
              `(update_thisnode (implicit_ctor_tag ,nid ,tag ,p) ,x))
             ((ivar $nid $tag)
              `(delayed_update ,nid ,tag ,x))
             (else x))))
        (to-code
         (fun (env x)
           (p:match x
             ((implicit_ctor . $rest) x)
             ((implicit_ctor_tag . $rest) x)
             (else (to-code-env env `(code ,x))))))
         ;; A helper function: make a hash of variant ids
        (hash-variants
         (fun (vs)
           (let* ((ht (mkhash)))
             (visitorlang1x:visit variants vs
               (visitorvar _ ((v (ohashput ht id node)))))
             (return ht))))
        (update-delayed
         (fun (e p)
           (visitorlang1x:visit visitorcode e
             (visitorcode DEEP
               ((delayed_update
                 `(update_thisnode
                   (implicit_ctor_tag ,id ,tag ,p)
                   ,e))
                (else node))))))

        ;; Fuse right formats into left tags
        (fuse-src-dst
         (fun (vs vd)
           (let* ((ht (mkhash)))
             (foreach (v vd)
               (format v (_ tg o pl pr x . _)
                       (ohashput ht tg pr)))
             (foreach-map (v vs)
               (format v (_ tg o pl pr x . md)
                 (let ((chk (ohashget ht tg)))
                   `(v ,tg ,o ,pl ,(if chk chk pl) ,x ,@md)))))))
        ;; Fuse user-provided variants into implicit variants,
        ;;   add else handling if necessary
        (fuse-variants
         (fun (vs1 vs2 ds e)
           (let* ((isdeep?
                   (or ds
                       (p:match e
                         ((vdelse . $x) #t) (else nil))))
                  (ex (p:match e
                        ((nil) nil)
                        ((vdelse $x) x)
                        (else e)))
                  (ht (hash-variants vs2)))
             (foreach-map (v vs1)
               (visitorlang1x:visit visitorvar v
                  (visitorvar _
                    ((v
                      (let* ((v1 (ohashget ht id))
                             (optns ps)
                             (optnd pd)
                             )
                        (if v1
                            ;; Fuse a pattern into a user-provided processor
                            (visitorlang1x:visit visitorvar v1
                               (visitorvar _
                                 ((v
                                   (ast:mknode (ps optns)
                                               (pd optnd)
                                               (e (update-delayed e optnd))
                                               )))))
                            ;; Leave an implicit processor, adjusting the order
                            ;;    and else handling
                            (ast:mknode (o (if isdeep? 'deep o))
                                        (e (if ex `(gotoelse dummy ,e)
                                               e)))
                            ))))))
               )))))
   (visitorlang1:visit visitorexpr src
     (visitorentry DEEP
        ((deep (p 'deep n os))
         (once (p 'once n os))))
     (visitorptn DEEP
        ((forall (fun (o n os)
                   `(forall ,os ,n
                            ,(if (eq? 'once o) nil
                                 (foreach-map (v vsrc)
                                   (v 'deep o)))
                            ,(if (eq? 'once o) nil
                                 (foreach-map (v vdst)
                                   (v 'deep o)))
                            ,(to-code nil e))))
         (simple (fun (o n os) `(simple ,os ,o ,n ,ps ,pd ,(to-code `(simple ,pd) e))))
         (list   (fun (o n os) `(list ,os  ,n ,id)))
         (vars   (fun (o n os)
                   (let* ((v0s (foreach-map (v vsrc) (v n o)))
                          (v0d (foreach-map (v vdst) (v n o)))
                          (v0 (fuse-src-dst v0s v0d))
                          (v1 (foreach-map (v vs) (v n o)))
                          (v2 (foreach-map (v ds) (v n 'deep))))
                     `(vars ,os  ,n ,(fuse-variants v0 (append v1 v2) ds e)
                              ,e))))))
     (topdef _ (forall node)) ;; do not go into ast sources
     (variant DEEP
        ((v (fun (nid o) `(v ,tag ,o ,p ,p (implicit_ctor_tag ,nid ,tag ,p))))))
     (visitorvar DEEP
        ((v (fun (nid o) `(v ,id ,o (nil) (nil) ,(to-code `(ivar ,nid ,id) e) ,@md)))))
     (noderef DEEP ((id id) (as id)))
     (visitorelse DEEP
        ((velse (to-code nil e))
         (vdelse `(vdelse ,(to-code nil e)))
         (none  `(nil))))
     ;; TODO: check if variants are exhaustive, etc.
     )))

;; visitorlang1x -> visitorlang1x
(function visitor-refine-else (src)
  (let* ((refine
          (fun (src newdst)
            (visitorlang1x:visit visitorentry src
              (visitorcode DEEP
                ((gotoelse (ast:mknode (dst newdst)))
                 (else node)))))))
    (visitorlang1x:visit visitorexpr src
      (visitorentry _
        ((vars (refine node id))
         (else node))))
    ))



;; Generate a sorted list of variant tags for a given AST
(function ast-get-variant-tags (astsrc)
  (astlang:visit topdef astsrc
    (topdef _ ((defast taglist)))))

;; Map a list of variants to a list of numeric tags;
;; Returns (<hashmap> <array>)
(function ast-map-variant-tags (tags)
  (let* ((ht (mkhash))
         (len (length tags))
         (rt (mkvector tags)))
    (foreach-count (t tags i) (ohashput ht t i))
    (return (list ht rt))))

(function ast2-debug-p ()
  (if (shashget (getfuncenv) 'debug-compiler-ast2) #t nil))

(function ast-pattern-entries (p)
  (collector (add get)
  (visitorlang1x:iter pattern p
    (pattern DEEP
      ((entry (add `(,ref ,name)))
       (else nil))))
  (let* ((es (get)))
    (foreach-map-count (e es i)
      (p:match e
        (($nod $nm) `(,nod ,nm ,(+ i 1))))))))

(function ast2-make-pattern-docstring (tag p)
  (S<< tag "(" (strinterleave (foreach-map (e (ast-pattern-entries p)) (S<< (cadr e))) ",") ")"))

;; visitorlang1x -> visitorlang2
(function visitor-backend-lowering (src)
 (collector (add get)
 (let* (
        ;; Insert a list node handler
        (compile-listnode
         (fun (id ref)
           (let* ((tg (gensym))
                  (next
                   `(,tg (pop_stack_with (listnode_complete)))))
             (add next)
             `(push_stack_run
               1
               (thisnodesrc)
               (make_list_collector)
               ((list_continuation_record ,ref
                                          (thisnodesrc)))
               (make_label ,tg))
             )))

        ;; Manage a set of nodes served by this visitor
        (nodes-map (mkhash))
        (nodeadd (fun (id) (ohashput nodes-map id id)))
        (node-served? (fun (id) (ohashget nodes-map id)))
        (phase0
         (visitorlang1x:iter visitorexpr src
           (visitorentry DEEP
             ((forall (nodeadd id))
              (simple (nodeadd id))
              (list   (nodeadd id))
              (vars   (nodeadd id))))
           ))
        ;; Compile simple leaf node
        (compile-leaf
         (fun (o p e nid)
           (let* ((macros
                     `((ast-node ,nid)
                       (ast-node-is-top #t)
                       (ast-node-type 'simple)
                       (ast-node-format ,p))))
             `(setup_macros ,macros
                 (set_target_simple ,e)))))
        ;; Compile a variant or simple entry
        (compile-pattern
         (fun (o p e tag nid md)
           (let* ((es (ast-pattern-entries p))
                  (ndref (if (eqv? o 'deep)
                             '(thisnode)
                             '(thisnodesrc)))
                  (addmetadata (if md
                                   (alet md1 (append md
                                                     `((docstring ,(ast2-make-pattern-docstring tag p))))
                                     (fun (x)
                                       `(with_added_metadata
                                         ,(foreach-map (en es)
                                            `(,(cadr en) ,@md1))
                                         ,x)))
                                   (fun (x) x)))
                  (ecode
                   `(bind
                     ,(foreach-map (x es)
                        (p:match x
                          (($nod $nm $num)
                           (if tag
                               `(,nm (get_tagged_tuple ,ndref ,num))
                               `(,nm (get_tuple ,ndref ,num))
                               ))))
                     ,(p:match e
                        ((goto_with . $rest) (addmetadata e))
                        (else
                         `(pop_stack_with ,(addmetadata e)))))))
             (let* ((tg (gensym))
                    (macros
                     `((ast-node ,nid)
                       (ast-node-is-top #t)
                       (ast-node-type ,(if tag 'variant 'simple))
                       (ast-node-format ,p)
                       ,@(if tag `((ast-variant ,tag))))))
               (add `(,tg (setup_macros ,macros
                             ,@(if (ast2-debug-p)
                                   `((debugmessage (ast2-debugmsg-node ENTERING-NODE-BUILDER))))
                             ,ecode)))
               ;; Will push an invocation record on stack
               `(setup_macros ,macros
                  (begin
                    (transform_listform (thisnodesrc))
                    ,@(if (ast2-debug-p)
                          `((debugmessage (ast2-debugmsg-node ENTERING-NODE))
                            ,@(if tag `((debugmessage (list 'TAG: (quote ,tag)))))))
                    (push_stack_run
                     ()
                     ;; Saving the current node source
                     (thisnodesrc)
                     ;; Create a new node tuple to be filled by the
                     ;;  referenced matchers, if needed
                     ,(if tag `(ast_make_tagged_tuple ,tag ,p)
                          `(ast_make_tuple ,p))
                     ;; Fill a list of continuation records, only
                     ;;  for the nodes served by this visitor
                     ,(foreach-map (e es)
                        (p:match e
                          (($nod $nm $pos)
                           (if (and (node-served? nod)
                                    (eqv? o 'deep))
                               `(make_continuation_record
                                 ,nod ,pos
                                 ,(if tag
                                      `(get_tagged_tuple (thisnodesrc) ,pos)
                                      `(get_tuple (thisnodesrc) ,pos)
                                      ))
                               `(make_move_record
                                 ,nod ,pos
                                 ,(if tag
                                      `(get_tagged_tuple (thisnodesrc) ,pos)
                                      `(get_tuple (thisnodesrc) ,pos))
                                 )
                               ))))
                     ;; A continuation label - when all continuation records
                     ;;  are evaluated, carry on to this label to finish the
                     ;;  current node processing.
                     (make_label ,tg)))))
             )))

        ;; First translation stage: lift the variant else entries,
        ;; compile patterns, lift the continuation nodes.
        (phase1
         (visitorlang1x:visit visitorexpr src
           (visitorexpr DEEP
             ((visitor
               `(visitor   ,(if srcdef (ast-get-variant-tags (car srcdef)))
                           ,(if dstdef (ast-get-variant-tags (car dstdef)))

                           ,src
                           ,srctp

                           ,from
                           ,(if to (car to) from)

                           ,opts

                           (node_switch () ,@vs)))))
           (visitorentry DEEP
             ((forall
               (add `(,(Sm<< id "--else")
                      (label ,(Sm<< id "--else")
                        (setup_macros ((ast-node ,id)
                                       (ast-node-is-top #t)
                                       (ast-node-type forall))
                           ,@(if (ast2-debug-p)
                                `((debugmessage (ast2-debugmsg-node ENTERING-NODE-BUILDER))))

                           (pop_stack_with ,e)))))
               `(,id (variant_switch () ,@(foreach-map (v vsrc) (v id)))))
              (simple
               (p:match ps
                 ((entry . $_) `(,id ,(compile-leaf o ps e id))) ;; It's a leaf node, no structure
                 (else `(,id ,(compile-pattern o ps e nil id nil)))))
              (list
               `(,id ,(compile-listnode id elt)))
              (vars
               (p:match e
                 ((nil) nil)
                 (else
                  (add `(,(Sm<< id "--else")
                        (setup_macros ((ast-node ,id)
                                       (ast-node-is-top #t)
                                       (ast-node-type else))
                          ,@(if (ast2-debug-p)
                                `((debugmessage (ast2-debugmsg-node ENTERING-NODE-VARS-BUILDER))))
                          (pop_stack_with ,e))))))
               `(,id (variant_switch () ,@(foreach-map (v vs) (v id)))))))
           (visitorvar DEEP
             ((v (fun (nid) `(,id ,(compile-pattern o ps e id nid md))))))
           (visitorcode DEEP
             ((code node)
              (gotoelse `(goto_with ,(Sm<< dst "--else") ,c))
              (else node) ;; TODO!!!!
              ))
           )))
   ;; Second translation step: add all the newly created nodes
   (visitorlang2:visit visitorexpr phase1
     (visitorcode _
       ((node_switch
         (ast:mknode (es (append es (get)))))
        (else node)
        )))
   )))


(function visitor-get-taglist (src)
  (visitorlang2:visit visitorexpr src
    (visitorexpr _
      ((visitor srctags)))))


;; visitorlang2 -> visitorlang2x: lift the switch entries for convenience,
;;   store alongside with a full list of tags. This will be required for
;;   generating node-specific tag id translation maps.
(function visitor-populate-varids (src)
 (let* ((glb (visitor-get-taglist src)))
  (visitorlang2:visit visitorexpr src
    (visitorcode DEEP
     ((node_switch
       (ast:mknode (ids
                    (map car es))))
      (variant_switch
       (ast:mknode (ids `(,(map car es) ,glb))))
      (else node)
      )))))

;; l - variant-local list
;; g - global ast tags list
(function visitor-map-tags-table (l g)
  (let* ((lh (mkhash))
         (n  (length l)))
    (foreach-count (l l i) (ohashput lh l i))
    (foreach-map (g g)
      (let* ((id0 (ohashget lh g))
             (id  (if id0 id0 n)))
        id))))

;; We assume that there is only one nodeswitch
(function visitor-get-nodemap (src)
  (let* ((ht (mkhash)))
  (visitorlang2x:visit visitorexpr src
   (visitorcode DEEP
    ((node_switch (foreach-count (e es i)
                    (ohashput ht e i)))
     (else node)))
   (switchentry DEEP id))
  (return (fun (id) (let ((chk (ohashget ht id)))
                      (if chk chk `(anyref ,id)))))))


(function visitor-get-options (src)
  (let* ((ol (visitorlang2x:visit visitorexpr src
               (visitorexpr _
                 ((visitor opts)))))
         (ht (mkhash)))
    (foreach (o ol)
      (ohashput ht (car o) #t))
    (return ht)
    ))

(function visitor-get-dsttagmap (src)
  (let* ((ht (mkhash)))
    (visitorlang2x:iter visitorexpr src
     (visitorexpr _
       ((visitor (let* ((tg (if dsttags dsttags srctags)))
                   (foreach-count (t tg i)
                      (ohashput ht t i)))))))
    (fun (id) (ohashget ht id))))

;; This pass lowers node_switch and variant_switch into generic switches,
;; replaces node labels with numbers, inserts some error handling, and defines
;; visitor machine variables (stack, next node id, thisnode, thisnodesrc, etc.)
(function visitor-lower-further (lfsrc? src)
 (let* ((xmap (visitor-get-nodemap src))
        (xtmap (visitor-get-dsttagmap src)))
 (with-syms (nodeentry nextnodevar thisnodesrc thisnode stackid
             runner target targetslot popper retlabel srctagsmap
            )
 (visitorlang2x:visit visitorexpr src
   (visitorexpr DEEP
     ((visitor
     `(setup_macros ((ast-source-tags ,srctags)
                     (ast-source-tags-map ,srctagsmap)
                     (thisnode        ,thisnode)
                     (thisnodesrc     ,thisnodesrc)
                     (ast-dst-tags    ,(if dsttags dsttags srctags))
                     (ast-visitor-options ,opts)
                     (ast-visitor-from ,srcast)
                     (ast-visitor-to ,dstast)
                     (ast-visitor-sourcetp ,srctp)
                     (node (ast2:thisnodeval (thisnode)))
                     (alt-mknode ast2)
                     ;; TODO: remove the backend details from here
                     (ast-current-metadata (ast2:get_metadata_1 ,lfsrc? (thisnodesrc)))
                     )

       (let ((,nextnodevar   (make_label ,(xmap srctp)))
             (,target        (dummy))
             (,targetslot    (dummyslot))
             (,thisnodesrc   (code ,src))
             (,thisnode      (nil))
             (,stackid       (nil))
             (,srctagsmap    (dyn_tags_map ,srctags))
             )
         (begin
           ,body
           (label ,runner
                  (make_runner ,stackid ,thisnodesrc ,thisnode ,target ,targetslot
                               ,nodeentry ,nextnodevar ,runner))
           (label ,popper
                  (make_popper ,runner ,retlabel ,stackid))
           (label ,retlabel
                  (return (getdummy (var ,target))))
           ))))))
   (visitorcode DEEP
    ((node_switch
      (with-syms (ndtop)
       `(label ,nodeentry
          (switch (var ,nextnodevar)
                  ,es
                  (visitor_report_error NODEID (var ,nextnodevar))))))
     (variant_switch
      (with-syms (tag stag)
      `(let ((,tag  (visitor_get_tag (var ,thisnodesrc)))
             (,stag (visitor_translate_tag
                     (var ,tag)
                     (table ,(visitor-map-tags-table (ids.> v) (ids.> g))))))
         (switch (var ,stag)
                 (,@es
                  (visitor_report_error VARIANTID (var ,stag)))
                 (visitor_report_error VARIANTID (var ,stag)))
         )))
     (goto_with `(begin
                   (set ,thisnode ,ret)
                   (set ,nextnodevar (make_label ,(xmap dst)))
                   (goto ,nodeentry)))
     (make_label `(make_label ,(xmap dst)))
     (make_continuation_record
      `(make_continuation_record ,(xmap nodeid)
                                 ,pos
                                 ,src))
     (make_move_record
      `(move                     ,pos ,src))
     (list_continuation_record
      `(list_continuation_record ,(xmap nodeid)
                                 ,src))
     (ast_make_tuple          `(allocate_tuple ,(ast-pattern-entries p)))
     (ast_make_tagged_tuple   `(allocate_tagged_tuple ,tg ,(xtmap tg) ,(ast-pattern-entries p)))
     (thisnode         `(var ,thisnode))
     (thisnodesrc      `(var ,thisnodesrc))
     (thismetadata     `(get_metadata (var ,thisnodesrc)))
     (get_tagged_tuple `(get_tagged_tuple ,e ,id))
     (get_tuple        `(get_tuple ,e ,id))

     (push_stack_run   `(push_stack_run ,listnd (var ,stackid) (var ,target)
                                        (var ,targetslot)
                                        ,nod ,tpl ,deep ,cnt ,runner))
     (nil              `(nil))
     (vdelse            e)
     (make_list_collector `(allocate_list_collector))
     (listnode_complete   `(listnode_complete (var ,thisnode)))

     (set_target_simple `(set_target_simple (var ,target) (var ,targetslot) ,ret))
     (pop_stack_with   `(begin
                          ,@(if (ast2-debug-p)
                                `((debugmessage (list 'POP-STACK-WITH))))
                          (set_target (var ,target) (var ,targetslot) ,ret)
                          (goto  ,popper)
                          ))

     (update_thisnode `(begin
                         (set ,thisnode ,c)
                         ,e))

     (else node) ;; TODO!
     ;; ...
     ))
   (switchentry DEEP e)
   ))))







