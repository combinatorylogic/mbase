(nrec:def ast2stackrecord
          scnt ; 0
          thisnodesrc ; 1
          tpl ; 2
          cnt ; 3
          target ;4
          targetslot  ;5
          )

(nrec:def ast2cntrecord
          C ;; 'C or nil
          d 
          s
          p ;; nil for list records
          )

(include "./ast2-initvector.al")

(macro ast2:aget (a b)
  `(aget ,a ,b))

(macro ast2:ageto (descr a b)
  `(ageto ,a ,b))

(macro ast2:aseto (descr a b c)
  `(aseto ,a ,b ,c))

;; MBase-specific codegen
(function visitor-backend-mbase (lfsrc? lfdst? src)
  (visitorbackend:visit expr src
   (expr DEEP
    ((let
      `(let* ,ds ,b))
     (var id)
     (bind
      `(let* ,ds ,b)) ;; TODO!!!! annotate bindings
     (label
      `(begin
         (n.label ,id)
         ,e))
     (goto
      `(n.goto ,dst))
     (make_label
      ;; TODO: check it's a number, check it is correct
      dst
      )
     (switch
      (with-syms (tmp)
                 `(let ((,tmp ,c))
                    (inner.switch ,tmp
                                   ,@(foreach-map-count (e es i)
                                                        `(begin ,e))
                                   )) ;; TODO: accomodate default hand
                 ))

     (visitor_report_error `(ccerror (list 'VISITOR:ERROR (quote ,id) ,v)))
     (visitor_translate_tag `(ast2:aget ,t ,v))
     (visitor_get_tag `(ast2:get_tag ,lfsrc? ,v))
     (table `(ast2-init-n-vector ,@dsts))
     (push_stack_run
      (with-syms (tplid)
        (let* ((ndeep (foreach-mappend (d deep)
                        (p:match d
                          ((*MOVE* $m) nil)
                          (else (wrap d))))))
          `(let ((,tplid ,t))
             ;; todo: don't even try it for the list collectors
             ;(writeline (list 'CURRENT: (ast2:print-test (thisnodesrc))))
             ,@(if listnd () `((ast2:set_metadata ,tplid (ast-current-metadata))))
             (begin
               ,@(foreach-mappend (d deep)
                    (p:match d
                      ((*MOVE* $m) (wrap (m tplid)))
                      (else nil)))
               (begin (ast2:push_stack
                                  ,stk
                                  ,target ,targetslot
                                  ,n ,tplid ;; TODO: goto to cnt if ndeep is
                                  ,ndeep    ;; empty, without visiting runner
                                  ,cnt)
                      (n.goto ,runner)))))))
     (listnode_complete `(ast2:listnode_complete ,src)) ;; get
     ;; Move entries must be separated from the real continuation entries,
     ;; for optimisation primarily (we can move everything at once, no need
     ;; to jump anywhere).
     (move `(*MOVE* ,(fun (dstid)
                       `(begin
                          (ast2:set_target_simple ,dstid ,pos ,src)))))

     (setup_macros `(ast2:setup_macros ,ps (begin ,@e)))
     (with_added_metadata `(with-added-metadata ,ps (begin ,@c)))
     (debugmessage `(println ,c))
     (set `(n.stloc! ,id ,v))
     (get_tagged_tuple `(ast2:get_tagged_tuple ,e ,f))
     (get_tuple `(ast2:get_tuple ,e ,f))
     (get_metadata `(ast2:get_metadata_1 ,lfsrc? ,src))

     (allocate_tuple `(ast2:allocate_tuple ,fs))
     (allocate_tagged_tuple `(ast2:allocate_tagged_tuple ,tg ,tagid ,fs))
     (set_target `(ast2:set_target ,dst ,slot ,v))

     (allocate_list_collector
      `(ast2:allocate_list_collector))
     (code c)
     (nil 'nil)

     (make_continuation_record
      `(ast2:make_continuation_record ,dst ,pos ,src))
     (list_continuation_record
      `(ast2:list_continuation_record ,dst ,src))

     (make_popper ;; all done for this record, pop the stack and continue
      (with-syms (rst)
        `(let* ((,rst (cdr ,stackid))
                )
           (if ,rst ;; more to be done
               (begin
                 (n.stloc! ,stackid ,rst)
                 (n.goto   ,runner))
               (begin ;; done already
                 (n.goto   ,retlabel))))))

     ;; Runner machine:
     (make_runner ;; a new record is on the stack
      (with-syms (hd nx dst tnsrc)
       `(let* (
               (,hd (car ,stackid))
               (,nx (ast2:pop-cnt-rec ,hd)))
          (if ,nx
              (let* ((,dst (ast2:cntrecord-dst                    ,nx)))
                (if (ast2:cntrecord-listp ,nx)
                    (let* ((,tnsrc (ast2:cntrecord-src ,nx)))
                      (if ,tnsrc
                          (begin
                            (n.stloc! ,thisnodesrc
                                      (car (ast2:cntrecord-src   ,nx)))
                            (n.stloc! ,target
                                      (ast2:stackrecord-tpl      ,hd))
                            (n.stloc! ,targetslot
                                      (ast2:cntrecord-targetslot ,nx))

                            (if (cdr ,tnsrc)
                                (begin
                                  (ast2:add_stack_cnt ,hd
                                   (ast2:list_continuation_record
                                    ,dst (cdr ,tnsrc)))))
                            (n.stloc! ,nextnodevar ,dst)
                            (n.goto   ,nodeentry)
                            )
                          (begin
                            (n.goto ,runner)
                            )
                          ))
                    (begin
                      (n.stloc! ,thisnodesrc (ast2:cntrecord-src        ,nx))
                      (n.stloc! ,target      (ast2:stackrecord-tpl      ,hd))
                      (n.stloc! ,targetslot  (ast2:cntrecord-targetslot ,nx))
                      (n.stloc! ,nextnodevar ,dst)
                      (n.goto   ,nodeentry)
                      )))
              ;; no more continuations
              (begin
                (n.stloc! ,thisnodesrc (ast2:stackrecord-thisnodesrc ,hd))
                (n.stloc! ,thisnode    (ast2:stackrecord-tpl         ,hd))
                (n.stloc! ,nextnodevar (ast2:stackrecord-cnt ,hd))
                (n.stloc! ,target      (ast2:stackrecord-target      ,hd))
                (n.stloc! ,targetslot  (ast2:stackrecord-targetslot  ,hd))
                (n.goto   ,nodeentry)))
          )))

     (return v)

     ;; TODO: do something for listform mode
     (transform_listform (if lfsrc?
                             `(ast2:transform_listform ,v (ast-node-format) (ast-node-type))
                             `(begin )))
     (implicit_ctor      (if lfdst?
                             `(ast2:transform_to_listform (thisnode) ,p)
                             `(thisnode)))
     (implicit_ctor_tag  (if lfdst?
                             `(ast2:transform_to_listform_tag (thisnode) ,id ,tag ,p)
                             `(thisnode)))

     (dyn_tags_map (if lfsrc?
                       `(straise (ast2:make_dyn_tags_map (quote ,ids)))
                       'nil))

     (dummy `(ast2:mkemptyvector 2))
     (dummyslot 0)
     (getdummy `(ast2:aget ,d 1))

     ;; TODO:
     (else node)
     ))))


;;;;;; MBase-backend supporting macros and functions
(macro ast2:setup_macros (args . body)
  `(with-macros
    ,(foreach-map (a args)
       (format a (k v)
               `(,k (fun (_) (quote ,v)))))
    ,@body))

(macro ast2:push_stack (stk target targetslot n t deep c)
  (with-syms (rc)
   `(let* ((,rc (ast2:make_stack_record ,n ,t ,c ,target ,targetslot)))
      ,@(foreach-map (d (reverse deep))
          `(ast2:add_stack_cnt ,rc ,d))
      (n.stloc! ,stk (noconst (cons ,rc ,stk))) ;; really push
      )))

(function ast2:make_stack_record (n t c tg ts)
  (ast2stackrecord.new nil n t c tg ts))

(function ast2:add_stack_cnt (rc d)
  (let* ((d0 (ast2stackrecord.scnt rc)))
    (ast2stackrecord.scnt! rc (noconst (cons d d0)))))

(macro ast2:get_tagged_tuple (s n)
  `(ast2:ageto tagged_tuple ,s (+ 1 ,n)))

(macro ast2:get_tuple (s n)
  `(ast2:ageto tuple ,s (+ 1 ,n)))

(macro ast2:get_metadata (s)
  `(ast2:ageto metadata ,s 0))

(macro ast2:get_metadata_1 (lfsrc? s)
  (if lfsrc? 'nil
      `(ast2:ageto metadata_1 ,s 0)))

(macro ast2:set_metadata (dst v)
  `(ast2:aseto metadata ,dst 0 ,v))

(macro ast2:mkemptyvector (len)
  `(not.neth ()
      (a = (mkarr object ,len))
      (leave ((object)a))))

(macro ast2:allocate_tuple (fs)
  `(ast2:mkemptyvector ,(+ 2 (length fs))))

(macro ast2:allocate_tagged_tuple (tg tagid fs)
  `(ast2:set_tag (ast2:mkemptyvector ,(+ 2 (length fs)))
                 ,tagid))

(macro ast2:allocate_list_collector ()
  `(noconst
    (cons (quote (L))
         (collector (add get)
                    (cons add get)))))

(macro ast2:listnode_complete (cl)
  `(begin
     (return ,cl)))

(function ast2:set_tag (t i) (begin (ast2:aseto tag t 1 i) t))

(macro ast2:get_tag (lfsrc? v)
  (if lfsrc?
      `(inner-expand-first
        ast2:get_tag_listform (ast-source-tags-map) (quote ,v))
      `(ast2:ageto tag ,v 1)))

(macro ast2:get_tag_listform (vt v0)
  (let* ((v (cadr v0)))
    `(ast2:get_tag_listform_f ,vt ,v)))

(function ast2:get_tag_listform_f (vt v)
  (ohashget vt (car v)))

(function ast2:thisnodeval (d)
  (p:match d
    (  ((L) . $rest)
       (let* ((get (cdr (cdr d))))
         (get)))
    (  else d)))

(function ast2:set_target (d s v)
  ;; Differentiate between tagged and simple tuples dynamically
  (if d
      (let* ((vl (ast2:thisnodeval v)))
        (if (not (list? d))
            (ast2:aseto target d (+ 1 s) vl)
            ;; list collector
            (let* ((add (car (cdr d))))
              ;; ignore slot
              (add vl)))
        )))

(function ast2:set_target_simple (d s vl)
  (if d
      (if (not (list? d))
          (ast2:aseto simple d (+ 1 s) vl)
          ;; list collector
          (let* ((add (car (cdr d))))
            ;; ignore slot
            (add vl)))
      ))

(macro ast2:make_continuation_record (d p s)
  `(ast2cntrecord.new 'C ,d ,s ,p))

(macro ast2:list_continuation_record (d s)
  `(ast2cntrecord.new nil ,d ,s nil))

(function ast2:pop-cnt-rec (s)
  (let* ((rc (ast2stackrecord.scnt s)))
    (if rc
        (let* ((hd (car rc)))
          (ast2stackrecord.scnt! s (cdr rc))
          (return hd))
        nil)))

(macro ast2:stackrecord-thisnodesrc (s)
  `(ast2stackrecord.thisnodesrc ,s))

(macro ast2:stackrecord-target (s)
  `(ast2stackrecord.target ,s))

(macro ast2:stackrecord-targetslot (s)
  `(ast2stackrecord.targetslot ,s))

(macro ast2:cntrecord-listp (s)
  `(not (ast2cntrecord.C ,s)))

(macro ast2:stackrecord-tpl (s)
  `(ast2stackrecord.tpl ,s))

(macro ast2:cntrecord-dst (r)
  `(ast2cntrecord.d ,r))

(macro ast2:cntrecord-src (r)
  `(ast2cntrecord.s ,r))

(macro ast2:cntrecord-targetslot (r)
  `(ast2cntrecord.p ,r))

(macro ast2:stackrecord-cnt (s)
  `(ast2stackrecord.cnt ,s))

;; Expand the format macro in the current local context
(macro ast2:transform_listform (dst frmt tp)
  `(inner-expand-first
    ast2:transform_listform_inner_1 (quote ,dst) ,frmt ,tp))

;; If it's a variant, additionally expand the variant tag and source tags list
(macro ast2:transform_listform_inner_1 (dst frmt tp)
  (if (eqv? tp 'simple)
      `(ast2:transform_listform_inner ,dst ,frmt ,tp () ())
      `(inner-expand-first
        ast2:transform_listform_inner ,dst ,frmt ,tp (ast-variant) (ast-source-tags))))

(function ast2:compile_pattern_to_matcher (frmt)
  (astlang:visit pattern frmt
    (pattern DEEP
      ((tuple (let loop ((v vs))
                (p:match v
                  (((slice-append $hd) . $tl) hd)
                  (($hd (slice-append $tl))
                   (cons hd tl))
                  (($hd) (list hd))
                  (($hd . $tl) (cons hd (loop tl)))
                  (() nil))))
       (append `(slice-append ,p)) ;; TODO!
       (entry (Sm<< "$" name))
       (nil '$_)))
    ))

(function ast2:find-position (t vs)
  (let loop ((v vs)
             (i 0))
    (if v
        (if (eqv? (car v) t) i
            (loop (cdr v) (+ i 1))))))

;; Do the actual heavy-lifting, with format already expanded
(macro ast2:transform_listform_inner (dst0 frmt tp tag vars)
  ;; 1. Build a p:match format expression
  (let* ((pmfmt (ast2:compile_pattern_to_matcher frmt))
         (dst (cadr dst0))
         (fd (ast-pattern-entries frmt))
         (tagid (if tag (ast2:find-position tag vars) nil)))
    ;; 2. Construct a record and fill it
    (with-syms (tmp)
     `(let* ((,tmp
              ,(if (eqv? tp 'simple)
                   `(ast2:allocate_tuple ,fd)
                   `(ast2:allocate_tagged_tuple ,tag ,tagid ,fd))))
        (p:match ,(if (eqv? tp 'simple) dst `(cdr ,dst))
          (,pmfmt
           (begin
             ,@(foreach-map (f fd)
                 (format f (tp nm pos)
                   `(ast2:set_target ,tmp ,pos ,nm)
                   ))
             )))
        (n.stloc! ,dst ,tmp)
        ))))

(macro ast2:misc (v) v)

(macro ast2:make_listform (src frmt)
  `(inner-expand-first
    ast2:make_listform_inner (quote ,src) ,frmt (ast-dst-node-type)
    (ast-dst-tags)))

(macro ast2:make_listform_inner (src frmt tp tags)
  (if (eqv? tp 'variant)
      `(inner-expand-first
        ast2:make_listform_inner_1 ,src (quote ,frmt) ,tp (quote ,tags) (ast-variant))
      `(ast2:make_listform_inner_1 ,src (quote ,frmt) ,tp (quote ,tags) ())))


(function ast2:listform_compile_maker (frmt)
  (astlang:visit pattern frmt
     (pattern DEEP
      ((tuple (let loop ((v vs))
                (p:match v
                  (((slice-append $hd) . $tl) hd)
                  (($hd (slice-append $tl))
                   `(cons ,hd ,tl))
                  (($hd) `(list ,hd))
                  (($hd . $tl) `(cons ,hd ,(loop tl)))
                  (() 'nil))))
       (append `(slice-append ,p))
       (entry name)
       (nil 'nil)))))

(macro ast2:make_listform_inner_1 (src0 frmt0 tp tags0 variant)
  (let* ((frmt (cadr frmt0))
         (src (cadr src0))
         (tags (cadr tags0))
         (srcnm (gensym))
         (getter (if (eqv? tp 'simple) 'ast2:get_tuple
                     'ast2:get_tagged_tuple))
         (makerbody (ast2:listform_compile_maker frmt))
         (maker
          (if (eqv? tp 'simple) makerbody
              `(cons (quote ,variant)
                     ,makerbody)))
         (vars (ast-pattern-entries frmt))
         )
    `(let ((,srcnm ,src))
      ,(if vars
           `(let ,(foreach-map (v vars)
               (format v (nod nm idx)
                      `(,nm (,getter ,srcnm ,idx))))
              ,maker)
           maker))))

(macro ast2:transform_to_listform (src frmt)
 `(ast2:wrap_misc
   (inner-expand-first
    ast2:make_listform_inner_1 (quote ,src)
    (quote ,frmt)
    simple
    (x (ast-dst-tags))
    ())))

(macro ast2:transform_to_listform_tag (src id tag frmt)
  `(ast2:wrap_misc
    (inner-expand-first
     ast2:make_listform_inner_1 (quote ,src)
     (quote ,frmt)
     variant
     (x (ast-dst-tags))
     ,tag
     )))

(macro ast2:wrap_misc (v) v)


(function ast2:make_dyn_tags_map (ids)
  (let* ((ht (mkhash)))
    (foreach-count (i ids n)
                   (ohashput ht i n))
    (return ht)))


;; Make a node of a current destination AST format, with a current tag
(macro ast2:mknode args
  (let* ((xy (p:match args
               (((@md $x) . $rest) (cons x rest))
               (else (cons nil args)))))
    `(inner-expand-first
      ast2:mknode-inner (quote ,(cdr xy))
      (ast-node)
      (ast-node-type)
      (ast-node-format)
      (ast-variant)
      (ast-dst-tags)
      (ast-visitor-to)
      (quote ,(car xy))
      )))

(function ast2:ast-var-name (id) (Sm<< "ast2::" id "::src"))

(function ast2:default-ifun (id)
  (let* ((ret (shashget (getfuncenv)
                        (ast2:ast-var-name id))))
    (if ret ret
        (ccerror `(AST-NOT-FOUND ,id)))))

(function ast2:unnamed-only (args)
  (not
   (filter (fmt (nm v)
             (not (eqv? nm '*NEXT*))) args)))

(function ast2:fix-mknode-args (args fd)
  (if (ast2:unnamed-only args)
      (foreach-map (af (zip fd args))
        (format af ((tp nm pos) (_ v))
          `(,nm ,v) ;; TODO: check!
          ))
      args))

(function ast2:annotate-implicit-args (args nf)
  (if (not (ast2:unnamed-only args))
  (let* ((entries (ast-pattern-entries nf))
         (ht (mkhash)))
    (foreach (a args)
      (p:match a
        (($nm . $rest)
         (ohashput ht (_get_md_id nm) 1))))
    (let* ((iargs (foreach-mappend (e entries)
                    (format e (a n i)
                     (if (not (ohashget ht n)) `(,(S<< n)) nil)))))
      (if iargs
          (S<< "\n   with implicit arguments: " (strinterleave iargs ", ") "\n")
          ""))) ""))

(macro ast2:mknode-inner (qargs nd ndtype ndformat ndvar
                                ndtags astdst . qmdata)
  (let* ((args0 (cadr qargs))
         (ast (ast2:default-ifun astdst))
         (asth (ast-make-cache ast nil))
         (tpl  (gensym))
         (fd (ast-pattern-entries ndformat))
         (args (ast2:fix-mknode-args args0 fd))
         (tagid (if (eqv? ndtype 'variant)
                    (ast2:find-position ndvar ndtags)
                    nil))
         (mtdarg (find (fmt (nm v)
                           (eqv? nm '*metadata*)) args))

         (tplalloc (if (eqv? ndtype 'variant)
                       `(ast2:allocate_tagged_tuple ,ndvar ,tagid ,fd)
                       `(ast2:allocate_tuple ,fd))))
    (if (and qmdata (cadr (car qmdata)))
        (let* ((md (cadr (car qmdata)))
               (remark `((remark ,(S<< astdst ":" nd ":" (ast2-make-pattern-docstring ndvar ndformat)
                                       (ast2:annotate-implicit-args (cadr qargs) ndformat)
                                       )))))
          ((deref cc:process-remark-metadata) 'mknode (append md remark))))
    `(let ((,tpl ,tplalloc)
           ,@(foreach-mappend (a args)
               (format a (nm v)
                 (if (eqv? nm '*metadata*)
                     nil
                     (let* ((afmt (find (fmt (xtp xnm xpos) (eqv? nm xnm))
                                          fd))
                              (_ (if (not afmt)
                                     (ccerror `(NODE-CTOR-ARG-NOT-FOUND ,nm))))
                              (etp (car afmt)))
                       `((,nm (ast2:setup_macros ((ast-node ,etp)
                                                  (ast-node-is-top ())) ,v)))
                              ))))
               ;; TODO: sanitise!
           )
       ;; TODO: inherit metadata
       ,@(foreach-map (f fd)
           (format f (tp nm pos)
                   `(ast2:set_target ,tpl ,pos ,nm)))
       (ast2:set_metadata ,tpl
                          ,(if mtdarg
                               (cadr mtdarg)
                               '(ast-current-metadata)))
       (inner-expand-first ast2:mknode-return (ast-node-is-top) (quote ,tpl)))))

(macro ast2:mknode-return (cd tpl)
  (if cd (cadr tpl) (cadr tpl)))

(macro ast-current-metadata () 'nil)

(macro ast-with-metadata (m . e)
  (with-syms (nm)
    `(let ((,nm ,m))
       (with-macros ((ast-current-metadata (fun (_) (quote ,nm))))
          (begin ,@e)))))



(macro ast2-debugmsg-node (pfx)
  `(inner-expand-first ast2-debugmsg-inner ,pfx (ast-node)))

(macro ast2-debugmsg-inner rest
  `(list ,@(foreach-map (r rest) `(quote ,r))))

(macro ast-node-is-top () '())
