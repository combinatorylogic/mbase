;; The nearly source language which the user-facing macro spits out
(def:ast astlang ()
  (topdef (|
           (defast <astident:name> <*astsrc:srcs>
               <*tagident:taglist>
               <*astopt:options>
             . <*astnode:ns>)))
  (astsrc (| (incl <astident:name> . <*rewrite:rs>)))
  (astopt (| (multisrc) (frompeg)))
  (rewrite (| (rename <nodeident:f> <nodeident:t>)
              (delete <nodeident:t>)))
  (astnode (|
            (simple <nodedefident:id> <pattern:p>)
            (varnode <nodedefident:id> . <*variant:vs>)
            (extend <nodedefident:id> . <*extvariant:vs>)
            (extendvarhint <nodedefident:id> <tagident:tg> <*ident:hints>)
            (extendnodehint <nodedefident:id> <*ident:hints>)
            ))
  (variant (| (v <tagident:tag> <pattern:p> . <*pphint:hs>)))
  (extvariant (| (remove <tagident:tag>)
                 (add <variant:v>)))
  (pattern (| (tuple . <*pattern:vs>)
              (append <pattern:p>)
              (entry <nodeident:ref> <ident:name> <etype:tp> . <*pphint:hs>)
              (nil) ;; for empty variants
              ))
  (etype (| (single)
            (list)
            (optional)))
  (nodedefident <nodeident:i>)
  (nodeident <ident:i>)
  (tagident <ident:i>)
  )

;; The nearly-source language representing visitors
(def:ast visitorlang ( (astlang) )
  (visitorexpr (| (visitor <vtype:tp>
                           <astident:from>
                           <?astident:to>
                           <*visoption:opts>
                           <any:src>
                           <nodeident:srctp>

                           . <*visitorentry:vs>)))

  (visoption
   (| (listform_src)
      (listform_dst)
      (fullcopy)
      ))

  (visitorentry (| (deep <noderef:n> <*dstopt:os> <visitorptn:p>)
                   (once <noderef:n> <*dstopt:os> <visitorptn:p>)
                   ))
  (noderef (| (id <nodeident:id>)
              (as <nodeident:id> <nodeident:rn>)))
  (visitorptn (| (forall <any:e>)
                 (simple <any:e>)
                 (vars <*visitorvar:vs>
                       <*visitorvar:ds>
                       <visitorelse:e>
                       )))
  (visitorvar (| (v <tagident:id> <tagmetadata:md> <any:e>)))
  (visitorelse (| (none)
                  (velse <any:e>)
                  (vdelse <any:e>) ;; deep else
                  ))
  )

;; Next stage of processing: fusing the AST and the visitor together
(def:ast visitorlang1 ( visitorlang )
  (visitorexpr (|
                (visitor <vtype:tp>
                         <astident:from>
                         <?astident:to>
                         <*visoption:opts>

                         <any:src>
                         <nodeident:srctp>

                         <?topdef:srcdef>
                         <?topdef:dstdef>

                         . <*visitorentry:vs>)))
  (visitorptn (| (forall <*variant:vsrc> <*variant:vdst> <any:e>)
                 (simple <pattern:ps> <pattern:pd> <any:e>)
                 (list   <nodeident:id>) ;; virtual nodes created for the lists
                 (vars   <*variant:vsrc>
                         <*variant:vdst>
                         <*visitorvar:vs>
                         <*visitorvar:ds>
                         <visitorelse:e>
                         )
                 ))
  )

;; Lowering stage: simplification, specific visitor code actions
(def:ast visitorlang1x (visitorlang1 )
  (visitorentry
   (| (forall <*dstopt:os> <nodeident:id> <*visitorvar:vsrc> <*visitorvar:vdst> <any:e>)
      (simple <*dstopt:os> <norder:o> <nodeident:id> <pattern:ps> <pattern:pd> <visitorcode:e>)
      (list   <*dstopt:os> <nodeident:id> <nodeident:elt>)
      (vars   <*dstopt:os> <nodeident:id>
              <*visitorvar:vs>
              <visitorcode:e>)
      ))
  (visitorvar
   (| (v <tagident:id> <norder:o> <pattern:ps> <pattern:pd> <visitorcode:e> . <metadata:md>)))
  (variants <*visitorvar:vs>)
  (visitorcode
   (| (code <any:e>)
      (gotoelse <label:dst> <visitorcode:c>)
      (implicit_ctor . <any:v>)
      (implicit_ctor_tag . <any:v>)
      (update_thisnode <visitorcode:c> <visitorcode:e>)
      (delayed_update <any:id> <any:tag> <visitorcode:e>)
      (nil)
      (vdelse <any:e>) ;; relic
      ))
  )

;; Next lowering step: building a flat FSM with explicitly managed stack
(def:ast visitorlang2 ( visitorlang1x )
  (visitorexpr (| (visitor <*tagident:srctags>
                           <*tagident:dsttags>

                           <any:src>
                           <nodeident:srctp>
                           <ident:srcast>
                           <ident:dstast>

                           <*visoption:opts>

                           <visitorcode:body>)))

  (switchentry (<labelident:id> <visitorcode:e>))
  (bindpair (<ident:id> <visitorcode:e>))
  (visitorcode (|
                  (node_switch <*nodeident:ids> . <*switchentry:es>)
                  (variant_switch <varswitchids:ids> . <*switchentry:es>)
                  (push_stack_run
                                  <bool:listnd>
                                  <visitorcode:nod>
                                  <visitorcode:tpl>
                                  <*visitorcode:deep>
                                  <visitorcode:cnt>)
                  (listnode_complete)
                  (make_label     <labelident:dst>)
                  (pop_stack_with <visitorcode:ret>)
                  (goto_with <labelident:dst> <visitorcode:ret>) ;; set thisnode to ret and jump
                  (make_move_record         <ident:nodeid>
                                            <number:pos>
                                            <visitorcode:src>)

                  (make_continuation_record <ident:nodeid>
                                            <number:pos>
                                            <visitorcode:src>)
                  (list_continuation_record <ident:nodeid>
                                            <visitorcode:src>)
                  (ast_make_tuple <pattern:p>) ;; allocate a tuple
                  (ast_make_tagged_tuple <tagident:tg> <pattern:p>) ;; allocate a tuple
                  (label <labelident:id> <visitorcode:e>)


                  (thisnode)
                  (thisnodesrc)
                  (thismetadata)

                  (nil)
                  (vdelse <visitorcode:e>)

                  (make_list_collector)

                  (get_tagged_tuple <visitorcode:e> <number:id>)
                  (get_tuple <visitorcode:e> <number:id>)
                  (bind <*bindpair:ps> <visitorcode:e>)

                  (code <any:c>) ;; arbitrary code
                  (setup_macros <any:c> . <*visitorcode:e>)
                  (debugmessage <any:c>)

                  (implicit_ctor . <any:v>)
                  (implicit_ctor_tag . <any:v>)
                  (update_thisnode <visitorcode:c> <visitorcode:e>)
                  (transform_listform <visitorcode:src>)
                  (with_added_metadata <*metadatapair:ps> <visitorcode:c>)
                  (begin . <*visitorcode:es>)
                  ))
  )

;; Lowering stage: refined meaning of varswitchids
(def:ast visitorlang2x ( visitorlang2 )
  (varswitchids (<*tagident:v> <*tagident:g>)))

;; Visitor backend language, already somewhat target-specific
(def:ast visitorbackend ()
  (letpair (<varident:id> <expr:v>))
  (fieldref (<nodeident:tp> <fieldident:nm> <number:pos>))
  (macropair (<ident:m> <any:v>))
  (expr
   (|
    (var   <varident:id>)
    (set   <varident:id> <expr:v>)
    (let   <*letpair:ds> <expr:b>) ; a simple let*
    (bind  <*letpair:ds> <expr:b>) ; tenative binding, removed if unused

    (label <labelident:id> <expr:e>)
    (switch <expr:c> <*expr:es> <expr:df>)
    (make_label <switchdst:dst>)
    (transform_listform <expr:v>)
    (dyn_tags_map <*tagident:ids>)

    (visitor_report_error <errid:id> <expr:v>)

    (visitor_get_tag <expr:v>)
    (visitor_translate_tag <expr:v>
                           <expr:t>)
    (table <*switchdst:dsts>)

    (move                     <number:pos>
                              <expr:src>)

    (make_continuation_record <switchdst:dst>
                              <number:pos>
                              <expr:src>)

    (list_continuation_record <switchdst:dst>
                              <expr:src>)

    (listnode_complete <expr:src>)

    (allocate_tuple <*fieldref:fs>)
    (allocate_tagged_tuple <ident:tg> <number:tagid> <*fieldref:fs>)
    (get_metadata   <expr:src>)
    (get_tagged_tuple      <expr:e> <number:f>)
    (get_tuple             <expr:e> <number:f>)

    (push_stack_run <bool:listnd>
                    <expr:stk> <expr:target> <expr:targetslot>
                    <expr:n> <expr:t> <*expr:deep> <expr:cnt> <label:runner>)
    (set_target     <expr:dst> <expr:slot> <expr:v>)
    (goto           <labelident:dst>)

    (code <any:c>) ; fallthrough
    (nil)
    (begin . <*expr:es>)
    (return <expr:v>)

    (allocate_list_collector)


    (make_runner <ident:stackid> <ident:thisnodesrc>
                 <ident:thisnode> <ident:target> <ident:targetslot>
                 <ident:nodeentry> <ident:nextnodevar> <ident:runner>)

    (make_popper <label:runner> <label:retlabel> <ident:stackid>)

    (setup_macros <*macropair:ps> . <*expr:e>)
    (with_added_metadata <*mdpair:ps> . <*expr:c>)
    (debugmessage <any:c>)

    ;; maybe
    (implicit_ctor <pattern:p>)
    (implicit_ctor_tag <number:id> <tagident:tag> <pattern:p>)

    ;; return value
    (dummy) (dummyslot) (getdummy <expr:d>)

    )))