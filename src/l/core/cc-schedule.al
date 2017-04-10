;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{Local variables scheduling pass}
;-

;; TODO: support for TryBegin-TryEnd thingy
(function cc:mkgen (lst)
  (if lst `((gen ,@lst)) nil))

(function cc:liveness ( block )
   (foreach-mappend (b block)
     (cc:flatast:visit code b
    (code DEEP
      ((Local nil)
       (FixLocal `((fixvar ,name)))
       (Label `((label ,lbl)))
       (Iflabel `((label ,lbl)))
       (Goto `((goto ,lbl)))
       (Setlocal
        `((genkill ((V ,id obj)) ,value)))
             (Patchclosure `((genkill () ((V ,id obj)))))
       (Push
        (cc:mkgen value))
       (Setglobal
        (cc:mkgen value))
       (Setnewglobal
        (cc:mkgen value))
       (Pushapp
        (cc:mkgen (append fn (foreach-mappend (a args) a))))
;= [[asm]] may destroy a target and may use some locals.
             (Asm
              (let ((kil (p:match tgt
                            ((local $nm)
                             `((V ,nm obj)))
                            (else nil)))
                    (us (filter (M@ not null?) (map car
             (filter
              (M@ not null?)
              (map cadr use))))))
                (if (or kil us)
        `((genkill ,kil ,us))
        nil)))
;= [[genkill]] used for some instrs here is a simultaneous gen and kill operation.
;= For example, any destructive assignment is [[genkill]] or [[kill]] operation.
       (Setlocapp
        `((genkill
           ((V ,nm obj))
           ,(append fn (foreach-mappend (a args) a)))))
       (Switch
        `(
          ,@(cc:mkgen val)
          ,@(foreach-map (l labels)
              `(gotocond () ,l))))
       (Retapp
        (cc:mkgen
         (append fn (foreach-mappend (a args) a))))
       (Dropapp
        (cc:mkgen
         (append fn (foreach-mappend (a args) a))))
       (Return
        (cc:mkgen value))
       (Gotoif
        `((gotocond ,cnd ,lbl)))
       (Gotoifnot
        `((gotocond ,cnd ,lbl)))
       (GotoNull
        `((gotocond () ,lbl)))
       (GotoPairP
        `((gotocond () ,lbl)))
       (GotoEqv
        `((gotocond () ,lbl)))
       (else nil)))
;= [[expr]] nodes stand for variable or stack references here, so they must
;= be transformed to typed ``register'' references. We are using only ``obj'' type for
;= all our variables since Arc is too dynamic for distinguishing one type from another.
    (expr DEEP
      ((Var `((V ,name obj)))
       (else nil)))
    )))

(function cc:reschedule ( block )
  (let* ((lness (cc:liveness block))
         (all (r3:allocateregisters nil (r3:lgraphs lness)))
         (newnm (fun (n)
                  (hashget all n)))
         (alrdy (mkhash))
         )
    (collector (locadd locget)
     (alet reslt
       (foreach-mappend (b block)
         (cc:flatast:visit code b
;= All variables are renamed, no exceptions
           (expr _
             ((Var `(Var ,(newnm name)))
              (else node)))
;= There are several local variable referencing operations:
           (code DEEP
             ((Local
               (let* ((n (newnm name)))
                 (if (hashget alrdy n) `((Nop))
                     (begin
                       (hashput alrdy n #t)
                       (locadd `(Local ,n))
                       nil
                       ))))
              (FixLocal (begin (locadd node) nil))
              (Setlocal
               (wrap (ast:mknode (id (newnm id)))))
              (Asm
               (wrap
                (p:match tgt
                  ((local $nm)
                   (ast:mknode (tgt `(local ,(newnm nm)))))
                  (else node))))
              (Patchclosure
               (wrap
                (ast:mknode
                 (id (newnm id))
                 (args (map-over args
                                 (fmt (i v)
                                      `(,i ,(newnm v))))))))
              (Setlocapp
               (wrap (ast:mknode (nm (newnm nm)))))
              (else (wrap node))))))
       (return `(,@(locget) ,@reslt))
       ))))

(function cc:lifted-reschedule (ss)
  (cc:flatast:visit lifted ss
     (block _
            (cc:reschedule node))))