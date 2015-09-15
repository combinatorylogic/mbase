;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; All the macros here are implementing peg atomic primitives which can
;   (and should) be possible to implement on top of the other execution engines
;   (i.e., not only MBase).
;
; See backend-ast.al for a target language definition.

(macro peg-ignore (terms code)
  code)

(macro peg-node (code)
  `(let* ((saved (__peg:get-position_ source)) ;; save the backtracking point
          (result ,code))
     (if (peg-success? result) result
         (begin
           (__peg:set-position_ source saved)
           result))))

(macro peg-if (quest tru fals)
  (with-syms (tmp)
    `(let ((,tmp ,quest))
       (if (peg-success? ,tmp)
           (peg-collect ,tmp
                        ,tru)
           ,fals))))

(macro peg-backend-hint (nm args)
  `(if Env
       (let* ((last (__peg:get-position_ source)))
         (peg:env-signal Env 'HINT last last (quote (,nm ,args))))))

(macro peg-check (e next)
  (with-syms (tmp)
     `(let ((,tmp ,e))
        (if (peg-success? ,tmp) ,tmp
            ,next))))

(not.class PegException (extends System.Exception)
     (field object loc (public))
     (field object env (public))
     (field object payload (public))
     )

(not.function peg-fatal-exception ((object Env) (object source) (object msg))
  (e = (new PegException))
  (e#env <- Env)
  (e#loc <- source)
  (e#payload <- msg)
  (throw e))

(macro peg-ordie (e m . args)
  (with-syms (tmp)
    `(let ((,tmp ,e))
       (if (peg-success? ,tmp) ,tmp
           (peg-fatal-exception
            Env
            source
            (,(Sm<< "peg-error-" m) Env saved source
             (list ,@(foreach-map (a args)
                       `(peg-constr-compile ,a ())))))))))

(macro peg-call-checker (id)
  (with-syms (tmp)
     `(let ((,tmp (,(Sm<< "peg-checkfunction-" id)
                   (__peg:get-delta saved
                                    (__peg:get-position_ source)))))
        (if (peg-success? ,tmp) (peg-dummy) (peg-fail)))))

(macro peg-call-action (nm args)
  `(begin ;TODO
     ))

(macro peg-call-highorder (args maker)
  (with-syms (pars)
     `(let ((,pars (,(Sm<< "peg-makerfunction-" maker)
                   ,@args)))
        (,pars Env _lrstk source))))

(macro peg-check-collect (e next)
  (with-syms (sav tmp rst l1)
     `(let ((,sav (__peg:get-position_ source))
            (,tmp ,e)
            (,l1 (__peg:get-position_ source))
            (,rst (begin
                    (__peg:set-position_ source ,sav)
                    ,next)))
        (if (peg-success? ,tmp)
            (if (peg-success? ,rst)
                (peg-collect-multi ,tmp ,l1 ,rst (__peg:get-position_ source))
                ,tmp)
            ,rst))))

(macro peg-merge-altbranches (es)
  `(begin ,@es) ;;;TODO!!!
  )

(macro peg-dummy ()
  'nil)

(macro peg-dummy-back ()
  '(begin
     (__peg:set-position_ source saved)
     nil))

(macro peg-loop (e)
  (with-syms (tmp tmp2 lbl)
    `(let ((,tmp nil))
       (n.label ,lbl)
       (let ((,tmp2 ,e))
         (if (peg-success? ,tmp2)
             (begin
               (n.stloc! ,tmp (peg-fast-collect ,tmp ,tmp2))
               (n.goto ,lbl))
             ,tmp))
       )))

(macro peg-bind (mlt name e)
  (alet iname (if (eqv? name '*val*) 'finalresult name)
  (with-syms (tmp)
   `(let ((,tmp ,e))
      (if (peg-success? ,tmp)
          (begin
            (n.stloc! ,iname
                      ,(if (eqv? name '*val*)
                           (if mlt
                               `(peg-collval ,tmp ,iname)
                               `(cons '*val* ,tmp))
                           (if mlt
                               `(cons ,tmp ,iname)
                               tmp))
                      )
            nil
            )
          ,tmp)))))


(macro peg-call-gen (name rec ttype)
  (if rec
      `(peg-call ,name)
      (p:match ttype
        (lterm `(peg-call-simple ,name))
        (token `(peg-call-simple ,name))
        (else
         `(peg-call-nr ,name)))))

(macro peg-call-terminal-gen (name rec ttype)
  (if rec
      `(peg-call-terminal ,name)
      (p:match ttype
        (lterm `(peg-call-simple ,name))
        (token `(peg-call-simple ,name))
        (else
         `(peg-call-nr-terminal ,name)))))

(macro peg-call-simple (name)
  `((straise (car (__peg_xhashget (PegContext) (quote ,(Sm<< name))))) Env _lrstk source))

(macro peg-call-terminal (name)
  `(__peg:apply_ Env _lrstk source
                 (straise (__peg_xhashget (PegContext) (quote ,(Sm<< name))))))

(macro n.eq? (a b)
  (with-syms (na nb)
    `(let ((,na ,a) (,nb ,b))
       (not.neth ((System.Int32 ,na) (System.Int32 ,nb))
          (if (== ,na ,nb) (leave ((object)1)) (leave null))))))

(macro peg-trivial (tcode)
  (pktrivial:visit pred tcode
    (pred DEEP
      ((char
        `(if (const.eq? (__peg:get-advance source) ,chr)
             (peg-dummy)
             (peg-fail))
        )
       (anychar
        `(alet ch (__peg:get-advance source)
               (if (const.>= ch 0)
                   (peg-dummy)
                   (peg-fail))))
       (fail `(peg-fail))
       (range
        `(alet ch (__peg:get-advance source)
               (if (and (const.>= ch ,from)
                        (const.<= ch ,to))
                   (peg-dummy)
                   (peg-fail)))
        )
       (or
        `(peg-trivial-or ,@ps)
        )
       (string
        `(peg-trivial-string ,@str)
        )
       (sstring
        `(peg-trivial-string ,@(map ascii (string->list str))))
       ))))