;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "./lrutil.al")

(function __peg:growlr_ (Env _lrstk Pos R P M H)
  (__peg:heads! P H)
  (let loop ()
    (__peg:set-position_ Pos P) ;; backtrack
    (__peg:head.evalSet! H (__peg:set-copy_ (__peg:head.involvedSet H)))
    (let ((ans (__peg:eval_ Env _lrstk Pos R)))
      (if (or (__peg:is-fail? ans)
              (__peg:pos-before? (__peg:get-position_ Pos) (__peg:memo-entry.pos M)))
          (begin
            (__peg:heads! P nil)
            (__peg:set-position_ Pos (__peg:memo-entry.pos M))
            (__peg:memo-entry.ans M))
          (begin
            (__peg:memo-entry.ans! M ans)
            (__peg:memo-entry.pos! M (__peg:get-position_ Pos))
            (loop))))))

(function __peg:preplr_ (Env _lrstk R L)
  (if (null? (PegLR.head L))
      (PegLR.head! L (__peg:head.new R nil nil))
      )
  (let ((L.head (PegLR.head L)))
    (let loop ((s (deref _lrstk)))
      (if (or
           (null? s)
           (if (null? (PegLR.head s)) nil
               (_rule-eq? (__peg:head.rule (PegLR.head s))
                          (__peg:head.rule L.head))))
        nil
        (begin
          (PegLR.head! s L.head)
          (__peg:head.involvedSet! L.head
                             (__peg:set-add-rule_
                              (__peg:head.involvedSet L.head)
                              (PegLR.rule s)))
          (loop (PegLR.next s)))))
    ))

(function __peg:lrreslt_ (Env _lrstk Pos R P M)
  (let ((h (PegLR.head (__peg:memo-entry.ans M))))
    (if (not (_rule-eq? (__peg:head.rule h) R))
        (PegLR.seed (__peg:memo-entry.ans M))
        ;; else
        (begin
          (__peg:memo-entry.ans! M (PegLR.seed (__peg:memo-entry.ans M)))
          (if (__peg:is-fail? (__peg:memo-entry.ans M))
              (__peg:memo-entry.ans M)
              (__peg:growlr_ Env _lrstk Pos R P M h))))))

(function __peg:recall_ (Env _lrstk Pos R P)
  (let ((m (__peg:memo R P))
        (h (__peg:heads P)))
    (cond
     ((null? h)
      m)
     ((and (null? m)
           (not (__peg:set-in? R (__peg:set-add-rule_ (__peg:head.involvedSet h)
                                         (__peg:head.rule h)))))
      (fail-memo-entry P))
     ((__peg:set-in? R (__peg:head.evalSet h))
      (begin
        (__peg:head.evalSet! h (__peg:set-remove_ R (__peg:head.evalSet h)))
        (let ((ans (__peg:eval_ Env _lrstk Pos R)))
          (__peg:memo-entry.ans! m ans)
          (__peg:memo-entry.pos! m (__peg:get-position_ Pos))
          m
          )))
     (else m))))

(define __peg:debug__ (mkref nil))


(function __peg:pushenv (Env)
  (let* ((nw  (mkref nil))
         (snw (mkref nil))
         (stk (PegEnv.termstack Env))
         (sigstk (PegEnv.signals Env)))
    (PegEnv.termstack! Env (cons nw stk))
    (PegEnv.signals! Env (cons snw sigstk))
    ))

(function __peg:pokeenv (Env m)
  (let* ((inner (__peg:memo-entry.inner m))
         (signals (__peg:memo-entry.signals m))
         (sigstk (PegEnv.signals Env))
         (stk (PegEnv.termstack Env)))
    (PegEnv.signals! Env (cons signals sigstk))
    (PegEnv.termstack! Env (cons inner stk))))

(function __peg:popenv (Env P R m)
  (let* ((tp (PegEnv.termstack Env))
         (sp (PegEnv.signals Env)))
    (__peg:memo-entry.inner! m (car tp))
    (__peg:memo-entry.signals! m (car sp))
    (PegEnv.termstack! Env (cdr tp))
    (PegEnv.signals! Env (cdr sp))
    ))

(function __peg:culllist (P l0)
  (let loop ((l l0))
    (if l
        (let* ((pos (__peg:memo-entry.first (car l))))
          (if (__peg:pos-before? P pos) (loop (cdr l))
              l))
        nil)))

(function __peg:saveentry (lst P R m)
  (let* ((L (deref lst)))
    (if (null? L)
        (r! lst (cons P (list m)))
        ;; otherwise check if it's on the right
        (if (__peg:pos-before? (car L) P)
            ;; Just add the new one and update
            (r! lst (cons P (cons m (cdr L))))
            ;; otherwise remove all the entries on the right of the new one
            (r! lst (cons P (cons m (__peg:culllist P (cdr L)))))))))

(recfunction __peg:itersignals (Env m0)
  (let* ((slot (PegEnv.other Env))
         (cycles (mkhash)))
    (let loop ((m m0))
      (if (ohashget cycles m)
          nil ; (writeline `(CYCLE!!!! ,m))
          (let* (
                 (ss (__peg:memo-entry.signals m))
                 (im (__peg:memo-entry.inner m)))
            (ohashput cycles m 1)
            (if ss (foreach (s (deref ss))
                     (slot (PegSignal.tag s)
                           (PegSignal.first s)
                           (PegSignal.last s)
                           (PegSignal.signal s))))
            (if (and im
                     (deref im))
                (foreach (i (cdr (deref im)))
                  (loop i))))))))

(function __peg:dumpmemo-range (Env m0)
  (let* ((str (__peg:get-delta (__peg:memo-entry.first m0)
                               (__peg:memo-entry.pos m0))))
    (if (< (length (string->list str)) 150)
        (begin
          (println (S<< "   >>> " str " <<< "))
          1)
        nil)))

(function __peg:dumpmemo (Env m0 m)
  (let* ((cdr1 (fun (l)
                 (if l (cdr l) nil)))
         (left (__peg:get-delta (__peg:memo-entry.first m0)
                                (__peg:memo-entry.first m)))
         (middl (__peg:get-delta (__peg:memo-entry.first m)
                                 (__peg:memo-entry.pos m)))
         (right (__peg:get-delta (__peg:memo-entry.pos m)
                                 (__peg:memo-entry.pos m0))))
    (println (S<< "|" (__peg:memo-entry.tag m) "::  " (S<< (__peg:memo-entry.ans m))))
    (print "   >>>|")
    (foreach (i (string->list left)) (print "-"))
    (print "!")
    (foreach (i (cdr1 (string->list middl))) (print "+"))
    (foreach (i (string->list right)) (print "-"))
    (println "|<<< ")
    ))

(recfunction firstn (n l x)
  (if l
      (if (> n 0) (cons (S<< (car l))
                        (firstn (- n 1) (cdr l) x))
          nil)
      (if (> n 0) (cons x (firstn (- n 1) nil x))
          nil)))

(function __peg:dumpsignal (Env m0 tag first last signal)
  (let* (
         (cdr1 (fun (l)
                 (if l (cdr l) nil)))
         (left (__peg:get-delta (__peg:memo-entry.first m0)
                                first))
         (middl (__peg:get-delta first last))
         (sig (cadr signal))
         (ln (length (string->list middl)))
         (sigstr (firstn ln (string->list (S<< sig)) "+"))
         (right (__peg:get-delta last
                                 (__peg:memo-entry.pos m0))))
    (print "S  >>>|")
    (foreach (i (string->list left)) (print "-"))
    (print "!")
    (foreach (c (cdr1 sigstr)) (print "+"))
    (foreach (i (string->list right)) (print "-"))
    (println "|<<< ")
    (println (S<< "S  >>>[" signal "]"))
    ))

(recfunction __peg:dumpsignals (Env m0)
  ;; dump the line
  (if (__peg:dumpmemo-range Env m0)
  (let* ((slot (PegEnv.other Env))
         (cycles (mkhash)))
    (let loop ((m m0))
      (if (ohashget cycles m)
          nil ; (writeline `(CYCLE!!!! ,m))
          (let* (
                 (ss (__peg:memo-entry.signals m))
                 (im (__peg:memo-entry.inner m)))
            (__peg:dumpmemo Env m0 m)
            (ohashput cycles m 1)
            (if ss (foreach (s (deref ss))
                     (__peg:dumpsignal Env m0
                                       (PegSignal.tag s)
                                       (PegSignal.first s)
                                       (PegSignal.last s)
                                       (PegSignal.signal s))))
            (if (and im
                     (deref im))
                (foreach (i (cdr (deref im)))
                  (loop i)))))))))

(function __peg:updenv (Env P R m)
  (if (eqv? __peg-epic-fail__ (__peg:memo-entry.ans m)) nil
  (let* ((tp (PegEnv.termstack Env)))
    (if tp
        (__peg:saveentry (car tp) P R m)
        ;; A topmost entry: time to pass all the signals back
        (if (shashget (getfuncenv) 'debug-compiler-signals)
            (__peg:dumpsignals Env m)
            (__peg:itersignals Env m))
        ))))

(function __peg:apply_ (Env _lrstk Pos R)
  (let* ((P (__peg:get-position_ Pos))
         (m (__peg:recall_ Env _lrstk Pos R P)))
    (if (null? m)
        (let ((lr (PegLR.new '*LR* __peg-epic-fail__ R nil (deref _lrstk))))
          (r! _lrstk lr)
          (__peg:pushenv Env)
          (let* ((m (__peg:memo-entry.new lr P nil nil nil nil))
                 (_ (__peg:memo! P R m))
                 (ans (__peg:eval_ Env _lrstk Pos R)))
            (r! _lrstk (PegLR.next (deref _lrstk)))
            (__peg:memo-entry.pos! m (__peg:get-position_ Pos))
            (let ((re0
                   (if (not (null? (PegLR.head lr)))
                       (begin
                         (PegLR.seed! lr ans)
                         (__peg:lrreslt_ Env _lrstk Pos R P m))
                       (begin
                         (__peg:memo-entry.ans! m ans)
                         ans))))
              (__peg:popenv Env P R m)
              (__peg:updenv Env P R m)
              re0
              )))
        ; else: memo is already there
        (alet ans (__peg:memo-entry.ans m)
          (__peg:set-position_ Pos (__peg:memo-entry.pos m))
          (if (__peg:is-lr? ans)
              (begin
                (__peg:pushenv Env) ; clear the old version

                (__peg:preplr_ Env _lrstk R ans)
                (alet re0 (PegLR.seed ans)
                  (__peg:popenv Env P R m)
                  re0))
              ans)
        ))))

(function __peg:sapply_ (Env _lrstk Pos R)
  (let* ((P (__peg:get-position_ Pos))
         (m (__peg:memo R P)))
    (cond
     ((null? m)
      (let* ((_ (__peg:pushenv Env)) ; create a new token/signal collection target
             (ans (__peg:eval_ Env _lrstk Pos R))
             (m (__peg:memo-entry.new ans (__peg:get-position_ Pos) nil nil nil nil)))
        (__peg:memo! P R m)
        (__peg:popenv Env P R m) ; flush the token/signal collector, add it to the token
        (__peg:updenv Env P R m) ; signal the upper level
        ans))
     (else
      (__peg:set-position_ Pos (__peg:memo-entry.pos m))
      (__peg:updenv Env P R m) ; signal the upper level
      (__peg:memo-entry.ans m))
     )))

