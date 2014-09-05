;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
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

(function __peg:apply_ (Env _lrstk Pos R)
  (let* ((P (__peg:get-position_ Pos))
         (m (__peg:recall_ Env _lrstk Pos R P)))
    (if (null? m)
        (let ((lr (PegLR.new '*LR* __peg-epic-fail__ R nil (deref _lrstk))))
          (r! _lrstk lr)
          (let* ((m (__peg:memo-entry.new lr P nil))
                 (_ (__peg:memo! P R m))
                 (ans (__peg:eval_ Env _lrstk Pos R)))
            (r! _lrstk (PegLR.next (deref _lrstk)))
            (__peg:memo-entry.pos! m (__peg:get-position_ Pos))
            (if (not (null? (PegLR.head lr)))
                (begin
                  (PegLR.seed! lr ans)
                  (__peg:lrreslt_ Env _lrstk Pos R P m))
                (begin
                  (__peg:memo-entry.ans! m ans)
                  ans))))
        (alet ans (__peg:memo-entry.ans m)
          (__peg:set-position_ Pos (__peg:memo-entry.pos m))
          (if (__peg:is-lr? ans)
              (begin
                (__peg:preplr_ Env _lrstk R ans)
                (PegLR.seed ans))
              ans))
        )))

(function __peg:sapply_ (Env _lrstk Pos R)
  (let* ((P (__peg:get-position_ Pos))
         (m (__peg:memo R P)))
    (cond
     ((null? m)
      (let* ((ans (__peg:eval_ Env _lrstk Pos R))
             (m (__peg:memo-entry.new ans (__peg:get-position_ Pos) nil)))
        (__peg:memo! P R m)
        ans))
     (else
      (__peg:set-position_ Pos (__peg:memo-entry.pos m))
      (__peg:memo-entry.ans m))
     )))

