;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define __peg-epic-fail__ (Sm<< " * EPIC * FAIL * "))


;; Structures

; LR(seed: AST, rule: Rule, head: Head, next: LR)

(nrec:def PegLR tag seed rule head next)

; __peg:head(rule:Rule, involvedSet, evalSet: Set of Rule)

(nrec:def __peg:head rule involvedSet evalSet)

; __peg:memo-entry(ans: Ast/LR, pos: Position)

(nrec:def __peg:memo-entry ans pos signals inner first tag)


(function __peg:set-add-rule_ (set rule)
  (cons (cdr rule) set))

(function __peg:set-in? (rule set)
  (let ((res (mkref nil))
        (rn (cdr rule)))
    (foreach (s set)
      (if (eqv? s rn)
          (begin
            (r! res #t)
            (foreach-break))))
    (deref res)))

(function __peg:set-remove_ (rule set)
  (alet rn (cdr rule)
  (collector (add get)
    (foreach (s set)
      (if (eqv? s rn) nil
          (add s)))
    (get)
    )))

(function __peg:set-copy_ (set)
  set)

;; Rule format: (fun . tag)

(function __peg:eval_ (Env _lrstk Pos R)
  ((car R) Env _lrstk Pos))

(ctime
 `(nrec:def StreamEntry heads memos (int idx) (int char) next
            ,@(if ##packrat-hist `(count) nil)
            ))

(function StreamEntry.new1 (heads memos idx char next)
  (ctime
   `(StreamEntry.new heads memos idx char next
                     ,@(if ##packrat-hist `(0) nil)
                     )))

(function StreamEntry.chknext (se)
  (alet ne (StreamEntry.next.M se)
    (if (list? ne)
        ((car ne))
        ne)))

(function __peg:memo (R P)
  (let* ((ms (StreamEntry.memos.M P))
         (tag (cdr R))
         (res (mkref nil)))
    (foreach (m ms)
      (if (eqv? (car m) tag)
          (begin
            (r! res (cdr m))
            (foreach-break))))
    (deref res)))

(function __peg:memo! (P R mn)
  (__peg:memo-entry.first! mn P)
  (__peg:memo-entry.tag! mn (cdr R))
  (StreamEntry.memos! P (cons
                         (cons (cdr R) mn)
                         (StreamEntry.memos P))))

(function __peg:heads (P)
  (StreamEntry.heads.M P))

(function __peg:heads! (P H)
  (StreamEntry.heads! P H))

(function __peg:set-position_ (Pos P)
  (if (null? P) (ccerror `(POS)))
  (r! Pos P))

(function __peg:get-position_ (Pos)
  (deref Pos))

(macro __peg:get-position_ (Pos)
  `(deref ,Pos))

(function __peg:get-advance (source)
  (let* ((P (__peg:get-position_ source)))
    (if P
        (let* (
               (ch (StreamEntry.char.M P))
               (N (StreamEntry.chknext P)))
          (ctime
           (if ##packrat-hist
               `(StreamEntry.count! P (+ (StreamEntry.count P) 1))
               'nil))
          (if (null? N)
              (r! source (StreamEntry.new1 nil nil 0 -1 nil))
              (r! source N))
          ch)
        -1
        )))

(function __peg:get-check (source)
  (let* ((P (__peg:get-position_ source)))
    (if (null? P) -1
        (let* (
               (ch (StreamEntry.char P))
               (N (StreamEntry.chknext P)))
          ch))))

(function __peg:stream-hist (P)
 (ctime (if ##packrat-hist
  `(let* ((h (cons nil nil))
         (cur (cons h nil)))
    (let loop ((p P))
      (let* ((ch (StreamEntry.count.M p))
             (N (StreamEntry.chknext p))
             (nw (cons (list (StreamEntry.idx.M p) (n2s (StreamEntry.char.M p)) ch) nil)))
        (set-cdr! (car cur) nw)
        (set-car! cur nw)
        (if (null? N) nil (loop N))))
    (cdr h))
  'nil)))

(function get-stream-next (P)
  (StreamEntry.chknext P))

(function _rule-eq? (A B)
  (eq? (cdr A) (cdr B)))

(function __peg:is-fail? (ans)
  (eqv? ans __peg-epic-fail__))

(function __peg:pos-before? (P1 P2)
  (<= (StreamEntry.idx.M P1)
      (StreamEntry.idx.M P2)))

(function fail-memo-entry (P)
  (__peg:memo-entry.new __peg-epic-fail__ P nil nil nil nil))

(function __peg:is-lr? (ans)
  (if (null? ans) nil
   (not.neth ((object ans))
      (if (istype ans PegLR)
          (leave ((object)true))
          (leave null)))))

(function __peg:newline (i)
  (not.neth ((int i))
     (x = (and i 65535))
     (leave ((object)(+ (- i x) 65536)))))

(function peg:decode-pos (i)
   (not.neth ((int i))
      (x = (and i 65535))
      (y = (/ (- i x) 65536))
      (leave (mblist ((object)y) ((object)x)))))

;;;;
(function __peg:lst2stream (lst len)
  (let* ((p0 (StreamEntry.new1 nil nil 0 -1 nil))
         (inc (if len (fun () (r! len (+ (deref len) 1))) (fun () nil)))
         )
    (let loop ((l lst) (i 0) (c p0) (p p0))
      (cond
       ((null? l)
        (alet lst (StreamEntry.new1 nil nil i -1 nil)
              (StreamEntry.next! c lst)
              (StreamEntry.chknext p)))
       ((list? l) ;; otherwise it's a continuation fun
        (alet n1 (StreamEntry.new1 nil nil i (ascii (car l)) nil)
              (StreamEntry.next! c n1)
              (if len (inc))
              (loop (cdr l)
                    (if (eq? #\Newline (car l)) (__peg:newline i) (+ i 1))
                    n1 p)))
       (else ;; continuation
        (StreamEntry.next! c
                           (let ((pn c))
                             (list (fun ()
                                     (loop (l) i pn pn)))))
        (StreamEntry.next.M p)
        )))))

(function pmemos (mms)
  (to-string
   (foreach-map (m mms)
     (alet mm (cdr m)
           (if (or (__peg:is-lr? (__peg:memo-entry.ans mm))
                   (eqv? __peg-epic-fail__ (__peg:memo-entry.ans mm)))
               (Sm<< "!" (car m))
               (cons (Sm<< (car m) ":") (__peg:memo-entry.ans mm)))))))

(function __peg:streamprint (shit)
  (let loop ((s shit))
    (if (null? s) nil
        (let* ((idx (StreamEntry.idx.M s))
               (ch (StreamEntry.char.M s))
               (mm (StreamEntry.memos.M s)))
          (println (S<< idx ": " (if (> ch 0) (n2s ch) "EOF") ": " (pmemos mm)))
          (loop (StreamEntry.chknext s))))))

(function __peg:get-delta (p0 p1)
  (let* ((res (cons nil nil))
         (tmp res)
         (p p0))
    (n.label repeat)
    (if (or (null? p) (eqv? p p1))
        (list->string (cdr res))
        (let* ((ch (StreamEntry.char.M p))
               (cv (n2s ch))
               (l (cons cv nil)))
          (set-cdr! tmp l)
          (n.stloc! tmp l)
          (n.stloc! p (StreamEntry.chknext p))
          (n.goto repeat)
          ))))

(function __peg:get-ndelta (p0 n)
  (let* ((res (cons nil nil))
         (i 0)
         (tmp res)
         (p p0))
    (n.label repeat)
    (if (or (null? p) (>= i n))
        (list->string (cdr res))
        (let* ((ch (StreamEntry.char.M p))
               (cv (if (> ch 0) (n2s ch) #\!))
               (l (cons cv nil)))
          (set-cdr! tmp l)
          (n.stloc! tmp l)
          (n.stloc! p (StreamEntry.chknext p))
          (n.stloc! i (+ i 1))
          (n.goto repeat)
          ))))

(function sposloc (pos)
  (StreamEntry.idx.M pos))

(function peg:locpair (pos)
  (let* ((idx (StreamEntry.idx.M pos))
         (col (not.neth ((int idx))
                (leave (and idx 65535))))
         (pos0 (- idx col))
         (lnum (not.neth ((int pos0))
                 (leave (/ pos0 65536)))))
    (cons lnum col)))


(nrec:def PegEnv failure other ctrp signals termstack)

(nrec:def PegSignal tag first last signal)

(function peg:env-signal (env tag first last signal)
  (let* ((sg (PegSignal.new tag first last signal))
         (sigstk (PegEnv.signals env))
         (dst (if sigstk (car sigstk)
                  (let* ((m (mkref nil)))
                    (PegEnv.signals! env (list m))
                    m))))
    (r! dst (cons sg (deref dst)))))

(function peg:dummy-slot (tag first last signal)
  nil)

(function peg:makeenv ()
  (PegEnv.new nil peg:dummy-slot #t nil nil))

(function peg:makeenv-noctr ()
  (PegEnv.new nil peg:dummy-slot nil nil nil))

(function peg:construct? (Env)
  (PegEnv.ctrp.M Env))

(function __peg:env:rightmostfailure (Env source name rep)
  (cond
   ((null? Env) nil)
   (else
    (let* ((pos (__peg:get-position_ source))
           (loc (StreamEntry.idx.M pos))
           (prev (PegEnv.failure.M Env)))
      (cond
       ((null? prev)
        (PegEnv.failure! Env (list loc pos (list (cons name rep)))))
       (else
        (format prev (oloc opos onames)
                (cond
                 ((> loc oloc)
                  (PegEnv.failure! Env
                                   (list loc pos (list (cons name rep)))))
                 ((eq? loc oloc)
                  (PegEnv.failure! Env
                                   (list loc pos
                                         (cons (cons name rep) onames))))
                 (else nil)))))))
   ))

(function __peg:displaypos (pos)
  (let* ((idx (StreamEntry.idx.M pos))
         (nd  (__peg:get-ndelta pos 80)))
    (list idx nd)))

(function peg:reportfailure (Env)
  (if (null? Env) nil
      (let* ((f (PegEnv.failure.M Env)))
        (if (null? f) nil
            (format f (loc pos names)
                    (cons (__peg:displaypos pos)
                          names))))))


(function peg:str->stream (str)
  (__peg:lst2stream (string->list str) nil))

(function peg:file->stream (fnam)
  (__peg:lst2stream (read-file-list fnam) nil))

(function peg:file->stream2 (fnam len)
  (__peg:lst2stream (read-file-list fnam) len))

