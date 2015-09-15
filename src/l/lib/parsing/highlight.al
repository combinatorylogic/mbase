;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Stream coloring engine

(function make-accept-signal-1 (env mergef)
  (fun (tag first last signal)
    (alet ll (sposloc last)
    (let loop ((f first))
     (alet fl (sposloc f)
         (if (= ll fl) nil
           (let* ((ox (ohashget env fl))
                  (nn (mergef signal ox)))
             (ohashput env fl nn)
             (loop (StreamEntry.chknext f))
             )))))))

(function make-accept-signal (env)
    (fun (tag first last signal)
      (let* ((f (sposloc first))
             (l (sposloc last)))
        (writeline `(SIGNAL ,tag ,signal ,f - ,l))
        (use-hash (env)
          (env+! f signal)
          (env+! l 'END)))))

(function ___mkheader (s)
  (p:match s
    (($a $b) (Sm<< (S<< a) "=" (S<< b)))
    (else (Sm<< s))))

(function prepare-colored-stream (env stream endstream)
  (use-hash (env)
  (list 'normal
  (let loop ((p stream) (cuent nil) (stk nil) (comnds nil))
    (if (or (null? p)
            (and endstream (eqv? p endstream)))
        (if stk
            (loop nil (cons (reverse cuent) (car stk))
                  (cdr stk) nil)
            (reverse cuent))
        (let* ((ch (StreamEntry.char p))
               (i (StreamEntry.idx p))
               (n (StreamEntry.chknext p))
               (cv (if (> ch 0) (n2s ch) ""))
               (f (if comnds
                      (if (list? comnds) comnds
                          nil)
                      (env> i)))
               (fnx (if f (let ((x (cdr f))) (if x x 'E-E-E-E))))
               )
          (if f (writeline `(FFF: ,i ,f ,(length stk))))
          (if f
              (if (list? (car f))
                  (loop p (list (___mkheader (car f))) (cons cuent stk) fnx)
                  (if stk
                      (loop p (cons (reverse cuent)
                                    (car stk))
                            (cdr stk) fnx)
                      (writeline `(Ooops! ,i))
                      ))
              (loop n (cons cv cuent) stk nil)
              )))))))

(function tthashget (h v)
  (alet r (hashget h v)
    (if r r "")))

(recfunction print-colored-stream (file tabs colours lst)
  (let* ((m (if (symbol? (car lst)) (car lst) nil))
         (rst (if m (cdr lst) lst)))
    (if m (fprint file (S<< "{" (tthashget colours m) "{")))
    (foreach (x rst)
      (if (list? x)
          (print-colored-stream file tabs colours x)
          (fprint file (tabs (S<< x)))))
    (if m (fprint file "}}"))))

;;;;;;; An alternative stream colouring approach



(function make-accept-signal-arlist (al mergef)
  (fun (tag first last signal)
    nil))

(function begin-new-state (file colour  stlist)
  (let* ((stcoll (map ___mkheader stlist))
         (lll
          (foreach-mappend (x stcoll)
           (alet t (hashget colour x)
                 (if t (list t) nil)))))
    (if lll (fprint file "{"))
    (if lll
        (begin
          (foreach (i lll) (fprint file i))
          (fprint file " ")))
    (if lll (fprint file "{"))
    (return (if lll nil #t))
    ))

(function flush-code (file tabs ch xs rlist)
  (let* ((str (list->string (reverse rlist)))
         (tabsfun (mkref tabs))
         (xvs (foreach-mappend (x xs)
                 (p:match x
                   ((screen none) (r! tabsfun I) nil)
                   (else
                    (alet t (hashget ch (S<< "f:" (car x) "=" (cadr x)))
                          (if t (list t) nil))))))
         (tr (if xvs
                 (foldl (fun (x v)
                          (v x)) str xvs)
                 ((deref tabsfun) str)
                 )))
    (fprint file tr)))


(function print-rle-stream (file tabs colour henv stream endstream)
  (let loop ((p stream) (state "")
             (clean? #t)
             (clect nil) (xstate nil)
             )
    (if (or (null? p)
            (and endstream (eqv? p endstream)))
        (begin
          (flush-code file tabs colour xstate clect)
          (if (not clean?)
              (fprint file "}}")))
        (let* ((ch (StreamEntry.char p))
               (i (StreamEntry.idx p))
               (n (StreamEntry.chknext p))
               (st (ohashget henv i))
               (stst (if st (to-string st) ""))
               (cv (if (> ch 0) (list (n2s ch)) nil))
               (ncl
                (cond
                 ((string=? stst state)
                  (begin
                    (list
                     (append cv clect)
                     clean?
                     xstate
                     )))
                 (st
                  (begin
                    (flush-code file tabs colour xstate clect)
                    (if (not clean?) (fprint file "}}"))
                    (alet nclean (begin-new-state file colour st)
                          (list
                           cv
                           nclean
                           st
                           ))))
                 (else
                  (begin
                    (flush-code file tabs colour xstate clect)
                    (if (not clean?) (fprint file "}}"))
                    (alet nclean (begin-new-state file colour nil)
                          (list cv
                                nclean
                                st
                                ))
                    ))
                 )))
          (loop n stst (cadr ncl) (car ncl) (caddr ncl) )))))

(function pp-defaultmerge (s l)
  (let loop ((i l) (t (car s)))
    (if (null? i) (list s)
        (let* ((ii (car i)))
          (if (eqv? (car ii) t)
              (cons (cons t (cdr s)) (cdr i))
              (cons ii (loop (cdr i) t)))))))

(function pp-defaultmerge2 (s l)
  (let loop ((i l) (t (car s)))
    (if (null? i) (list s)
        (let* ((ii (car i)))
          (if (eqv? (car ii) t)
              i
              (cons ii (loop (cdr i) t)))))))


