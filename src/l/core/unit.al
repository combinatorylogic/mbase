;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Simple unit tests generation framework.

(recfunction deep-comp (a b)
  (cond
   ((and (null? a) (null? b)) #t)
   ((and (list? a) (list? b))
    (and (deep-comp (car a) (car b))
         (deep-comp (cdr a) (cdr b)) ))
   (else (eq? a b))))

(cmacro unit-test (level src exp)
  (alet tests (shashget (getfuncenv) 'unit-tests-destination)
     (when tests
       (set-car! tests (cons (list level 'T src exp) (car tests))))
     `(begin )))

(cmacro unit-test-defn (level code)
 (alet tests (shashget (getfuncenv) 'unit-tests-destination)
     (when tests
       (set-car! tests (cons (list level 'I code nil) (car tests))))
     `(begin )))


(macro unit-tests-use ()
  (shashput (getfuncenv) 'unit-tests-destination (noconst (cons nil nil)))
  `(begin ))

(macro unit-tests-dump outs
  (alet tests (shashget (getfuncenv) 'unit-tests-destination)
    (when tests
       (with-hash (lvls)
         (foreach (i (car tests))
           (format i (lvl md src exp)
              (lvls! lvl (cons (list md src exp) (lvls> lvl)))))
         (foreach (o outs)
           (format o (level fname)
              (alet ii (lvls> level)
                 (call-with-output-file (S<< fname)
                    (fun (fo)
                      (foreach (i ii)
                        (format i (md src exp)
                          (case md
                            ((T)
                             (fprintln fo (to-string `(u:test ,src ,exp))))
                            ((I)
                             (fprintln fo (to-string src)))
                            ))
                      )))))
         ))))
  `(begin )
  )


