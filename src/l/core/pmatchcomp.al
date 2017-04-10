;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; A tail recursion-friendly reimplementation of pattern matching
;;

(expand-if (shashget (getfuncenv) 'compiler-final)
 (top-begin
  (cmacro pm:ptn-try (cnd body)
   `(if ,cnd ,body
        (pm:ptn-failed-m)))

  (cmacro p:match (val . ptns)
     (with-syms (lop agr cnt res)
        `(let ,lop ((,agr ,val) (,cnt 0))
           (switch ,cnt
              ,@(let loop ((ps ptns) (n 0))
                  (cond
                   ((null? ps) `((,n nil)))
                   ((eqv? 'else (caar ps))
                    `((,n ,(cadar ps))))
                   (else
                    `((,n
                       (with-macros ((pm:ptn-failed-m (fun (_)
                                                        (list (quote ,lop) (quote ,agr) ,(+ n 1)))))
                                     ,(pm:ptn-unroll
                                       (pm:ptn-process (caar ps))
                                       agr
                                       `(begin ,@(cdar ps)))))
                      ,@(loop (cdr ps) (+ n 1))))
                   ))))))
  ))



