;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(macro fccase_sw (arg . elts0)
   (let* ((s (gensym)) (ss (gensym))
          (h (gensym))
          (mp (mkhash))
          (tcnt (mkref 0))
          (cnt (mkref 0))
          (ts (tailsplit (fun (x) (eqv? (car x) 'else)) elts0))
          (elts (cdr ts))
          (deflt (car ts))
          )
     (foreach (k elts)
       (when (list? (car k))
         (foreach (i (car k))
           (alet chk (ohashget mp i)
             (if chk
                 (ccerror `(FCCASE-DUPLICATE ,i))))
           (ohashput mp i (deref cnt))
           (r! tcnt (+ (deref tcnt) 1))
           )
         (r! cnt (+ (deref cnt) 1))
         ))
     (if (> (deref tcnt) 7)
     `(let* (
             (,h (straise
                  (let ((,h (mkhash)))
                    ,@(foreach-mappend (k elts)
                         (foreach-map (i (car k))
                           `(ohashput ,h (quote ,i) ,(ohashget mp i))))
                    ,h
                    )))
             (,s ,arg)
             (,ss (ohashget ,h (car ,s)))
             )
        (if ,ss
          (switch ,ss
           ,@(foreach-map (k elts)
               (format k (syms fm . body)
                  `(,(ohashget mp (car syms))
                      (format (cdr ,s) ,fm
                              ,@body)))))
         ,(if deflt
             (format (car deflt) (_ . body)
                  `(begin ,@body))
             'nil
             )))
     `(fccase.inner ,arg ,@elts0)
     )))

(cmacro fccase rest `(fccase_sw ,@rest))

(unit-test 3 (fccase '(a 1 2) ((a) _ 10) ((b c) _ 20)) 10)


