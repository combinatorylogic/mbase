;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define t_exception (dotnet "Exception"))
(recfunction deep-comp (a b)
  (cond
   ((and (null? a) (null? b)) #t)
   ((and (list? a) (list? b))
    (and (deep-comp (car a) (car b))
         (deep-comp (cdr a) (cdr b)) ))
   (else (eq? a b))))

(function test-to-string (expr)
  (to-string
   (let loop ((e expr))
     (cond
      ((null? e) e)
      ((list? e)
       (cond
        ((and (car e)
              (eqv? (car e) '-test-hide-))
         (cadr e))
        (else
         (cons (loop (car e))
               (loop (cdr e))))))
      (else e)))))

(macro u:etest (expr res0)
  `(begin
     (print ,(test-to-string expr))
     (let ((rval ,res0)
           (res (try ,expr t_exception to-string)) )
       (print " = ")
       (print (to-string res))
       (println (if (deep-comp res rval) " [OK]"
                    (buildstring " [FAILED], exp: "
                                  (to-string rval))))
       )))

(macro u:test (expr rest)
  `(u:etest ,expr (quote ,rest)))