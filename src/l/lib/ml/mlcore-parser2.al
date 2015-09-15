;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(function peg-function-mltypename (n)
  (p:match n
    ((T $v . $args) `(,v ,@args))
    (else (ccerror `(ML:TYPENAME ,n))))
  )

(function peg-function-mltypeargcheck (a)
  (p:match a ((T $id) (list id))
             (else (ccerror `(ML:TYPENAME ,a))))
  )

(function peg-function-mlmaketype (a b)
  (let* ((l (cons a b))
         (tl (cuttail l))
         (lt (car (lasttail l))))
    `(T ,lt ,@tl))
  )

(define <*ML-INFIX*> (mkhash))

(function peg-function-definfix (str)
  (hashput  <*ML-INFIX*> str #t))

(function peg-checkfunction-ml_check_infix (str)
  (if (hashget <*ML-INFIX*> str) #t nil))


(packrat-file "./mllexer.peg")
(packrat-file "./mlparser.peg")


