;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(function cc:constant-p ( expr )
  (cc:mbcoreast:visit expr expr
    (expr DEEP
     ((Num #t)
      (Str #t)
      (Chr #t)
      (Bool #t)
      (Symbol #t)
      (Nil #t)
      (Quote #t)
      (Car e)
      (Cdr e)
      (Cons (and a b))
      (Cons1 a)

      (NullP a)
      (PairP a)
      (Not a)
      (Eqv (and a b))
      (BinOp (and left right))
      
      (else nil)))))

(function cc:get-arithop (op)
  (case op
    ((Add) '+)
    ((Sub) '-)
    ((Mul) '*)
    ((Div) '/)
    (else (ccerror 'ARITHOP))))

(function cc:constant-eval ( expr )
  (let* ((code 
         (cc:mbcoreast:visit expr expr
           (expr DEEP
            ((Num n)
             (Str s)
             (Chr c)
             (Bool b)
             (Symbol `(quote ,s))
             (Nil 'nil)
             (Quote `(quote ,l))
             (Car `(car ,e))
             (Cdr `(cdr ,e))
             (Cons `(cons ,a ,b))
             (Cons1 `(cons ,a nil))
             
             (NullP `(null? ,a))
             (PairP `(pair? ,a))
             (Not `(not ,a))
             (Eqv `(eqv? ,a ,b))
             (BinOp `(,(cc:get-arithop op) ,left ,right))
             (else (ccerror '(IMPOSSIBLE)))))))
         (v (read-int-eval code)))
    `(Quote ,v)))
        
; A simple constant folding pass
(recfunction cc:constant-fold ( expr )
  (cc:mbcoreast:visit expr expr
    (expr _
      ((NoConst node)
       (else-deep
        ((else (if (cc:constant-p node)
                   (cc:constant-eval node)
                   node))))))))