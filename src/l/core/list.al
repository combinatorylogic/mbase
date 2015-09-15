;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Basic list comprehensions implementation

(Section "List comprehensions")

(function lch:tknize ( right )
  (foreach-map (r right)
     (cond
       ((symbol? r)
        (case r
         ((<- & |) `(,r))
         (else `(VAR ,r))))
       (else `(OTHER ,r)))))


(bnf-parser
  ((comprexpr lch:parse-compr))

  (comprexpr
    ((expr:e | rightexpr:r) `(lch:list-comprehension ,e (generators ,@r))))

  (expr
    ((VAR) $0)
    ((OTHER) $0))

  (rightexpr
    ((rightsome:l | rightexpr:r) (cons l r))
    ((rightsome) (list $0)))

  (rightgena
    ((expr:v <- expr:e) `(<- ,v ,e)))

  (rightsome
    ((rightgena:l & rightexprs:r) `(with ,l ,@r))
    ((rightgena) $0))
  (rightexprs
    ((expr:l & rightexprs:r) (cons l r))
    ((expr) (wrap $0)))

)

(function lch:parse ( expr )
   (car ((lch:parse-compr nil) (lch:tknize expr))))

(macro lch:list-comprehension ( inner-expr generators )
  (let loop (( v (cdr generators )))
    (p:match v
      (() inner-expr)
      (((<- $vr $e) . $cdrv)
       (let ((nvr (if (list? vr) (gensym) vr)))
         `(,(if (null? cdrv) 'foreach-map 'foreach-mappend) (,nvr ,e)
           ,(if (list? vr) `(format ,nvr ,vr ,(loop cdrv)) (loop cdrv)))))
      (((with ($_ $vr $e) . $qery) . $cdrv)
       `(,(if (null? cdrv) 'foreach-map-filter 'foreach-mappend-filter)
         (,vr ,e) (and ,@qery)
         ,(loop cdrv)
         )))))

(macro <L> rest
   (
    "A list comprehensions macro."
    ""
    "Format: [[(<L> generator-expression | source-sets*)]]"
    ""
    "Usage example:"
    "[["
    " (<L> (cons x y) | x <- '(a b) | y <- '(a b) & (not (eqv? x y)))"
    "]]"
    )
   (lch:parse rest))

(macro list-comprehension rest
  (lch:parse rest))

