;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define flt:format
  ((r_tbind "System.Globalization.CultureInfo" "get_NumberFormat")
   ((r_tsbind "System.Globalization.CultureInfo" "get_InvariantCulture"))))

(function flt:parse (s)
  ((r_tsbind "System.Double" "Parse" string "System.IFormatProvider")
   s flt:format ))

(function flt1:parse (s)
  ((r_tsbind "System.Single" "Parse" string "System.IFormatProvider")
   s flt:format ))

(define flt:abs (r_tsbind "System.Math" "Abs" "System.Double"))

(macro f.op (nm op)
 `(function ,nm (a b)
   (n.asm ( a b )
      (expr a)
      (Unbox ,t_Double)
      (Ldind_R8)
      (expr b)
      (Unbox ,t_Double)
      (Ldind_R8)
      (,op)
      (Box ,t_Double)
      )))

(f.op f+ Add)
(f.op f- Sub)
(f.op f* Mul)
(f.op f/ Div)

;;; TODO:  BUG BUG BUG!
;;; r_tbind fails here!
(function f> (a b)
 (>
  ((r_bind t_Double "CompareTo" t_object) a b)
  0))
(function f= (a b)
 (eq?
  ((r_bind t_Double "CompareTo" t_object) a b)
  0))

(macro f# (str)
   `(n.asm () (Ldc_R8 ,(flt:parse str)) (Box ,t_Double)))

(net.types Convert)

(force-class-flush)

(define f->i (r_tsbind t_Convert "ToInt32" t_Double))
(define i->f (r_tsbind t_Convert "ToDouble" t_Int32))

(define rgen (new "System.Random"))
(define rnext (r_tbind "System.Random" "Next" int))
(function random ()
  (let* ((n (rnext rgen 100000))
         (nf (f- (f/ (i->f n) (f# "50000")) (f# "1"))))
    nf))

(function gauss ()
  (f/ (foldl f+ (f# "0") (formap (i 0 12) (random))) (f# "6")))



