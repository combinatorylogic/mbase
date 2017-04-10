;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;-
;- \subsection{S--expressions ML frontend}
;-
;-
;- Since our implementation is embeddable, having an intermediate list--based
;- representation could be quite useful for implementing higher level languages
;- on top of ML.
;-

(function mml-constr? (sym)
  (matches? p.mlcapident (symbol->string sym)))

(function mml->ml.pattern (ptrn)
  (let loop ((p ptrn))
    (p:match p
      ((% . $rest) `(tuple ,@(map loop rest)))
      ($$M:v (if (eqv? v '_) '(any)
                 `(bind ,v (any))))
      (($$M:s . $rest) `(constr ,s ,@(map loop rest)))
      ($$S:s `(string ,s))
      ($$N:n `(number ,n))
      (($$F:b (fun (x) (eq? x #t))) `(bool #t))
      (($$F:b (fun (x) (eq? x #f))) `(bool #f)))))

(function mml->ml.expr (expr)
  (let loop ((e expr))
    (p:match e
      ((let rec $nm $vl $body)
       `(letrec ,nm ,(loop vl) ,(loop body)))
      ((let $nm $vl $body)
       `(let ,nm ,(loop vl) ,(loop body)))
      ((fun $args $body)
       (ml-curry-function args (loop body)))
      ((begin . $body)
       `(begin ,@(map loop body)))
      ((match $v with . $pattern)
       `(match ,(loop v) ,@(map-over pattern
                              (fmt (p e)
                                 (list (mml->ml.pattern p)
                                       (loop e))))))
      ((% . $rest)
       `(tuple ,@(map loop rest)))
      (($$M:s . $rest)
       (if (mml-constr? s)
           `(constr ,s ,@(map loop rest))
           (mlapply `(var ,s) (map loop rest))))
      (($e . $rest)
       (mlapply (loop e) (map loop rest)))
      ($$M:s `(var ,s))
      ($$S:s `(string ,s))
      ($$N:n `(number ,n))
      (($$F:b (fun (x) (eq? x #t))) `(bool #t))
      (($$F:b (fun (x) (eq? x #f))) `(bool #f))
      (else (ccerror `(MML:EXPRESSION ,e))))))

(function to-string-i (s)
  (innerlist outerlist 1 s))

(function mml->ml.type (tdef)
  (lex-and-parse mllexer mltype (to-string-i tdef)))

(function mml->ml.typedefs (tdef)
  (lex-and-parse mllexer mltypedefs (to-string-i tdef)))

(recfunction mml->ml (add src)
  (p:match src
    ((type . $typedefs)
     (add `(mltype ,@(mml->ml.typedefs typedefs))))
    ((let rec $nm $expr)
     (add `(mlletrec ,nm ,(mml->ml.expr expr))))
    ((let $nm $expr)
     (add `(mllet ,nm ,(mml->ml.expr expr))))
    ((foreign $nm $type $fnm)
     (add `(mlffi ,nm ,(mml->ml.type type) ,fnm)))
    ((mlinclude $$S:fname)
     (add `(mlinclude ,fname)))
    ((mbinclude $$S:fname)
     (add `(mbinclude ,fname)))
    ((val $nm $type)
     (add `(mlannotate ,nm ,(mml->ml.type type))))
    ($expr
     (add `(mlexpr ,(mml->ml.expr expr))))))

(macro ml$ rest
  (try
   (let* ((src1 (collector (add get)
                   (iter (cut mml->ml add <>) rest)
                   (get)))
          (res (ml-driver ml:env src1)))
     `(top-begin
        ,@res))
   t_MBaseException
   (fun (e)
     (writeline `(TOPLEVEL-ERROR: ,(mbaseerror e)))
     `(top-begin ))))
