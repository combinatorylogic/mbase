;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(include "../level1/test.al")
(include "./auto.al")

(bnf-parser
  ((elt tstparse))
  (elt 
    ( (LB tst RB) $1 )
    ( (LB RB)     nil)
    ( (TKN)       $0)) 
  (tst 
    ( (elt tst)  (cons $0 $1) )
    ( (elt)      (list $0) ) ))

(test (car ((tstparse nil) '((LB _) (TKN 1) (LB _) (TKN 2) (TKN 3) (RB _) (TKN 4) (RB _)))) (1 (2 3) 4))

(def:ast tstast ()
  (*TOP* <expr>)
  (expr
   (|
    (VAR <ident:name>)
    (CONST . <cnst:val>)
    (COMMENT cmnt <expr>)
    (OP op <expr> <expr>)))
  (cnst (<any:val> . <type:tp>))
  (ident nm)
  )

(test ((ast:visit tstast expr 
             (expr DEEP 
                 ((VAR (symbol->string (name.> nm)))
                  (CONST (val.> val))
                  (else node)))) '(OP + (OP * (VAR x) (CONST 2))
				        (COMMENT (VAR bbb) (VAR y))))
      (OP + (OP * "x" 2) (COMMENT (VAR bbb) "y")))
      
;; Advanced pattern matching

(let* ((data '( (a b c) (a 1 c) ("qq" (x . y)) (a 20 c) (foo foo bar) 
                (foo bar foo) (foo hokus-pokus-boom)
		(bee 1 2 3 4 5 x a b c)
		))
       (res (foreach-map (d data)
              (p:match d
		( ( $x ($$F:y (fun (v) (and (number? v) (> v 5)))) $z)
		  `(CASE1 ,y))
		( ( $$S:x ($y . y))
		  `(CASE2 ,x ,y))
		( ( $x $$N:y $z)
		  `(CASE3 ,x ,y ,z))
		( ( $a =a $b) `(CASE5 ,a ,b))
		( ( a b $x)
		  `(CASE4 ,x))
		( (bee $$XXX:a x $$XXX:b)
		  `(CASE7 ,a ,b))
		( (foo ($$FF:a (fun (x) 
				 (map string->symbol 
				      (strsplit (<r> "-") (symbol->string x))))
			       hokus pokus  $abc))
		  `(CASE6 ,abc ,a))
		(else `(CASE-E))
		))))
  (test res 
	( (CASE4 c) (CASE3 a 1 c) (CASE2 "qq" x) (CASE1 20) (CASE5 foo bar) 
	  (CASE-E) (CASE6 boom hokus-pokus-boom) 
	  (CASE7 (1 2 3 4 5) (a b c))
	  )
	))

;; List comprehensions tests

(test
  (<L> (* a a) | a <- (fromto 0 11))
  (0 1 4 9 16 25 36 49 64 81 100))

(test
  (length (<L> (cons a b) | a <- (fromto 0 11) & (eq? 0 (% a 2)) | b <- (fromto 0 a)))
  30)

(test
  (<L> `(,x ,y) | x <- '(a b) | y <- '(c d))
  ((a c) (a d) (b c) (b d))
)

(test
  (<L> b | (a . b) <- (czip (fromto 0 10) (reverse (fromto 0 10))))
  (9 8 7 6 5 4 3 2 1 0))

;; I/O

(test
  (to-string '(1 2 (3 . 4)))
  "(1 2 (3 . 4))")

;;
(function ast2test ()
  ((ast:visit tstast expr
       (expr DEEP 
	  (forall (cons (list (this-ast-name) (this-ast-node)) node))))
   '(OP + (OP * (VAR x) (CONST 2)) (CONST 1))))
(test (ast2test) 
  ((tstast expr) OP + 
   ((tstast expr) OP * 
    ((tstast expr) VAR x) ((tstast expr) CONST 2)) 
   ((tstast expr) CONST 1)))

(def:ast asttest2 ()
  (*TOP* <nodex:n>)
  (nodex (<pair:abc> <pair:cde>))
  (pair (a b)))

(function ast3test (v)
  (ast:access:element "asttest2" nodex v 
     ((list
	(+ (abc.> a) (cde.> a))
	(+ (abc.> b) (cde.> b))))))

(test (ast3test '((1 2) (3 4)))
      (4 6))


(define-syntax forX
  (syntax-rules (in as)
    ((forX element in lst body ...)
     (map (lambda (element)
            body ...)
          lst))
    ((forX lst as element body ...)
     (forX element in lst body ...))))

(test (forX '(1 2 3) as i (* i i)) (1 4 9))
(test (forX i in '(1 2 3) (* i i)) (1 4 9))


(define-syntax l2tabc
  (lambda (x)
    (syntax-case x ()
      ((l2tabc ((a b) ...) body ...)
       (syntax
	(quote (a ... b ... (body ...))))))))

(test (l2tabc ((1 2) (3 4) (9 9)) 5 6) (1 3 9 2 4 9 (5 6)))

(define-syntax mylet
  (syntax-rules ()
    ((mylet ((a b) ...) body ...)
     ((lambda (a ...) body ...) b ...))))

(test (mylet ((a 2) (b 3)) (+ a b)) 5)


(define-syntax mytestsyntax
  (syntax-rules ()
    ((mytestsyntax a b)
     (let ((~x a))
       (b ~x)))))

(test (let ((~x 3)) (mytestsyntax 2 (mytestsyntax 0 
					(fun (i) 
					  (fun (a) (+ ~x a))))))
      5)
