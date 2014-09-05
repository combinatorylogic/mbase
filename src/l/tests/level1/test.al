;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "../common.al")

(include "./auto.al")

(test (+ 2 2) 4)
(test (filter (fun (x) (eq? 0 (% x 2))) (formap (i 0 10) i) ) (0 2 4 6 8))
(test (map-over (fromto 1 5) (fun (x) (+ x (* x 2))))
      (3 6 9 12))
(test (reverse '(1 2 3 4)) (4 3 2 1))
(test (p-result ((<r> ("a" *) -> ::) (string->list "aaaabbbb")))
      ("aaaa"))
(test (map S->N (strsplit (<r> ",") "1,2,3,4,5,6"))
      (1 2 3 4 5 6))
(test ((M@ list->string reverse string->list) "123")    
      "321")
(test (strreplace* (<r> "[" + (p.alpha +*) + "]") "<>" "[abc]1[def]2[][x]3")
      "<>1<>2[]<>3")
(test (strmatch* (<r> (p.alpha +*) -> ::) "abc def xxx,yyy,z")
      ("abc" "def" "xxx" "yyy" "z"))
(test (strmatch* (<r> (p.alpha +*) :-> (@ string->symbol list->string)) "abc def xxx,yyy,z")
      (abc def xxx yyy z))


(test (car (reverse (formap (i 1 1001) i))) 1000)
(test (foldl + 0 (fromto 1 11)) 55)
(test (foldl * 1 (fromto 1 11)) 3628800)

(test (foldl string-append "" '("a" "b" "c")) "abc")

(test (flatten '(1 (2 (3 4) 5 (6))))
      (1 2 3 4 5 6))

(test (pre-emit '(((i1) (((i2))) (i3))))
      ((i1) (i2) (i3)))

(test ((lambda (x y) (y x))
       2
       (lambda (x) (* x x)))
      4)

(test ((:Y1 (lambda (Ylength)
               (lambda (l)
                  (if (null? l) 0
                      (+ 1 (Ylength (cdr l)))))))
       '(1 2 3 4 5))
      5)

(test (format '(((1) 2) . 3) (((x) y) . z) (list x y z) )
      (1 2 3))

(test (letf (( (a (b . c)) '(1 (2 . 3)))) (+ a (+ b c)))
      6)

(test (map (fmt (a . b) (+ a b)) '((1 . 2) (3 . 4) (5 . 6)))
      (3 7 11))

(test (fccase '(a 1 2) ((a) (x y) (+ x y)) (else 0))
      3)

(test (let ((a (cons 0 1))) (for (i 0 100) (set-cdr! a (+ (cdr a) i))) (cdr a))
      4951)

(test (let ((a (cons 0 1))) (set-cdr! a a) (first 5 a)) 
      (0 0 0 0 0))

(test (postcompile nil '((lambda (x) (lambda (y) (+ x y))) 2))
      ((lambda (1) (lambda (1 (0 arg)) (corelib:+ (env-ref 0) (arg-ref 0)))) 2) )

(test ((r_tbind "System.Object" "ToString") ((r_tsbind "System.Math" "Sqrt" "System.Double") ((r_tsbind "System.Double" "Parse" string) "0.25")))
      "0.5")

(test (/-> '(x y) "car") x)

(test (/-> '(1 . 2) "cdr") 2)
(test (let ((a '(1 . 2))) (<-: a "car" 'x) (car a)) x)
(test (let ((x 0)) (try (/ 1 x) t_exception (fun (x) 'exception))) exception)
(test (new t_Pair (t_object "1") (t_object "2")) ("1" . "2"))
(test (map-over (fromto 1 12)
                (fun (x) (foldl (cut * <> <>) 1 (fromto 1 (+ 1 x)))))
 (1 2 6 24
  120 720 5040 40320 362880 3628800 39916800))

(test (do (+ a b) (where (a 2) (b 2))) 4)

;; Lexical macros

(test
 (let ((a (fun (x y) (list 'Y x y))))
   (append
    (with-macros
     ((a (fmt (_ . x) `(quote (XXX ,x))))
      (b (fmt (_ x y) `(,x ,y ,y))))
     (b list (a a a)))
    (list (a 2 2))))
 ((XXX (a a)) (XXX (a a)) (Y 2 2)))


(test (iso '(a (b c . 1) 2)
           (cons 'a (cons (cons 'b (cons 'c 1))
                          (cons 2 nil))))
      #t)

(test (iso '(a b c)
           '(a b b))
      ())


(module test1 (export test1f)
        (function test10 (x) (* x x))
        (function test1f (x) (test10 (+ x 1))))

(module test2
        (using test1)

        (test (test1f 1) 4))

(module test3
        (using test1)

        (macro checkglobal (id)
          (list 'quote (core:lookup-global id)))
        (test (checkglobal test10) test10)
        (test (checkglobal test1f) test1:test1f)
        )