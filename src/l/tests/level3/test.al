;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(n.module test exe)
(include "../level2/test.al")
(include "./auto.al")

(test (let loop ((i 0)) (if (< i 800000) (loop (+ i 1)) i))
      800000)

(rec:def tstrecord a b c)

(let ((r (tstrecord.new 1 2 3)) )
   (tstrecord.b! r 100)
   (test (list (tstrecord.a r) (tstrecord.b r) (tstrecord.c r)) (1 100 3)))


;; SXML path

(define xml
 '(*TOP*
    (chapter (@ (num 1))
        (section
          (para "abc")
          (para "cde"))
        (section
          (para "efg")
          (para "ooo")))
    (chapter (@ (num 2))
        (section
          (subsection
             (para "oqq")
             (para "qqo"))))))
 

(test
  ((sxml-path (chapter) (*) para) xml) ;; all the first paragraphs of each chapter's sub-elements
  ((para "abc") (para "efg") (para "oqq"))
)

(test
  ((sxml-path * para) xml) ;; first paragraph
  ((para "abc"))
)

(test
  ((sxml-path chapter (*) (para)) xml) ;; all paragraphs of the first chapter
  ((para "abc") (para "cde") (para "efg") (para "ooo")))

(test
  ((sxml-path (*) subsection para) xml) ;; a paragraph under the first subsection
  ((para "oqq")))


(test (strinterleave 
       (<L> v | (_ v) <- ((sxml-path (*) (para)) xml)) ",")
      "abc,cde,efg,ooo,oqq,qqo")

;; .NET types

(define stk (stack:new))
(begin
  (stack:push stk 'a)
  (stack:push stk 'b)
  (stack:push stk 1)
  (stack:push stk '(x y z))
  (test (list (stack:count stk) (stack:pop stk) (stack:pop stk) (stack:pop stk) (stack:peek stk)
              (stack:peek stk) (stack:count stk))
        (4 (x y z) 1 b a a 1)))

(define als (alist:new:l '(a b c d e)))
(begin
  (alist:add als 1)
  (alist:add als 2)
  (test (n.foreach-map (v als) v)
       (a b c d e 1 2)))

(test (n.foreach-map (l '(1 2 3)) l) (1 2 3))

(define qq (queue:new:l '(1 2 3)))
(begin
  (queue:add qq 4)
  (queue:add qq 5)
  (test (queue:length qq) 5)
  (test (formap (i 0 5) (queue:get qq))
	(1 2 3 4 5)))

;; floating point arithmetics tests

(test (->s (f* (f# "2.2") (f# "1.1"))) "2.42")

;; Not.Net tests

(using (System Meta.Scripting)
  (not.function nntest1 ((int a) (string b))
     (return (concat b ((* a (* a a))@ToString))))
  (test (nntest1 20 "20**3=") "20**3=8000")

  (not.class NNClass1 (extends ValueType)
     (field int X (public))
     (field double Y (public))
     (initfield test ((public) (static)) 
                1 0 0 0
                2 0 0 0
                3 0 0 0
                4 0 0 0
                5 0 0 0
                6 0 0 0
                )
     )

   (test
   (->s
    ((fun ()
      (not.neth ()
		 (lift-field (field int XXX (public) (static)))
                 (lift-field (field (array int) XXZ (public) (static)))
		 (lift-method (method ((public) (static)) int dblit ((int x))
				      (return (* 2 x))))
		 (a = (new NNClass1))
		 (a#X <- 10)
		 (a#Y <- d10.0)
		 (this#XXX <- (this@dblit (a#X)))
                 (this#XXZ <- (mkarr int 6))
                 (System.Runtime.CompilerServices.RuntimeHelpers
                  @ InitializeArray 
                    ((System.Array)(this#XXZ))
                    ((System.RuntimeFieldHandle)(fieldtoken (NNClass1#test))))
		 (leave ((object)(+ ((double)
                                     (aref (this#XXZ) 3)
                                     ) 
                                    (* ((double)(this#XXX)) (a#Y)))))))))
   "204")

  (not.class NNClass2 (extends ValueType)
      (field int X (public))

      (method ((public)) void dbl ()
	  (lift-field (field double Y (public)))
	  (self#X <- (* (self#X) 2))
	  (self#Y <- (* (self#Y) d2.0))
	  (return))
      )

  (not.class NNClass3 
      (method ((public) (static)) string test ()
	 (a = (new NNClass2))
	 (a#X <- 10)
	 (a#Y <- ((double)(a#X)))
	 (a@dbl)
	 (return (concat "<" ((a#X)@ToString) ":"
			 ((a#Y)@ToString "e1") ">"))))

  (test ((r_tsbind C_NNClass3 "test")) "<20:2.0e+001>")

  (lltnet-macro concat2 lst
    `(concat ,@(foreach-map (l lst)
		   (if (string? l) l `(,l @ ToString)))))

  (test ((fun ()
	   (not.neth ()
	     (leave (concat2 "Abc" "def" 1 2 3)))))
	"Abcdef123")

  (test ((fun (lst)
	   (not.neth ((Pair lst))
	       (foreach (l lst)
		  (x = ((Pair)l))
		  (x#car <- (concat ((string)(x#car)) "!"))
		  (x#cdr <- (x#car))
		  )
	       (leave lst)
	       ))
	 '(("a") ("b") ("c")))
	(("a!" . "a!") ("b!" . "b!") ("c!" . "c!")))

  (test (not.neth () (a = (* 2 2)) (leave ((object)a))) 4)

  ;; Method function
  (function mbase-fun1 (x y) (+ x y))
  ;; Field function (closure)
  (function mbase-fun2x (x) (fun (y) (+ x y)))
  (define mbase-fun2 (mbase-fun2x 8))
  ;; Make sure functions are stored
  (force-class-flush)

  ;; Not.Net fallback:
  (test (not.neth () (leave (mbase-fn mbase-fun1 2 2))) 4)
  (test (not.neth () (leave (mbase-fn mbase-fun2 2))) 10)

  ;; Not

)

(mixed-class NNClassM

     (lmethod ((public) (static)) Pair (s2l s2l) ((String p))
              (string->list p))
             
     (lmethod ((public) (static)) Pair test ((string abc))
              (reverse (cdr (s2l abc))))

     (method ((public) (static)) int itest ()
             (str = "Abc, def")
             (a = (this@test str))
             (b = (this@s2l str))
             (i = 0)
             (foreach (aa a) (i <- (+ i 1))) ; gives 7
             (foreach (aa b) (i <- (+ i 1))) ; gives 8
             (return i)
             ))

(test ((r_tsbind C_NNClassM "itest")) 15)

(not.staticdata testdata int 1 2 3 4 5 6)

(test (+ (aget testdata 1) (aget testdata 2)) 5)

;; Nesting
;(ctimex (-bind-em (lltnet-current-module) 
;		  `(class "Nested1" (extends "System.Object")
;			  (class "Nested2

;; threading tests

(module test_thr
        (using threads)

(let* ((pl (thr:mkpool nil))
       (cntr (cons nil 0))
       (mtx (thr:mkmutex))
       (cfun (fun (env) 
	       (thr:mutex_wait mtx) (set-cdr! cntr (+ (cdr cntr) 1))
	       (thr:mutex_release mtx)))
       (qu (thr:mkqueue #t (fun (v) (thr:pool-send pl v)))))
  (for (i 0 55) (thr:pool-add pl))
  (for (i 0 1500) (thr:queue-add qu cfun))
  (thr:queue-kill qu (fun () (thr:pool-kill pl) (test (cdr cntr) 1500))))

)
