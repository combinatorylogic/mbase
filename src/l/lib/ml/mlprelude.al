;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(function mlprint (x) (print (ml-pprint-value x)))

(function terpri (x) (println ""))

(function mlmkhash (x) (mkhash))

(function mlstringtolist (str)
  (let loop ((l (string->list str)))
    (p:match l
      (($a . $b) `(Cons ,a ,(loop b)))
      (() `(Nil)))))

(function mllisttostring (lst)
  (list->string
   (let loop ((l lst))
     (p:match l
       ((Cons $a $b) `(,a ,@(loop b)))
       ((Nil) nil)))))

(function ml-setcar! (l c)
  (set-car! (cdr l) c))

(function ml-setcdr! (l c)
  (set-car! (cddr l) c))

(function ml-mkref (v) (cons v nil))

(function ml-deref (v) (car v))
(function ml-setref (v n) (set-car! v n))


(function lispclass (v)
  (p:match v
    (() '(FNull))
    ($$N '(FNumber))
    ($$S '(FString))
    ($$M '(FSymbol))
    ($$L '(FList))
    (else
     (if (char? v) '(FChar) '(FOther)))))

(function ml-lispnull (_) nil)
             

