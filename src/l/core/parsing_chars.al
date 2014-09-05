;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Support for a generalised "characters" for parsing.
;

(define *gen-marker* (string->symbol " *g* "))

(function gen? (x)
  (and
   (list? x)
   (not (null? (car x)))
   (eqv? (car x) *gen-marker*)))

(function mkgen (v p)
  (cons *gen-marker* (cons v p)))

(function genvalue (v)
  (if (gen? v) (cadr v) v))

(function genposition (v)
  (if (gen? v) (cddr v) nil))

(function genchar? (x)
  (if (char? x) #t
      (if (and
	   (gen? x)
	   (char? (cadr x)))
	  #t
	  nil)))

(function genchar (x)
  (genvalue x))

(function genascii (x)
  (ascii (genvalue x)))

(function genchar=? (cha b)
  (eq? (ascii cha) (genascii b)))

(function genachar=? (chasc b)
  (= chasc (genascii b)))

(function lazylasttail (lz)
  (let loop ((lst lz))
    (if (null? lst) nil
        (let ((cl (cdr lst)))
          (if (null? (cdr lst))
              lst
              (if (list? (cdr lst))
                  (loop (cdr lst))
                  (loop (cl))
                  ))))))

(function rangeposition (lst)
  (if (null? lst) nil
  (let ((l0 (genposition (car lst))))
    (if l0
	(let* ((ll (lazylasttail lst))
               (l1 (if ll (genposition (car ll)) (list -1 -1))))
	  `(RANGE ,l0 ,l1))
	nil))))

(function genlist->string (lst)
  (if (null? lst) ""
  (let* ((pos (rangeposition lst))
	 (s
	  (list->string (map genchar lst))))
    (if pos (mkgen s pos) s))))

(function genapply (f v)
  (if (gen? v)
      (mkgen (f (genvalue v)) (genposition v))
      (f v)))
	       
(function genlist->symbol (lst)
  (genapply string->symbol (genlist->string lst)))

(function sgenlist->string (lst)
  (list->string (map genchar lst)))

(function sgenlist->symbol (lst)
  (string->symbol (sgenlist->string lst)))

