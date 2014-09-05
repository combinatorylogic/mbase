;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macro alet (name value . body)
  ("Arc--style {\\tt let} construction")
  `(let ((,name ,value)) ,@body))

(unit-test 2 (alet x 2 (* x x)) 4)

(function map-pairs (fn lst)
  (let loop ((l lst))
    (p:match l
      (($a $b . $rest) (cons (fn a b) (loop rest)))
      (() nil)
      (else nil))))

(unit-test 2 (map-pairs + '(1 2 3 4)) (3 7))

(macro awith (namevalues . body)
  ("Arc--style {\tt with} construction")
  `(let ,(map-pairs (fun (a b) (list a b)) namevalues) ,@body))

(unit-test 2 (awith (a 2 b 4 c +) (c a b)) 6)


(macro aif body
  ("Arc--style {\tt if} construction")
  (p:match body
    (($a $b) `(if ,a ,b nil))
    (($a $b $c) `(if ,a ,b ,c))
    (($a $b $c . $rest)
     `(if ,a ,b (aif ,c ,@rest)))))

(unit-test 2 (aif nil 1 nil 2 #t 3) 3)

(macro pipeline> body
  ("Makes a pipeline of one argument functions")
  `(M@ ,@(reverse body)))

(unit-test 2 (alet inc (fun (x) (+ x 1)) ((pipeline> inc inc inc) 3)) 6)
    
(macro mapn (fn . lsts)
  (let* ((nms (map (fun (x) (cons x (gensym))) lsts))
         (len (length lsts))
         (nnms (map cdr nms))
         (inits (foreach-map (n nms)
                  `(,(cdr n) (cons ,(car n) nil))))
         (ars (foreach-map (n nnms)
                `(caar ,n)))
         (updats (foreach-map (n nnms)
                   `(set-car! ,n (cdr (car ,n)))))
         (loopn (gensym)) (resn (gensym)) (rresn (gensym))
         (tmpn (gensym)) (fnn (if (symbol? fn) fn (gensym)))
         )
    `(let* ((,resn (cons nil (cons nil nil)))
            (,rresn (cdr ,resn))
            ,@(if (symbol? fn) nil `((,fnn ,fn)))
            ,@inits)
       (let ,loopn ()
            (let ((,tmpn (cons (,fnn ,@ars) nil)))
              (set-cdr! (cdr ,resn) ,tmpn)
              (set-cdr! ,resn ,tmpn)
              ,@updats
              (if (or ,@(foreach-map (n nnms)
                          `(null? (car ,n))))
                  (cdr ,rresn)
                  (,loopn)))))))
          
(unit-test 2 (mapn + '(1 2 3) '(3 2 1)) (4 4 4))