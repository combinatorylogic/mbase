;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "../../version.al")
(n.module MBaseML)
(sysdll MBasePackrat)
(macro save-gensym ()
  (let ((gctr (cdr *gensym-counter-storage*)))
    `(set-cdr! *gensym-counter-storage* ,(+ gctr 10001))))

(include "./mllib.al")

(ml-saveenv)

(save-gensym)

