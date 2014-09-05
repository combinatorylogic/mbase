;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
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

