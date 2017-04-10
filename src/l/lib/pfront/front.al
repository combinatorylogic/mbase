;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "../../version.al")
(n.module MBaseFront)
(sysdll MBasePackrat)

(force-class-flush)

(unit-tests-use)

(include "./lib.al")


(force-class-flush)

(unit-tests-dump (4 "../src/l/tests/level4/auto.al"))