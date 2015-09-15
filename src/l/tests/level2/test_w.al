;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ENV (cc:newenv))
(cc:env:defmodule ENV "testl2" 'dll)

(cc:toplevel-devour ENV '(include "./test.al"))

