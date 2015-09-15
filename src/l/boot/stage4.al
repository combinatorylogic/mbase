;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(_Clean_Deps)

(include "../options.al")

(ctimex (println "Bootstrap stage: 4"))

(define ENV (cc:newenv))
(cc:env:defmodule ENV "boot4" 'dll)

(define compiled-environment #t)
(define compiler-final #t)

(begin
  (cc:toplevel-devour ENV '(top-begin
                             (define compiler-final #t)
                             (define compiled-environment #t)
                             (define core-environment-compiled #t)
                             (include "../boot/boot.al")
                             (include "../boot/initlib.al")
                             (include "../boot/dotnetlib.al")
                             (include "../boot/common.al")
                               ;;;;;

                             (include "../core/compiler.al")
                             (include "../core/unit.al")
                             ))
  (cc:dump-module ENV)
  )
