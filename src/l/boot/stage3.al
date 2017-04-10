;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \section{Stage 3 --- compiling the core library $+$ compiler}


(_Clean_Deps)
(include "../options.al")

(ctimex (println "Bootstrap stage: 3"))

(define ENV (cc:newenv))
(cc:env:defmodule ENV "boot3" 'dll)

(define compiled-environment #t)

(begin
  (cc:toplevel-devour ENV '(top-begin
                             (define core-environment-compiled #t)
                             (include "../boot/boot.al")
                             (include "../boot/initlib.al")
                             (include "../boot/dotnetlib.al")
                             (include "../boot/common.al")
                             (include "../core/compiler.al")
                             ))
  )
(cc:dump-module ENV)
