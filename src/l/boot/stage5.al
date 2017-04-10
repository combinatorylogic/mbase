;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(_Clean_Deps)
(include "../version.al")
(include "../options.al")

(ctimex (println "Bootstrap stage: 5 - final"))
(ctimex (define debug-display-include-paths #t))
;;(ctimex (define debug-compiler-prelift #t))
;;(ctimex (define debug-compiler-dotnet #t))

(define ENV (cc:newenv))
(cc:env:defmodule:strong ENV "MBaseBin" 'dll assembly-version assembly-keyfile)

(include "../core/asmlib_build.al")
(asmlib:build (env:get: ENV dotnet-assembly)
              (env:get: ENV dotnet-module)
              "MBaseBin."
              )

(define stage5-assembly (env:get: ENV dotnet-assembly))
(add-assembly-inner stage5-assembly)

(define generic_pfx "MBaseBin.")
(include "../core/asmlib_load.al")

(set-car! m_Call_Generic m_Call_Generic_00)

(set-car! ms_Call_Generics ms_Call_Generics_00)

(set-car! ms_Call_RevGenerics ms_Call_RevGenerics_00)

(define asmlib-final #t)
(define compiled-environment #t)
(set-cdr! (shashget (getfuncenv) '*gensym-counter-storage*)
          1)
(define *current-cc-env* ENV)
(define stage-final #t)

(unit-tests-use)

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
                             (include "../core/final.al")
                             (include "../boot/extra.al")
                             (include "../core/clibsystem.al")
                             (include "../ext/tcprepl.al")
                             (include "../core/native.al")
                             (include "../ext/unittests.al")

                             ;; Things needed to run pfront programs
                             (include "../lib/pfront/backport.al")

                             ))
  (cc:env:printreport (cc:env:report ENV))
  (cc:dump-module ENV)
  )

(doc.defaults)
(doc.files (def "../doc/clib.tex"))
(doc.flush)

(unit-tests-dump (1 "../src/l/tests/level1/auto.al")
                 (2 "../src/l/tests/level2/auto.al")
                 (3 "../src/l/tests/level3/auto.al"))