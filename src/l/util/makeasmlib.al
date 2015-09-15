;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "../core/ccerrors.al")
(include "../core/emit.al")
(include "../version.al")
(include "../options.al")

(include "../core/asmlib_common.al")
(include "../core/asmlib_build.al")
(define _asmlib_asm (make-strong-assembly "bootasm" assembly-version assembly-keyfile))
(define _asmlib_module (make-module-s _asmlib_asm "DYNAMIC" "bootasm.dll" #f))

(asmlib:build _asmlib_asm _asmlib_module "bootasm.")

;; yes, save it now:
(asm-save  _asmlib_asm "bootasm.dll")
