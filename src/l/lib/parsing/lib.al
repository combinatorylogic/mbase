;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ctimex
 (#define packrat-optimised #t))

(ctimex
 (#define packrat-hist nil))

(include "./ast.al")
(include "./util.al")
(include "./ranges.al")
(include "./compiler.al")
(include "./lrengine.al")
(include "./backend.al")

(include "./temporary-source.al")

(macro packrat-file (nm)
 (alet fp (generic-filepath nm)
   `(generic-include ,nm
      ,(alet res (peg:easyparse2 peg_pegparser
                        (peg:file->stream fp))
         (p:match res
           (((FAIL: . $err) . $r) (ccerror `(PEG: ,err ,(__peg:displaypos r))))
           (else (car res)))))))

(macro packrat-top-s (str)
 (alet res (peg:easyparse2 peg_pegparser
                        (peg:str->stream str))
         (p:match res
           (((FAIL: . $err) . $r) (ccerror `(PEG: ,err  ,(__peg:displaypos r))))
           (else (car res)))))

(include "./highlight.al")