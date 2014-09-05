;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "./peg.al")

(function peg-translate (out sources)
  (call-with-output-file out
    (fun (fo)
     (foreach (s sources)
       (alet x (lex-and-parse peg-lexer parse-peg-decls
                              (read-file-list 
                               (S<< (corelib:get-lookup-path) "/" s)))
           (foreach (xi x) (fprintln fo (to-string xi)))
           )))))


(peg-translate "./temporary-source.al" '("./basics.peg" "./pegbasics.peg" "./peg.peg"))




