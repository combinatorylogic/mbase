;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
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




