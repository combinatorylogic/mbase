;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(n.module mlcomp exe)
(sysdll MBaseML)

(function compile-ml (src tgt)
  (read-compile-eval `(n.module ,(Sm<< tgt) exe))
  (read-compile-eval `(ml-file ,src))
  (read-compile-eval '(n.save)))

(function main ( )
  (shashput (getfuncenv) 'main nil)
  (p:match (a->l *CMDLINE*)
    (($src)
     (compile-ml src "mlout"))
    (($src $tgt)
     (compile-ml src tgt))
    (else
     (println "Usage:\n mlcomp <source.ml> [<outfile>]\n"))))

