;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
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

