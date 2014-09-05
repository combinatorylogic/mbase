;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(function peg-function-sccval (s)
  (list->symbol (cdr (string->list (cdr s)))))

(function peg-makerfunction-invokeparser (pr) 
  (alet res
    (p:match pr
      ((parser $p $e)
       (alet c (shashget (getfuncenv) (Sm<< "peg_" p "_Context"))
	     (if c (car (ohashget c e)) nil)))
      (else nil))
    (if res res
	(fun (env _lrstk source)
	  (peg-fail)))))

(function peg-function-fixpattern (s)
  (p:match s
    ((binding $i) `(var ,i))
    (else `(ptn ,s))))


(define pf__dllglobcache (mkhash))

(function peg-function-pfcheckdll (nm)
  (alet chk (ohashget pf__dllglobcache nm)
     (if chk nil
	 (try
	  (begin
	    (read-compile-eval `(sysdll ,(Sm<< nm)))
	    (ohashput pf__dllglobcache nm nm))
	  t_Exception 
	  (fun (e) nil)))
     `(pldllref ,nm)
     ))

(packrat-file "./pcommon.peg")
(packrat-file "./minipeg.peg")

;(debugmacro 'peg-constr-compile)
;(force-class-flush)
;(ctimex (define debug-compiler-postlift #t))
;(force-class-flush)

(packrat-file "./pfront.peg")


(packrat-file "./pliter.peg")



