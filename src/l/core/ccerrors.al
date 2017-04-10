;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(function mkexcp (arg)
  (r_raise (new t_MBaseException (object arg)))
  )

(function ccerror (arg)
  "Raises [MBaseException] with a given argument."
  (if (shashget (getfuncenv) 'debug-compiler-failure)
     (begin
       (writeline `(EXCEPTION: ,arg))
       (exit -1)
       )
     (mkexcp arg)))

(define *WARNINGS* (mkref))

(function ccwarning (arg)
  "Adds a warning to the global list of warnings."
   (set-cdr! *WARNINGS* (cons arg *WARNINGS*))
   (print "WARNING: ")
   (println (to-string arg)))

(function getwarnings ()
   "Returns the current list of warnings."
   (cdr *WARNINGS*))



