;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
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
    ((mdbinding $i) i)
    (else `(ptn ,s))))


(define pf__dllglobcache (mkhash))
(define pf_checkdll_external (mkref nil))

(function peg-function-pfcheckdll (nm)
  (let* ((c0 (deref pf_checkdll_external)))
    (if (not c0)
        (alet chk (hashget pf__dllglobcache nm)
              (if chk nil
                  (try
                   (begin
                     (read-compile-eval `(usedll ,(Sm<< nm)))
                     (hashput pf__dllglobcache nm nm))
                   t_Exception
                   (fun (e) nil)))
              `(pldllref ,nm)
              )
        (c0 nm))))

(function peg-function-pfchecksysdll (nm)
        (alet chk (hashget pf__dllglobcache nm)
              (if chk nil
                  (try
                   (begin
                     (read-compile-eval `(sysdll ,(Sm<< nm)))
                     (hashput pf__dllglobcache nm nm))
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




