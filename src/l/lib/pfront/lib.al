;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(include "./codegen.al")

(net.types Environment GC)
(define pfront_time_now
   (let* ((tproc (sdotnet "System.Diagnostics.Process"))
	  (curproc (fun () ((r_sbind tproc "GetCurrentProcess")))))
     (fun ()
       ((r_bind "System.TimeSpan" "get_TotalMilliseconds")
	((r_bind tproc "get_UserProcessorTime")
	 (curproc))))))

(define pfront-mkhist (mkref nil))
(define pfront-dump-alfile (mkref nil))
(define pfront-benchmark-only (mkref nil))

(force-class-flush)

(define **hlevl-file-path** (mkref nil))

(function __peg-function-makeloc (saved source)
  (if (shashget (getfuncenv) 'compiler-debug-enabled)
      (alet chk (deref **hlevl-file-path**)
	    (if chk 
		`((loc (,(car chk) ,saved ,source)))
		nil))))

(function __peg-function-makelocbegin (e saved source)
  (let ((lc (__peg-function-makeloc saved source)))
    (if lc `(flatbegin ,@lc ,e) e)))

(macro peg-function-makeloc ()
  `(__peg-function-makeloc (StreamEntry.idx saved) (StreamEntry.idx (deref source))))

(macro peg-function-makelocbegin (e)
  `(__peg-function-makelocbegin ,e (StreamEntry.idx saved) (StreamEntry.idx (deref source))))

(force-class-flush)

(include "./parser.al")
(force-class-flush)

(recfunction ploop (env str p fn)
  (let* ((res (peg:easyparse3 env p (deref str)))
         (s (p:match (car res)
              ((FAIL: . $_) nil) (else #t))))
    (if s
        (begin
          (fn (car res))
          (if (peg-alldead? (cdr res)) nil
              (begin
                (r! str (cdr res))
                (ploop env str p fn))))
        (begin
          (println (car res))))))


(function hlevl-consume (parser dstream rfun)
  (let* ((clect (mkref nil))
         (reallyadd (fun (x)
		      (let ((cx 
			     (if (deref pfront-benchmark-only)
				 nil
				 (hlevel-compile x))))
			(if (deref pfront-dump-alfile)
			    (fprintln (deref pfront-dump-alfile) 
				      (S<< cx)))
			(r! clect (cons cx
					(deref clect))))))
         (cget (fun ()
                 (let ((res (reverse (deref clect))))
                   (r! clect nil)
                   res)))
         (flush (fun ()
                  (alet code (cget)
		   (if (not (deref pfront-benchmark-only))
		       (cc:flush-bypass-from-macro `(top-begin ,@code))))))
         (cadd (fun (x)
                 (hlevel:iter topexpr x
                    (topexpr _
                       ((topflush (flush))
                        (else (reallyadd x)))))))
         (xfun (rfun cadd flush)))
    (ploop (peg:makeenv) dstream parser xfun)
    
    (cget)))

(macro hlevl-file (nm)
  (let* ((fp (generic-filepath nm))
         (oxpath (corelib:get-lookup-path))
	 (file-length (mkref 0))
	 (fps (mkref (peg:file->stream2 fp file-length)))
	 (fph (if (deref pfront-mkhist) (deref fps)))
	 (begtime (pfront_time_now))
	 )
   (r! **hlevl-file-path** (cons fp (deref **hlevl-file-path**)))
   (if (shashget (getfuncenv) 'debug-display-include-paths)
       (println (buildstring "include file: " fp)))
   (register-target-dependency fp)
   (alet ret
     `(top-begin
	(ctimex (corelib:set-lookup-path ,(_getpath fp)))
	,@(begin
	    (corelib:set-lookup-path (_getpath fp))
	    (hlevl-consume peg_pfront
			   fps
			   (fun (cadd flush)
			     (fun (x) (if x (cadd x))))
			   ))
	(ctimex (corelib:set-lookup-path ,oxpath))
	)
     (r! **hlevl-file-path** (cdr (deref **hlevl-file-path**)))
     (if (deref pfront-mkhist)
	 (iter println (__peg:stream-hist fph)))
     (let* ((endtime (pfront_time_now))
	    (time (f- endtime begtime))
	    (speed (f/ (f* (f# "1000") (f/  (i->f (deref file-length)) (f# "1024.0"))) time)))
       (if (deref pfront-benchmark-only)
	   (println (S<< "PFront parsing: " speed " kb/sec"))))
     (return ret)
     )))

(include "./pftexinclude.al")
(include "./pftexincludeinv.al")

(macro hlevl-lfile-inner (tp texnm nm)
 (let* ((fp (generic-filepath nm))
	(texpath (generic-filepath texnm))
        (oxpath (corelib:get-lookup-path)))
   (r! **hlevl-file-path** (cons fp (deref **hlevl-file-path**)))
   (if (shashget (getfuncenv) 'debug-display-include-paths)
       (println (buildstring "include file: " fp)))
   (register-target-dependency fp)
   (alet ret
     `(top-begin
	(ctimex (corelib:set-lookup-path ,(_getpath fp)))
	,@(begin
	    (corelib:set-lookup-path (_getpath fp))
	    ((if (eqv? tp 'literate) hlevl-consume1-tex
                                     hlevl-consume1-texinv)
	                   texpath
			   (mkref (peg:file->stream fp)))
	    )
	(ctimex (corelib:set-lookup-path ,oxpath))
	)
     (r! **hlevl-file-path** (cdr (deref **hlevl-file-path**)))
     (return ret)
     )))

(macro hlevl-lfile (texnm nm) `(hlevl-lfile-inner literate ,texnm ,nm))
(macro hlevl-lfile-inv (texnm nm) `(hlevl-lfile-inner inv ,texnm ,nm))

(macro inplace (code)
  `(ctime (hlevel-compile-expr ,code)))

(function applyfunction (fn args)
  (apply (shashget (getfuncenv) fn) args))

(function string_to_symbol (s) (string->symbol s))

(function pfront-include (fname)
  (if (shashget (getfuncenv) 'debug-display-include-paths)
      (println (S<< "pfront include: " fname)))
  (read-compile-eval `(hlevl-file ,fname)))

(function pfront-eval (stream)
  (read-compile-eval `(top-begin ,@(hlevl-consume peg_pfront (mkref stream)
                                                  (fun (cadd flush)
                                                    (fun (x) (if x (cadd x))))
                                                  )))
  )

(function pfront-eval-string (str)
  (pfront-eval (peg:str->stream str)))

(macro pfront-expand-string (str)
  (alet stream (peg:str->stream str)
  `(begin ,@(hlevl-consume peg_pfront (mkref stream)
			       (fun (cadd flush)
				 (fun (x) (if x (cadd x))))
			       ))))


(force-class-flush)

(hlevl-file "./notnet.hl")

(function peg-checkfunction-mbaseglobal (str)
  (if (shashget (getfuncenv) (Sm<< str)) nil (peg-fail))
  )

(function peg-checkfunction-mbasemacro (str)
  (if (hashget-seq (getcurmacroenv) (Sm<< str)) nil (peg-fail))
  )

(hlevl-file "./sexp.hl")

(force-class-flush)

(hlevl-file "./extensions.hl")

(force-class-flush)

(not.class Meta.Scripting.PFront
 (method ((static) (public)) void include ((string fname))
         (mbase-fn pfront-include fname)
         (return))
 (method ((static) (public)) object eval ((string code))
	 (return (mbase-fn pfront-eval-string code)))
 )

(unit-test 4 (pfront-expand-string "2*2+1") 5)
(unit-test 4 (pfront-expand-string "{a=2;b=3;return a*a+b*b;}") 13)
(unit-test 4 (pfront-expand-string "map a in fromto(1,4) do a*a") (1 4 9))



