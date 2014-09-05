;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ctimex (include "../../version.al"))
(n.module pfront exe)
(sysdll MBaseFront)

(function __pfront_register-dumpcore (ofil)
  (let ((plugin
	 (fun (core)
	   (fprintln ofil (S<< `(inner.verbatimcore (quote ,(cc:core-sanitise-tokens core)))))
	   core)))
    (cc:add-plugin 'pre-lift plugin)))

(function main ()
  (alet exitfile (mkref nil)
  (shashput (getfuncenv) 'main nil)
  (corelib:set-lookup-path (not.neth () (leave
                                         (System.IO.Directory@GetCurrentDirectory))))
  (let iloop ((ln (a->l *CMDLINE*)))
  (p:match ln
    (("/e" $fnm . $_)
     (begin
       (read-compile-eval `(hlevl-file ,fnm))
       (read-compile-eval `(n.save))))

    (("/c" $exenm $fnm . $_)
     (begin
       (read-int-eval `(n.module ,(Sm<< exenm) exe))
       (read-compile-eval `(hlevl-file ,fnm))
       (read-compile-eval `(n.save))))

    (("/d" $exenm $fnm . $_)
     (begin
       (read-int-eval `(n.module ,(Sm<< exenm)))
       (read-compile-eval `(hlevl-file ,fnm))
       (read-compile-eval `(n.save))))
    
    (("/dbg" . $rest)
     (begin
       (read-int-eval `(define compiler-debug-enabled #t))
       (iloop rest)))

    (("/b" . $rest)
     (begin
       (read-int-eval `(define compiler-debug-failure #t))
       (iloop rest)))

    (("/bench" . $rest)
     (begin
       (r! pfront-benchmark-only #t)
       (iloop rest)))

    (("/dumpal" $fnm . $rest)
     (let* ((ofil (io-open-write (S<< (corelib:get-lookup-path) "/" fnm))))
       (r! pfront-dump-alfile ofil)
       (r! exitfile (cons ofil (deref exitfile)))
       (iloop rest)))

    (("/dumpcore" $fnm . $rest)
     (let* ((ofil (io-open-write (S<< (corelib:get-lookup-path) "/" fnm))))
       (__pfront_register-dumpcore ofil)
       (r! exitfile (cons ofil (deref exitfile)))
       (iloop rest)))

    (("/hist" . $rest)
     (begin
       (r! pfront-mkhist #t)
       (iloop rest)))

    (($fnm . $_) 
     (begin
       (read-int-eval '(n.module frontrepl dll))
       (read-compile-eval `(hlevl-file ,fnm))))

    (else
     (iter println '
        ("Usage:" 
         " pfront [options] <filename>              : execute a file" 
         " pfront [options] /c <exename> <filename> : compile a file" 
         " pfront [options] /d <dllname> <filename> : compile a file into a dll"
	 ""
	 "Options are:"
	 "   /dbg    : emit debugging info"
	 "   /b      : exit on a first error"
	 "   /dumpal <filename>  : dump the file as .al"
         ))))
    )
    (foreach (f (deref exitfile))
	(io-wclose f))
  ))
