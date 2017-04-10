;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(n.module pftexprint exe)

(sysdll MBaseFront)

(define ofname (mkref "output"))

(function main ()
  (shashput (getfuncenv) 'main nil)
  (corelib:set-lookup-path (not.neth () (leave
                                         (System.IO.Directory@GetCurrentDirectory))))
  (let loop ((args (a->l *CMDLINE*)))
  (p:match args
    (("/i" $fnm . $rest)
     (read-compile-eval '(n.module temp))
     (read-compile-eval `(hlevl-file ,fnm))
     (loop rest)
     )

    (("/inv" $ofl $fnm . $rest)
     (begin
       (r! ofname ofl)
       (read-compile-eval '(n.module front))
       (read-compile-eval `(hlevl-file1-texinv ,(deref ofname) ,fnm))
       (if rest (loop rest))
     ))

    (($ofl $fnm)
     (begin
       (r! ofname ofl)
       (read-compile-eval '(n.module front))
       (read-compile-eval `(hlevl-file1-tex ,(deref ofname) ,fnm))))

    (($ofl "/c" $exenm $fnm)
     (begin
       (r! ofname ofl)
       (read-compile-eval `(n.module ,(Sm<< exenm) exe))
       (read-compile-eval `(hlevl-file1-tex ,(deref ofname) ,fnm))
       (read-compile-eval `(n.save))))

    (($ofl "/d" $exenm $fnm)
     (begin
       (r! ofname ofl)
       (read-compile-eval `(n.module ,(Sm<< exenm)))
       (read-compile-eval `(hlevl-file1-tex ,(deref ofname) ,fnm))
       (read-compile-eval `(n.save))))

    (else
     (iter println '
        ("Usage:"
         " pfront.exe <outfile> <filename> - execute a file"
         " pfront.exe <outfile> /c <exename> <filename> - compile a file"
         " pfront.exe <outfile> /d <dllname> <filename> - compile a file into a dll"
         )))
    )))