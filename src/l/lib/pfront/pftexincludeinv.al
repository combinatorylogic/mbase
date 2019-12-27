;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(recfunction ploop1_texinv (outfile str p fn)
  (let* ((outenv (mkhash))
         (env (PegEnv.new nil
                        (make-accept-signal-1 outenv pp-defaultmerge)
                        #t
                        nil
                        nil
                        (mkhash)
                        ))
         (res (peg:easyparse3 env p (deref str)))
         (s (p:match (car res)
              ((FAIL: . $_) nil) (else #t))))
    (if s
        (begin
          (fn (car res))
          (p:match (car res)
            (else
             (begin
               (fprint outfile "\\pfcodeblockbegin{}")
               (print-rle-stream outfile
                                 ___Tabs_tex
                                 __tex_pfrontcolours
                                 outenv
                                 (deref str)
                                 (cdr res)
                                 rle-tex)
               (fprint outfile "\\pfcodeblockend{}")
               )))
          (if (peg-alldead? (cdr res)) nil
              (begin
                (r! str (cdr res))
                (ploop1_texinv outfile str p fn))))
        (begin
          (pfront-report-syntax-error (deref str) res)
          ))))

(recfunction hlevl-consume1-texinv (texnm dstream)
  (let* (
         (outfile (io-open-write (S<< texnm ".tex")))
         (clect (mkref nil))
         (reallyadd (fun (x)
                      (r! clect (cons (hlevel-compile x)
                                      (deref clect)))))
         (cget (fun ()
                 (let ((res (reverse (deref clect))))
                   (r! clect nil)
                   res)))
         (flush (fun ()
                  (alet code (cget)
                   (try
                   (try
                    (cc:flush-bypass-from-macro `(top-begin ,@code))
                    t_MBaseException
                    (fun (e)
                      (writeline `(MBaseException ,(mbaseerror e) in ,@code))))
                   t_Exception
                   (fun (e)
                     (writeline `(Exception ,(->s e) in ,@code)))

                   ))))
         (cadd (fun (x)
                 (hlevel:iter topexpr x
                    (topexpr _
                       ((topflush (flush))
                        (else (reallyadd x))))))))
    (ploop1_texinv outfile
            dstream peg_pfront
            (fun (x) (writeline `(OOO: ,x)) (cadd x)))
    (io-wclose outfile)
    (cget)))

(macro hlevl-file1-texinv (texnm nm)
  `(top-begin
     ,@(hlevl-consume1-texinv
        texnm
        (mkref (peg:file->stream nm)))))

