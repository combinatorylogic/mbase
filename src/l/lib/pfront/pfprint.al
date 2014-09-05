;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(n.module pfprint exe)

(sysdll MBaseFront)

(include "./ast.al")

(define print-fmt (mkref 'rtf))
(define outfile-name (mkref nil))

(define header_RTF
  (S<<
   "{\\rtf1\\ansi{\\fonttbl\\f0\\fswiss Courier;"
   "\\f1\\fswiss Symbol;"
   "}{\\colortbl;"
   "\\red0\\green0\\blue0;" ;; 1 black
   "\\red255\\green0\\blue0;" ;; 2 red
   "\\red0\\green155\\blue0;" ;; 3 green
   "\\red100\\green100\\blue100;" ;; 4 gray
   "\\red0\\green0\\blue255;" ;; 5 blue
   "\\red200\\green200\\blue200;" ;; 6 lighgray
   "\\red100\\green0\\blue100;" ;; 7 ???
   "\\red0\\green0\\blue255;" ;; 8 blue
   "}\\f0\\fs24\\pard"))

(define ___TabsX_RTF 
  (strreplacers*
   ("{" "\\{")
   ("}" "\\}")
   ("\\" "\\\\")
   ("\n" "\\par\n")
   ("\t" "    ")
   ))

(function ___Tabs_RTF (str)
  (strapply* ___TabsX_RTF str))

(define footer_TEX "")
(define footer_RTF "}")

(define header_TEX
  (S<<  ""))

(define ___TabsX_TEX
  (strreplacers*
   ("{" "{$\\{$}")
   ("}" "{$\\}$}")
   ("(" "\\lsplp{}")
   (")" "\\lsprp{}")
   ("&" "\\&")
   ("%" "\\%")
   ("_" "\\_")
   ("^" "{$\\hat{~}$}")
   ("$" "\\$")
   ("#" "\\#")
   ("<" "$<$")
   (">" "$>$")
   ("|" "$|$")
   (" " "\\lspspc{}")
   ("\n" "~\\\\\n")
   ("\\" "{$\\backslash$}")
   ("\t" "\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}")))

(function ___Tabs_TEX (str)
  (strapply* ___TabsX_TEX str))



(define pfrontcolours_TEX (mkhash))
(use-hash (pfrontcolours_TEX)
 (iter (fmt (x y) (pfrontcolours_TEX! x y))
   `(
     (ctoken=const "\\tt" )
     (ctoken=ident "\\sl")
     (ctoken=symbol "\\bf")
     (ctoken=keyword "\\tt")
     (ctoken=lexic "\\color{blue}")
     
     (state=comment "\\it")
     (qstate=quote "\\color{cyan}")
     (qstate=unquote "\\color{black}")
     (state=lambda "")
     (state=pattern "\\color{light-gray}")
     (state=constr "")
     
     (f:state=empty ,(fun (x) ""))
     (f:ctoken=doubleto ,(fun (x) "$\\Rightarrow$"))
     (f:ctoken=to ,(fun (x) "$\\rightarrow$"))
     )))



(define *overrides* (mkhash))

(function pp-mymerge (s l)
  (let loop ((i l) (t (car s)))
    (if (null? i) (list s)
        (let* ((ii (car i)))
          (if (eqv? (car ii) t)
              (if (hashget *overrides* t)
                  (cons s (cdr i))
                  i)
              (cons ii (loop (cdr i) t)))))))

(iter (fun (x) (hashput *overrides* x x))
      '(ctoken))

(define pfrontcolours_RTF (mkhash))
(use-hash (pfrontcolours_RTF)
 (iter (fmt (x y) (pfrontcolours_RTF! x y))
   `(
     (ctoken=const "\\cf4" )
     (ctoken=ident "\\i")
     (ctoken=symbol "\\cf2")
     (ctoken=keyword "\\b")
     (ctoken=lexic "\\cf2")
     
     (state=comment "\\cf3")
     (qstate=quote "\\chcbpat6")
     (qstate=unquote "\\chcbpat6\\cf5")
     (state=lambda "\\uldash")
     (state=pattern "\\i")
     (state=constr "\\uld")

     (f:state=empty ,(fun (x) ""))
     (f:ctoken=doubleto ,(fun (x) "{\\field{\\*\\fldinst SYMBOL 222 \\\\f \"Symbol\" \\\\s 12}}"))
     (f:ctoken=to ,(fun (x) "\\u8594\\'3f"))
     )))


(function mergefailure (f1 f2)
  (cond
   ((null? f1) f2)
   ((null? f2) f1)
   (else
    (format f1 ((idx1 . _) . _)
            (format f2 ((idx2 . _) . _)
                    (if (> idx2 idx1) f2 f1))))))

(recfunction ploop1 (outfile str p fn prevfail)
  (let* ((outenv (alist:new))
	 (env (PegEnv.new nil 
			(make-accept-signal-arlist outenv pp-mymerge)
			#t
			nil
			))
	 (res (peg:easyparse3 env p (deref str)))
         (s (p:match (car res)
              ((FAIL: . $_) nil) (else #t)))
	 (___Tabs (case (deref print-fmt)
		    ((tex) ___Tabs_TEX)
		    ((rtf) ___Tabs_RTF)))
	 (pfrontcolours (case (deref print-fmt)
			  ((tex) pfrontcolours_TEX)
			  ((rtf) pfrontcolours_RTF)))
	 )
    (if s
        (begin
          (fn (car res)) 
          (print-rle-stream outfile
                            ___Tabs
                            pfrontcolours
                            outenv
                            (deref str)
                            (cdr res))
          (if (peg-alldead? (cdr res)) nil
              (begin
                (r! str (cdr res))
                (ploop1 outfile str p fn (mergefailure 
                                          prevfail
                                          (peg:reportfailure env))))))
        (begin
          (if (null? prevfail)
              (println (car res))
              (format prevfail ((idx . _) . _)
                (format (car res) (_ (idx2 . _) . _)
                  (if (> idx idx2)
                      (println `(FAIL: ,@prevfail))
                      (println (car res))
                      ))))))))

(recfunction hlevl-consume1 (dstream)
  (let* (
	 (fname (if (not (deref outfile-name))
		    (S<< "output." (deref print-fmt))
		    (deref outfile-name)))
	 (header (case (deref print-fmt)
		   ((tex) header_TEX)
		   ((rtf) header_RTF)))
	 (footer (case (deref print-fmt)
		   ((tex) footer_TEX)
		   ((rtf) footer_RTF)))
	 (outfile (io-open-write fname))
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
                   (cc:flush-bypass-from-macro `(top-begin ,@code)))))
         (cadd (fun (x)
                 (hlevel:iter topexpr x
                    (topexpr _
                       ((topflush (flush))
                        (else (reallyadd x))))))))
    (fprintln outfile header)
    (ploop1 outfile
	    dstream peg_pfront
	    (fun (x)
	      (if x (cadd x)))
            nil
	    )
    (fprintln outfile footer)
    (io-wclose outfile)
    (cget)))

(macro hlevl-file1 (nm)
  `(top-begin
     ,@(hlevl-consume1
        (mkref (peg:file->stream nm)))))

(function main ()
  (shashput (getfuncenv) 'main nil)
  (corelib:set-lookup-path (not.neth () (leave
                                         (System.IO.Directory@GetCurrentDirectory))))
  (let loop ((args (a->l *CMDLINE*)))
   (p:match args
    (($fnm) 
     (begin
       (read-compile-eval '(n.module front))
       (read-compile-eval `(hlevl-file1 ,fnm))))

    (("/c" $exenm $fnm)
     (begin
       (read-compile-eval `(n.module ,(Sm<< exenm) exe))
       (read-compile-eval `(hlevl-file1 ,fnm))
       (read-compile-eval `(n.save))))

    (("/d" $exenm $fnm)
     (begin
       (read-compile-eval `(n.module ,(Sm<< exenm)))
       (read-compile-eval `(hlevl-file1 ,fnm))
       (read-compile-eval `(n.save))))
    (("/tex" . $rest)
     (r! print-fmt 'tex)
     (loop rest))

    (("/rtf" . $rest)
     (r! print-fmt 'rtf)
     (loop rest))

    (("/o" $nm . $rest)
     (r! outfile-name nm)
     (loop rest))

    (else
     (iter println '
        ("Usage:" 
         " pfprint.exe [opts] <filename> - execute a file" 
         " pfprint.exe [opts] /c <exename> <filename> - compile a file" 
         " pfprint.exe [opts] /d <dllname> <filename> - compile a file into a dll"
	 " options are:"
	 "  /tex - use LaTeX format "
	 "  /rtf - use RTF format "
	 "  /o <filename> - write output into a given file"
	 "            the default is 'output.tex' or 'output.rtf'"
         )))
    )))