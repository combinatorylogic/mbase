;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ___TabsX_tex
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
   ("\t" "\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}")))

(define ___TabsX_ident
  (strreplacers*
   ("alpha" "$\\alpha$")
   ("Alpha" "$\\Alpha$")
   ("beta" "$\\beta$")
   ("Beta" "$\\Beta$")
   ("gamma" "$\\gamma$")
   ("Gamma" "$\\Gamma$")
   ("delta" "$\\delta$")
   ("Delta" "$\\Delta$")
   ("epsilon" "$\\epsilon$")
   ("Epsilon" "$\\Epsilon$")
   ("varepsilon" "$\\varepsilon$")
   ("zeta" "$\\zeta$")
   ("Zeta" "$\\Zeta$")
   ("eta_" "$\\eta$")
   ("Eta_" "$\\Eta$")
   ("theta" "$\\theta$")
   ("Theta" "$\\Theta$")
   ("vartheta" "$\\vartheta$")
   ("iota" "$\\iota$")
   ("kappa" "$\\kappa$")
   ("lambda" "$\\lambda$")
   ("Lambda" "$\\Lambda$")
   ("mu_" "$\\mu$")
   ("nu_" "$\\nu$")
   ("pi_" "$\\pi$")
   ("varpi" "$\\varpi$")
   ("rho_" "$\\rho$")
   ("phi" "$\\varphi$")
   ))

(function ___Tabs_tex (str)
  (strapply* ___TabsX_tex str))

(function texify-ident (str)
  (strapply* ___TabsX_ident (___Tabs_tex str)))

(define __tex_pfrontcolours (mkhash))
(use-hash (__tex_pfrontcolours)
 (iter (fmt (x y) (__tex_pfrontcolours! x y))
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
     (f:uberstate=empty ,(fun (x) ""))
     (f:ctoken=doubleto ,(fun (x) "$\\Rightarrow$"))
     (f:ctoken=to ,(fun (x) "$\\mapsto$"))
     (f:ctoken=lambda ,(fun (x) "$\\lambda$"))
     (f:ctoken=forall ,(fun (x) "$\\forall$"))
     (f:ctoken=leftset ,(fun (x) "$\\Leftarrow$"))
     (f:ctoken=emptyset ,(fun (x) "$\\emptyset$"))
     (f:ctoken=append ,(fun (x) "$\\oplus$"))
     ;; (f:ctoken=ident ,(fun (x) (S<< "{\\sl " (texify-ident x) "}")))
     (f:ctoken=sub ,(fun (x) (S<< "$^{" x "}")))
     )))

(recfunction ploop1_tex (outfile str p fn)
  (let* ((outenv (mkhash))
         (env (PegEnv.new nil
                        (make-accept-signal-1 outenv pp-defaultmerge)
                        #t
                        nil
                        nil
                        ))
         (res (peg:easyparse3 env p (deref str)))
         (s (p:match (car res)
              ((FAIL: . $_) nil) (else #t))))
    (if s
        (begin
          (fn (car res))
          (p:match (car res)
            ((pltexstring $cstr)
             (fprint outfile cstr))
            ((plqhexpr $e) nil)
            (else
             (begin
               (fprint outfile "\\pfcodeblockbegin{}")
               (print-rle-stream outfile
                                 ___Tabs_tex
                                 __tex_pfrontcolours
                                 outenv
                                 (deref str)
                                 (cdr res))
               (fprint outfile "\\pfcodeblockend{}")
               )))
          (if (peg-alldead? (cdr res)) nil
              (begin
                (r! str (cdr res))
                (ploop1_tex outfile str p fn))))
        (begin
          (pfront-report-syntax-error (deref str) res)
          ))))

(recfunction hlevl-consume1-tex (texnm dstream)
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
                      (writeline `(MBaseExecption ,(mbaseerror e) in ,@code))))
                   t_Exception
                   (fun (e)
                     (writeline `(Exception ,(->s e) in ,@code)))

                   ))))
         (cadd (fun (x)
                 (hlevel:iter topexpr x
                    (topexpr _
                       ((topflush (flush))
                        (else (reallyadd x))))))))
    (ploop1_tex outfile
            dstream peg_pliter
            (fun (x)
              (p:match x
                ((pltopexpr $es)
                 (foreach (e es) (cadd e))
                 (flush))
                ((plqhexpr $es)
                 (foreach (e es) (cadd e))
                 (flush))
                ((plqexpr $es) nil)
                (else nil))))
    (io-wclose outfile)
    (cget)))

(macro hlevl-file1-tex (texnm nm)
  `(top-begin
     ,@(hlevl-consume1-tex
        texnm
        (mkref (peg:file->stream nm)))))

