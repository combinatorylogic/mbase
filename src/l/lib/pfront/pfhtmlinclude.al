;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define hl-screen-x
  (strreplacers*
   ("\"" "&quot;")
   (" " "&nbsp;")
   ("&" "&amp;")
   ("<" "&lt;")
   (">" "&gt;")
   ("\n" "<br>\n")
   ))

(define hl-docstrings-rev (mkhash))
(define hl-docstrings (mkhash))
(function hl-process-docstring-signal (tp id0 md doc)
  (let* ((id (if id0 id0 (gensym)))
         (chk (ohashget hl-docstrings-rev id))
         (toknid (if chk chk
                     (let* ((tk (gensym)))
                       (ohashput hl-docstrings-rev id tk)
                       (ohashput hl-docstrings tk doc)
                       tk))))
  (foreach (m md)
    (p:match m
      ((useloc (unquote-spec ($fn $sfn $loc $end)))
       (peg:stream-signal sfn 'doctoken loc end `(doctoken ,toknid)))
      (else nil))))

(function hl-screen (str) (strapply* hl-screen-x str))

(define hl-tabs
  (alet ht (mkhash)
    (iter (fmt (id tp v) (ohashput ht id `(,tp ,v)))
       `(
         (ctoken=const p "class='const'" )
         (ctoken=ident p "class='ident'")
         (ctoken=symbol p "class='symbol'")
         (ctoken=keyword p "class='keyword'")
         (ctoken=lexic p "class='lexic'")
         (ctoken=const p "class='const'")
         
         (state=comment p "class='comment'")
         (qstate=quote p "class='const'")
         (state=pattern p "class='pattern'")
         
         (state=empty t ,(fun (x) ""))
         (uberstate=empty t ,(fun (x) ""))

         (cctoken=doubleto t ,(fun (x) "&#8658;"))
         (cctoken=to t ,(fun (x) "&#8594;"))
         (cctoken=lambda t ,(fun (x) "&lambda;"))
         (cctoken=forall t ,(fun (x) "&#8704;"))
         (cctoken=leftset t ,(fun (x) "&#8656;"))
         (cctoken=emptyset t ,(fun (x) "&#8709;"))
         (cctoken=append t ,(fun (x) "&#8853;"))

         (deftoken x ,(fun (x) (fun (v) (S<< "<span id='" x "'>" v "</span>"))))
         (reftoken x ,(fun (x) (fun (v) (S<< "<a href='#" x "'>" v "</a>"))))
         (deftokenloc x ,(fun (x) (fun (v) (S<< "<span id='" x "'><span>" v "</span></span>"))))
         (reftokenloc x ,(fun (x) (fun (v) (S<< "<span class='idfrom' onmouseenter='srcref_over(\"" x "\",this)' onmouseleave='srcref_leave(\"" x "\",this)'><span>" v "</span></span>" ))))

         (doctoken x ,(fun (x)
                        (let* ((docstring (hl-screen (ohashget hl-docstrings x))))
                          (fun (v) (S<< "<span class='withhint'>" v "<span class='hint'>" docstring "</span></span>")))))

         (screen=none s ,I)
         ))
    ht))

(function hl-prepare-state (state)
  (collector (tadd tget)
  (collector (xadd xget)
  (collector (cadd cget)
  (alet sfun (mkref hl-screen)
    (foreach (s state)
      (let* ((f (if (or
                     (eqv? (car s) 'deftoken)
                     (eqv? (car s) 'reftoken)
                     (eqv? (car s) 'deftokenloc)
                     (eqv? (car s) 'reftokenloc)
                     (eqv? (car s) 'doctoken))
                    (ohashget hl-tabs (car s))
                    (ohashget hl-tabs (Sm<< (car s) "=" (cadr s))))))
         (p:match f
           ((s $f) (r! sfun f))
           ((t $f) (tadd f))
           ((x $f) (xadd (f (cadr s))))
           ((p $v) (cadd v)))))
    (list (deref sfun) (tget) (cget) (xget)))))))

(function hl-flush-buffer (file state buffer)
  (let* ((rb (list->string (reverse buffer)))
         (s  (hl-prepare-state state)))
    (format s (sfun ts cs xs)
       (let* ((inner (foldl (fun (v f) (f v)) (sfun rb) ts))
              (colours (foldl (fun (i c) (S<< "<span " c ">" i "</span>")) inner cs))
              (final (foldl (fun (v f) (f v)) colours xs)))
         (fprint file final)))))

(function hl-print-rle-stream (file henv stream endstream)
  (let loop ((p stream)
             (state "") (xstate nil)
             (buffer nil)
             )
    (if (or (null? p)
            (and endstream (eqv? p endstream)))
        ;; END
        (hl-flush-buffer file xstate buffer)
        (let* ((ch (StreamEntry.char p))
               (i (StreamEntry.idx p))
               (n (StreamEntry.chknext p))
               (st (ohashget henv i))
               (stst (if st (to-string st) ""))
               (cv (if (> ch 0) (list (n2s ch)) nil))
               )
          (cond
           ((string=? stst state) ;; No change in state
            (loop n stst st (append cv buffer)))
           (else
            (begin
              (hl-flush-buffer file xstate buffer)
              (loop n stst st cv))))))))

(recfunction ploop1_html (outfile str p fn ifile)
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
            ((pltexstring $cstr)
             (fprint outfile cstr))
            ((plinclude $nm)
             (let* ((fp (generic-filepath nm))
                    (oxpath (corelib:get-lookup-path))
                    (fps (mkref (peg:file->stream fp))))
               (r! **hlevl-file-path** (cons fp (deref **hlevl-file-path**)))
               (if (shashget (getfuncenv) 'debug-display-include-paths)
                   (println (buildstring "include file: " fp)))
               (register-target-dependency fp)
               (alet ret
                 (ifile fps)
                 (r! **hlevl-file-path** (cdr (deref **hlevl-file-path**)))
                 ret)))
            ((plqhexpr $e) nil)
            ((plmetaexpr $e)
             (let* ((str1 (read-int-eval (hlevel-compile-expr e))))
               (fprint outfile str1)))
            ((plqmetaexpr (pfdemo $v $p $e))
             (let* ((str1 (read-int-eval (hlevel-compile-expr v)))
                    (parser (shashget (getfuncenv) (Sm<< "peg_" p)))
                    (outenv1 (mkhash))
                    (env1 (PegEnv.new nil
                                      (make-accept-signal-1 outenv1 pp-defaultmerge)
                                      #t
                                      nil
                                      nil
                                      (mkhash)
                                      ))
                    (str1s (mkref (peg:str->stream str1)))
                    (ret (if parser
                             (peg:easyparse3 env1 parser (deref str1s))
                             nil)))
               (fprint outfile "\n<p><div class='democode'>\n")
               (hl-print-rle-stream outfile
                                 outenv1
                                 (deref str1s)
                                 (cdr ret))
                                 
               (fprint outfile "\n</div><p>\n")
               ))
            (else
             (begin
               (fprint outfile "\n<p><div class='code'>\n")
               (hl-print-rle-stream outfile
                                 outenv
                                 (deref str)
                                 (cdr res))
               (fprint outfile "\n<p></div>\n")
               )))
          (if (peg-alldead? (cdr res)) nil
              (begin
                (r! str (cdr res))
                (ploop1_html outfile str p fn ifile))))
        (begin
          (pfront-report-syntax-error (deref str) res)
          ))))

(recfunction hlevl-consume1-html-inner (outfile dstream)
  (let* ((clect (mkref nil))
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
                    (begin
                      (shashput (getfuncenv) '__peg_feedback_stream dstream)
                      (cc:flush-bypass-from-macro
                       `(top-begin ,@code)))
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
    (ploop1_html outfile
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
                (else nil)))
            (fun (stream1)
              (hlevl-consume1-html-inner outfile stream1)))
    (cget)))

(recfunction hlevl-consume1-html (texnm dstream)
  (let* ((saved-fun (deref pfront:process-docstring-signal))
         (_ (r! pfront:process-docstring-signal hl-process-docstring-signal))
         (outfile (io-open-write (S<< texnm ".html")))
         (ret (hlevl-consume1-html-inner outfile dstream)))
    (io-wclose outfile)
    (r! pfront:process-docstring-signal saved-fun)
    ret))


(macro hlevl-file1-html (texnm nm)
  `(top-begin
     ,@(hlevl-consume1-html
        texnm
        (mkref (peg:file->stream nm)))))

