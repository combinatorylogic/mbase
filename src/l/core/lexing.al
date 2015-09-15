;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Simplifying the common lexers definition: another one meta-layer on top of <r>

; Usage:
; (make-simple-lexer <lexer-name>
;   [ (ident-or-keyword <regexp> <tokenname>) ]
;   [ (keywords/keywords-insensitive <token>*) ]
;   [ (simple-tokens <string> <tokenname> *) ]
;   [ (regexp-tokens <regexp> <tokenname> *) ]
;   [ (ignore <regexp>*) ]
; )
; Any number of keywords, simple-tokens, regexp-tokens sections is allowed

(Section "An easy lexing wrapper.")

(recfunction mkpairs (lst)
  (p:match lst
    (() nil)
    (($a $b . $rest) (cons (list a b) (mkpairs rest)))
    (else (ccerror `(MKPAIRS: ,lst)))))

(function pWhitespace (pars)
  (<r> pars T> (WHITESPACE) I))

(define p.whitespace (ctime `(<r> " " | "\t" | "\n" | ,(mkchar 13))))
(macro make-simple-lexer (name . code)
 ("Makes a simple lexer using the given hints."
  "Available hints are:"
  "[["
  "(ident-or-keyword <regexp> <tokenname>)"
  "           - defines the regexp and token for identifiers"
  "             and keywords."
  "(ident-exceptions <predicate> <tokenname> ...)"
  "(keywords <token>*)     - list of keywords"
  "(keywords-insensitive <token>*)"
  "           - list of case insensitive keywords"
  "(simple-tokens <string> <tokenname> ...)"
  "           - simple string tokens (other than keywords)"
  "(regexp-tokens <regexp> <tokenname> ...)"
  "           - regular expression tokens (constant literals, etc.)"
  "]]"
  )
  (let* ((ident (cons nil 'p.ident)) ; default
         (ident-name (cons nil 'var)) ; default
         (keywords (cons nil nil))
         (keyexs (cons nil nil))
         (ignore (cons nil nil))
         (regexps (cons nil nil)))
    (foreach (c code)
        (fccase c
           ((ident-or-keyword) (rgxp name)
            (set-cdr! ident rgxp)
            (set-cdr! ident-name name))
           ((ident-exceptions) lst
            (set-car! keyexs (mkpairs lst)))
           ((keywords keywords-insensitive) lst
            (set-cdr! keywords (append (cdr keywords) lst)))
           ((regexp-tokens) lst
            (let* ((p (mkpairs lst))
                   (r `(pm<|> ,@(map-over p
                                  (fmt (l r)
                                    `(<r> (,l
                                           T> ,r I)))))))
               (set-cdr! regexps (append (cdr regexps) (list r)))))
           ((simple-tokens) lst
            (let* ((p (mkpairs lst))
                   (r `(pm<|> ,@(map-over p
                                  (fmt (l r)
                                    `(<r> ((,l -> genlist->string)
                                           T> ,r I)))))))
               (set-cdr! regexps (append (cdr regexps) (list r)))))
           ((ignore) lst
            (set-cdr! ignore (cons `(pWhitespace (pm<|> ,@lst))
                                   (cdr ignore))))
           ((ignoretkn) lst
            (let* ((p (mkpairs lst))
                   (r (map-over p
                                (fmt (l r)
                                     `(<r> (,l T> (,r) I))))))
              (set-cdr! ignore (append r (cdr ignore)))))
           ))
   `(define ,name
     (pm<|>
        ,@(if (null? (cdr ignore)) nil `((pm<|> ,@(cdr ignore))))
        ,@(cdr regexps)
        (<r> (,(cdr ident) X->
               (fun (l r)
                 (let* ((s0 (genlist->symbol l))
                        (s (genvalue s0))
                        (sp (genposition s0)))
                   ,(let ((rrr
                           `((p[xTp] (quote ,(cdr ident-name)) I) s r sp)))
                       (if (null? (cdr keywords)) rrr
                          `(case s
                              (,(cdr keywords) ((p[xTp] s I) s r sp))
                              (else
                               ,(if (null? (car keyexs)) rrr
                                    `(cond
                                      ,@(foreach-map (e (car keyexs))
                                          `((,(car e) s)
                                            ((p[xTp] (quote ,(cadr e)) I) s r sp)))
                                      (else ,rrr)
                                      )
                               )))))))))
        ))))

(function debug-lexer (lexer src)
  ("Returns a lexing result or error in a printable format.")
  (try
   (map (fmt (a b) (list a b))
        (p-result ((<r> lexer *) src)))
   t_MBaseException
   (fun (e)
     `(DEBUG-LEXER-ERROR: ,(mbaseerror e)))))