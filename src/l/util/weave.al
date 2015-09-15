;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntax:
;; Strings starting with ;- are literate comments
;; Strings starting with ;= are in-code literate comments
;; All other strings are code
;;
;; ;{{
;; Omitted code
;; ;}}
;;
;;
(n.module mbweave exe)

(define stdheader
  "\\documentclass{article}
\\usepackage{alltt}
\\usepackage{listings}
\\usepackage{graphicx}
\\usepackage{floatflt}
\\usepackage[usenames]{color}
\\lstloadlanguages{Lisp}
\\lstset{language=Lisp,escapechar=£,commentstyle=\\itshape}
\\def\\NET{\\hbox{\\tt .NET}}
\\def\\Lzero{${\\cal L}_0$}
\\def\\Lone{${\\cal L}_1$}
\\def\\LonePrim{${\\cal L}_1'$}
\\def\\LoneC{${\\cal L}_1^{\\cal C}$}
\\def\\LoneCPrim{${{\\cal L}_1^{\\cal C}}'$}
\\def\\Csharp{\\hbox{C\\#}}

\\newcommand{\\mbfunction}[2]{\\noindent {\\bf Function:}~{\\sl #1}~{#2} \\par}

\\newcommand{\\mbmacro}[2]{{\\noindent \\bf Macro:}~{\\sl #1}~{#2} \\par}
\\newcommand{\\mbdefine}[1]{\\noindent {\\bf Definition:}~{\\sl #1} \\par}

\\newcommand{\\mbsection}[1]{\\subsection{#1}}

\\newcommand{\\fixik}[1]{#1}
\\newcommand{\\lspspc}{~}
\\newcommand{\\lsplp}{{\\color{blue}{(}}}
\\newcommand{\\lsprp}{{\\color{blue}{)}}}
  ")

(define tex-string
  (<r>
     ((_ (p.space *)) (_ ";-") (_ (?? " ")) ((! "\n") *)) -> list->string))

(define tex-string-inner
  (<r>
     ((_ (p.space *)) (_ ";=") (_ (?? " ")) ((! "\n") *)) -> list->string))

(define code-omitted
  (<r>
     (_ ((p.space *) (";{{" ((! ";}}") *) ";}}" ((! "\n") *) "\n")))))

(define empty-line-1 (<r> (p.space +*)))
(define empty-line (<r> (((% " \t")) *) "\n"))

(function not-empty-line? (str)
  (let ((v (empty-line-1 (string->list str))))
    (or (p-fail? v) (not (null? (p-rest v))))))

(define a-number-of-empty-lines
  (<r>
   ((_ (empty-line +*)) -> (fun (_) (list '#\Newline)))))


(define dtax (cons nil nil))
(define plux (cons nil nil))
(define controllers (mkhash))

(function add-some (tpe vlu)
  (if (and (not (null? (cdr dtax)))
           (not (eq? (cdr plux) tpe)))
      (let ((x (reverse (cdr dtax))))
        (unless (null? (filter not-empty-line? x))
                ((hashget controllers (cdr plux)) x))
        (set-cdr! dtax nil)))
  (set-cdr! plux tpe)
  (set-cdr! dtax (cons vlu (cdr dtax))))

(define CR (<r> "\n"))

(define TabsX
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
   (" " "\\lspspc{}")
   ("\n" "~\\\\\n")
   ("\\" "{$\\backslash$}")
   ("\t" "\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}")))


(function Tabs (str)
  (strapply* TabsX str))

(function Coment (str)
  (S<< "{\\sl " str "}"))

(define comentstring
  (<r>
   (
    (((("\"" ((! "\"") *) "\"") | (! ";")) *) -> (M@ wrap Tabs list->string))
    (?? (((";" (_ (";" *)))
          ((! "\n") *))
         ->
         (M@ wrap Coment list->string)))
    (?? ("\n" -> (M@ wrap Tabs list->string))))
   -> (fun (l)
        (foldl string-append "" l))))

(define includestring
  (<r>
     ((_ ((p.space *) "(" (p.space *) "include" (p.space +*) "\""))
      ((! "\"") +*)
      (_ ("\"" (p.space *) ");-I")))
     -> list->string
     ))

(function CodeString (str)
  (p-result (comentstring (string->list str))))

(function get-strings (str)
  (strsplit CR str))

(define tex-process-r
  (<r>
   (
    ((_ "[[") ((! "]]") +*) (_ "]]"))
    ->
    (fun (ll)
      (let ((lx (list->string ll)))
        (if (null? (strmatch* CR lx))
            (begin
              (print "\\verb~")
              (print lx)
              (print "~{}"))
            (begin
              (print "\\begin{verbatim}")
              (print (S<< ">" (strinterleave (get-strings lx) "\n>")))
              (println "\n\\end{verbatim}"))))))))

(define tex-process-x
  (<r>
   (
    (
     ((! "[[") +*) ->
     (fun (x)
       (println (list->string x))))
    |
    tex-process-r)
   *))

(function tex-process (lst)
  (tex-process-x lst))

(hashput controllers 'code
         (fun (lst)
           (println "\n{\\noindent
\\fbox{\\parbox{\\textwidth}{
\\noindent\\raggedright\\tt
")
;           (print "\\begin{verbatim}")
           (iter print lst)
           (println "}}}\\vskip1mm")
;           (println "\\end{verbatim}")
           ))

(hashput controllers 'tex
         (fun (lst)
           (println "")
           (let* ((dd (strinterleave lst "\n")))
             (tex-process (string->list dd)))
           (println "")))

(hashput controllers 'inner
         (fun (lst)
           (print "{\\sl ")
           (let* ((dd (strinterleave lst "\n")))
             (tex-process (string->list dd)))
           (println "}")
           ))

(function print-tex-string (str)
  (add-some 'tex str))

(function print-tex-string-inner (str)
  (add-some 'inner str))

(function print-code-line (str)
  (add-some 'code (CodeString str)))

(define lit-ref (cons nil nil))

(function print-include-file (nm)
   (alet literate (car lit-ref)
       ((<r> literate *) (read-file-list nm))))

(define literate
  (<r>
       (includestring -> print-include-file)
     | (tex-string -> print-tex-string)
     | (tex-string-inner -> print-tex-string-inner)
     | (code-omitted)
     | (((?? a-number-of-empty-lines)
         (((! "\n") *) "\n"))
        -> (M@ print-code-line list->string))))

(set-car! lit-ref literate)

(function processfile (fname)
  ((<r> literate *) (read-file-list fname))
  nil)

(function main ()
  (let loop ((cmd (a->l *CMDLINE*)))
   (p:match cmd
    (("-h" . $rest)
     (println stdheader)
     (loop rest))
    (($fn)
     (try (begin
            (processfile fn)
            (add-some 'end ""))
      t_Exception
          (fun (e)
            (println (S<< "%%% Error while processing file '" fn "'"))
            )))
    (else
     (println "Usage:\n mbweave [-h] <source-file>\n")))))


