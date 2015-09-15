;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Documenting and testsuite generation macros.
;  May be used as an intermediate target for normal 'function' and 'macro'.

(define *documentation-file* nil)
(define *documentation-plugin* nil)

(define doc.texsubst
  (strreplacers*
    ("{" "{$\{$}")
    ("}" "{$\}$}")
    ("&" "{\\tt \\&}")
    ("|" "{\\tt |}")
    ("<" "{\\tt <}")
    (">" "{\\tt >}")
    ("_" "{\\tt \\_}")
    ("$" "{\\tt \\$}")
    ("#" "{\\tt \\#}")
    ))

(function doc.texnic (str)
   (strapply* doc.texsubst str))

(define doc.sbody
  (strreplacers*R
   ("\\[" -> (fun (_) "["))
   ("\\]" -> (fun (_) "]"))
   ("[br]" -> (fun (_) "\\\\"))
   (("[[" ((! "]]") *) "]]")
    ->
    (fun (str)
         (S<< "\\begin{verbatim}"
              ((M@ list->string cuttail cuttail cdr cdr string->list) str)
              "\\end{verbatim}")))
   (("[" ((! "]") *) "]")
    ->
    (fun (str)
         (S<< "\\verb;"
              ((M@ list->string cuttail cdr string->list) str)
              ";{}")))
   ))

(function doc.texbody (str)
  (strapply* doc.sbody str))

(function doc.write (channel rest)
  (let ((docfile (#eval '*documentation-file*)))
    (if (not (null? docfile))
      (if (eq? channel 'close) (docfile 'CLOSE nil)

       (let* ((docplugin (#eval '*documentation-plugin*))
              (docstrings (docplugin rest)))
          (docfile channel docstrings))))
    nil
    ))


; *documentation-file*: function performing the output of the
;     given lines list into the given channel
; *documentation-plugin*: formatting function

;; Default formatting function:
(function ap.s (l) (foldl (fun (a b) (S<< a  b "\n")) "\n" l))

(function doc.index ( title filt stor )
   (let ((rslt (qsort string<? (filt stor))))
     (if (null? rslt) ""
      (S<< title "% "
        (strinterleave
           rslt
           ", ")
        "\n"
     ))))


(function doc.funcfilt ( lst )
  (map
     (fmt (_ x a)
         (S<< x "(" (strinterleave a ",") ")"))
     ((cdr lst) (fun (x) (p:match x ((function . $_) #t) (else nil))))))

(function doc.macrfilt ( lst )
  (map
     (fmt (_ x a)
         (S<< x "/" (to-string a) "/"))
     ((cdr lst) (fun (x) (p:match x ((macro . $_) #t) (else nil))))))

(function doc.deffilt ( lst )
  (map
     (fmt (_ x)  (->s x))
     ((cdr lst) (fun (x) (p:match x ((define . $_) #t) (else nil))))))

(function doc.texify (str) ;;TODO!
  (doc.texnic str)
  )

(function doc.mkdefplugin ( stor )
  (fun (l)
   (p:match l
      ((function $name $args . $text)
       ((car stor) `(function ,name ,args))
       (S<< "\\mbfunction{" (doc.texify name) "}{" (doc.texify (to-string args)) "}\n\n{"
            (doc.texbody (ap.s text)) "}\n\n"))
      ((macro $name $args . $text)
       ((car stor) `(macro ,name ,args))
       (S<< "\\mbmacro{" (doc.texify name) "}{" (doc.texify (to-string args)) "}\n\n{" (doc.texbody (ap.s text)) "}\n\n"))
      ((define $name $text)
       ((car stor) `(define ,name))
       (S<< "\\mbdefine{" (doc.texify name) "}\n\n{" (doc.texify text) "}\n\n"))
      ((par . $text)
       (S<< "\n\n{" (doc.texbody (ap.s text)) "}\n\n"))
      ((section $title . $text)
       (S<< "\\mbsection{" title "}{" (ap.s text) "}\n\n"))
      ((index.functions $title)
       (doc.index (S<< "% Functions " title "\n") doc.funcfilt stor))
      ((index.macros $title)
       (doc.index (S<< "% Macros " title "\n") doc.macrfilt stor))
      ((index.defines $title)
       (doc.index (S<< "% Defines " title "\n") doc.deffilt stor))
      (else (writeline `(LLL: ,l))
            ""))))

(function doc.mkstor ()
   (let* ((lst (cons nil nil))
          (add (fun (v) (set-cdr! lst (cons v (cdr lst)))))
          (fetch (fun (f)
                    (let ((ll (tailsplit f (cdr lst))))
                       (set-cdr! lst (cdr ll))
                       (reverse (car ll))))))
      (cons add fetch)))


(function doc.mkoutput (channels)
   (let ((hs (mkhash)))
     (iter-over channels
         (fmt (ch fl)
            (let ((out (io-open-write fl)))
               (hashput hs ch out))))
     (fun (ch strs)
       (let ((c (any->string ch)))
         (if (eq? c "CLOSE")
             (let ((ll (hashmap (fun (k _) k) hs)))
               (foreach (k ll)
                  (let ((o (hashget hs k)))
                    (io-wclose o)
                    (hashput hs k nil)))
               nil)
             (let ((fo (hashget hs c)))
                (if (null? fo) nil
                 (cond
                  ((list? strs)
                    (foreach (s strs) (fprint fo s)))
                  (else (fprint fo strs)))
                  ;; TODO: flush it
                  )))))))


(macro doc.files channels
  `(ctimex (define *documentation-file* (doc.mkoutput (quote ,channels)))))

(macro doc.defaults rest
  `(ctimex (define *documentation-plugin* (doc.mkdefplugin (doc.mkstor)))))

(macro doc.flush _
  `(ctimex (begin
              (foreach (i (reverse (hashget *documentation* 'D)))
                  (doc.write (car i) (cadr i)))
              (hashput *documentation* 'D nil)
              (doc.write 'def '(index.functions ""))
              (doc.write 'def '(index.macros ""))
              (doc.write 'def '(index.defines ""))
              (doc.write 'close nil))))

