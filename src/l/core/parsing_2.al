;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auxillary parsing functions
; parse an integer number (atoi)
(function S->N (str)
  "Converts a string into an integer number."
  (letf ((xx (string->list str))
         ((zz . vv)
          (cond
           ((eq? (car xx) #\-)
            (cons -1 (cdr xx)))
           ((eq? (car xx) #\+)
            (cons 1 (cdr xx)))
           (else (cons 1 xx)))))
    (* zz
      (cdr (foldl (fun (x v)
                   (format x (a . b)
                     (let ((vv (- (ascii v) (c# #\0))))
                       (cons (* a 10) (+ b (* vv a))))))
                  '(1 . 0)
                  (reverse vv))))))

;;;;;;;;;;;;;;;;;;;;;;
; Probably not so useful, but at least self-documenting stuff:
(define p.integer
  "Recognises an integer decimal number."
   (<r> (?? (#\- | #\+)) (p.digit +*)))

(define p.integer.p
  "Recognises an integer decimal number and returns an integer."
  (p<R> p.integer (M@ S->N sgenlist->string)))

(define p.ident
  "Recognises an MBase symbol."
   (<r> p.alpha ((p.alpha | p.digit | (% ".,:#-+*/[]$%^&!|@_")) *)))

(define p.ident.p
  "Recognises an MBase symbol, returns a symbol."
  (p<R> p.ident sgenlist->symbol))


(function strsplit (pp str)
  "Splits a string using a given delimiter regular expression."
  (p-result ((<r> (((((! pp) *) -> ::) (_ pp)) *) (?? (((! pp) +*) -> ::)))
              (string->list str))))

(function strmatch* (pp str)
  "Returns a list of all matches of [pp] in [str]."
  (p-result ((<r> ((_ ((! pp) *)) pp) *) (string->list str))))

(function p-result? (r)
  (if (p-fail? r) nil (p-result r)))

;; pp result would be ignored
(function strreplace* (pp rstr str)
  "Returns [str] with all occurences of [pp] replaced with [rstr]."
  (p-result
    ((<r> (((((! pp) *) (pp -> (fun (_) (string->list rstr)))) *) (p. *)  )
          -> list->string)
     (string->list str))))


(function strmkreplacer* (pp)
  "Makes a prepared replacer regular expression out of [pp]."
   (<r> ((((! pp) * ) pp) *) (p. *)))

(macro strreplacers* rest
  ("Makes a prepared replacer for a given list of"
   "pairs of regular expressions and replacement strings.")
   `(strmkreplacer*
     (pm<|>
      ,@(map-over rest
          (fmt (str rep)
            `(<r> ,(any->string str) ->
                  (fun (_) (list ,@(string->list (any->string rep))))))))))

(macro strreplacers*R rest
  ("Makes a prepared replacer for a given list of"
   "pairs of regular expressions and processing functions.")
    `(strmkreplacer*
       (pm<|>
          ,@(map-over rest
              (fmt (rgxp -> rep)
                `(<r> ,rgxp -> (fun (l) (string->list (,rep (list->string l)))))
                )))))


(function strapply* (pp str)
  "Applies a prepared replacer to the given string."
   (list->string (p-result (pp (string->list (any->string str))))))

(function matches? (pp str)
  (let* ((r (pp (string->list str))))
    (and (p-success? r) (null? (p-rest r)))))

;;;;;;;;;;;;;;;;;;;;;;;
(define phex (<r>
              (  (p.digit -> (fmt (d) (list (- (ascii d) (c# #\0)))))
               | (p.lcalpha -> (fmt (d) (list (+ 10 (- (ascii d) (c# #\a))))))
               | (p.ucalpha -> (fmt (d) (list (+ 10 (- (ascii d) (c# #\A))))))) *))


(function HX->N (str)
  (let* ((nn (p-result (phex (string->list str))))
         (rn (reverse nn)))
    (cdr
     (foldl (fun (a z)
             (format a (c . x)
                     (cons (* c 16) (+ x (* z c)))))
           (cons 1 0) rn))))

(define _simple_screen_symbol_regexp
  (<r> " " | "(" | ")" | "\n" | "\t" | "¬"))

(function _simple_screen_symbol (s)
  (let* ((str (symbol->string s))
         (chk (strmatch* _simple_screen_symbol_regexp str)))
    (if chk
        (S<< "&" (to-string (symbol->string s)))
        s)))

(set-car! screen-symbol-stub _simple_screen_symbol)