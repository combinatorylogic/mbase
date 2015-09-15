;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(function p[T] (tn)
   (fun (l) (list (list tn l))))

(function p[xT] (tn pf)
   (fun (l r) (list (list tn (pf l) r))))

(function p[xTap] (tn pf)
   (fun (l r)
       (list (list tn (pf (genvalue l)) r (genposition l)))))

(function p[xTp] (tn pf)
   (fun (l r pos)
     (let ((vv (pf l)))
       (list (list tn vv r pos)))))

(function p[lT] (tn)
   (fun (l)
     (list (list tn (genlist->string l)))))

(function p.t (tk) (cadr tk))

(macro R> (frmt . body)
   `(fmt ,frmt (list (begin ,@body))))

(define <p.t> (R> (tk) (cadr tk)))

(function :: (x)
  "Convert a list of chars [x] into string, returns a list containing that string."
  (list (genlist->string x)))

(function wrap (x)
  "Creates a list of one element, same as [(list x)]."
  (cons x nil))

;;; Basic parsers

(function p.eof (l)
  "Recognizes an end of input stream."
  (if (null? l) (p-mkresult nil nil) (p-mkfail 'eof l)))

(function p>pred (pr)
  ("Makes a predicate recogniser, which applies a given predicate [pr]"
   "to the first element of an input stream. Fails on EOF."
   )
   (fun (l)
     (if (null? l) (p-mkfail 'eof l)
      (if (pr (car l))
          (p-mkresult (list (car l)) (cdr l))
          (p-mkfail '? l)))))

(macro p>pred (pr)
   `(fun (l)
       (if (null? l) (p-mkfail 'eof l)
         (if (,pr (car l))
           (p-mkresult (list (car l)) (cdr l))
           (p-mkfail '? l)))))

(function p>eq (v)
  ("Makes an equality recogniser, using [eq?] predicate."
   "Same effect as [(p>pred (cut eq? v <>))]."
   )
   (fun (l)
        (if (null? l) (p-mkfail 'eof l)
            (if (eq? v (car l))
                (p-mkresult (list (car l)) (cdr l))
                (p-mkfail v l)))))

(function p>chareq (v)
  ("Makes a character equality recogniser, using [genchar=?] predicate."
   )
  (let ((va (ascii v)))
   (fun (l)
        (if (null? l) (p-mkfail 'eof l)
            (if (genachar=? va (car l))
                (p-mkresult (list (car l)) (cdr l))
                (p-mkfail v l))))))

(function p>touch (p)
  ("Makes a recogniser which is successful if [p] is successful, discarding [p] application results,"
   "and fails otherwise."
   )
   (fun (l)
      (let ((r (p l)))
         (if (p-success? r)
             (p-mkresult nil l)
             (p-mkfail (p-result r) l)))))

;;; Derivative parsers

(macro c# (ch)
  "Expands into an integer representing the given character code."
  (ascii ch))

(function p.any (l)
  "An always successful recogniser with no result value."
  (p-mkresult nil l))

(define p.
  "Recogniser, successful for any non-EOF input stream element. Result contains that one element."
  (p>pred (fun (x) #t)))
(define p.lcalpha
  "Regonises a lower case latin character."
  (p>pred
    (fun (x) (if (genchar? x)
               (let ((a (genascii x)))
                 (and (>= a (c# #\a)) (<= a (c# #\z))))))))

(define p.ucalpha
  "Recognses an upper case latin character."
  (p>pred
    (fun (x) (if (genchar? x)
               (let ((a (genascii x)))
                 (and (>= a (c# #\A)) (<= a (c# #\Z))))))))

(define p.alpha
  "Recognises any latin character."
  (p<|> p.ucalpha p.lcalpha))


(define p.digit
  "Recognises any decimal digit."
  (p>pred
    (fun (x) (if (genchar? x)
               (let ((a (genascii x)))
                 (and (>= a (c# #\0)) (<= a (c# #\9))))))))

(function p>string (str)
  "Makes a recogniser for a string."
  (let ((sl (string->list str)))
     (let loop ((l sl))
        (if (null? l) p.any
            (if (null? (cdr l)) (p>chareq (car l))
                (p<+> (p>chareq (car l)) (loop (cdr l))))))))

(macro pm>string (str)
  "Macro version of [p>string]."
  (let ((sl (string->list str)))
     `(pm<+> ,@(map (fun (c) `(p>chareq ,c)) sl))))

(macro pm>chars (str)
  "Makes a recogniser which accepts all of the [str]'s chars."
  (let ((sl (string->list str)))
     `(pm<|> ,@(map (fun (c) `(p>chareq ,c)) sl))))


(define p.space
  "Recognises any whitespace charater."
  (pm<|> (p>chareq #\Tab) (p>chareq #\Space) (p>chareq #\Newline)))

(define p.newline
  "Recognises a newline character."
  (p>chareq #\Newline))

(function p>token (tk)
  "Recognises a given token in an input stream, where tokens are lists: [(<tokenname> ...)]."
   (p>pred (fun (x) (and (list? x) (eqv? tk (car x))))))

(function p<0> (p)
  "Makes a parser, which discards a recognition result of [p]."
  (p<R> p (fun (x) '())))


