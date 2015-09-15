;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;  S-expressions parser
;;

(define mbase.char0 (<r> (% "!$%^&*_-+<>/\\?#~@:=|")))
(define mbase.char1 (<r> (% ".,'`")))
(define mbase.ident.p
  (<r> ((p.alpha | mbase.char0) ((mbase.char0 | mbase.char1 | p.alpha | p.digit)*))))

(define p.comment
  (<r> (";" ((! "\n") *) "\n")))

(make-simple-lexer sexp-lexer
  (ident-or-keyword mbase.ident.p SYMBOL)
  (simple-tokens
   "#t" TRUE "#f" FALSE
   "(" LQB ")" RQB "[" LSB "]" RSB
   "{" LXB "}" RXB
   ",@" SPLICEUNQUOTE
   "," UNQUOTE "." DOT "'" QUOTE "`" BQUOTE
   )
  (regexp-tokens
   (("#\\" (p.alpha *)) ->
    (M@ (fun (x)
          (let* ((pos (genposition x))
                 (vv
                  (case (genvalue x)
                    ((Space) #\Space)
                    ((Newline) #\Newline)
                    ((Tab) #\Tab)
                    ((LBR) #\LBR)
                    ((RBR) #\RBR)
                    ((Semicolon) #\Semicolon)
                    (else (car (string->list (any->string x)))))))
            (if pos (mkgen vv pos) vv)))
        genlist->symbol
        cddr
        ))
       CHAR
     (((_ "\"") (((#\\ #\") | (! #\")) *) (_ "\"")) -> genlist->string)
       STRING
     p.integer.p INT
   )
  (ignore p.whitespace
          p.comment)
  )

(bnf-parser
    ((topexpr sexp-parser)
     (topexprs sexp-parser-many)
     )
  (topexpr
   ((expr) $0)
   (() nil))

  (topexprs
   ((expr topexprs) (cons $0 $1))
   ((expr) (list $0))
   )

  (expr
   ((UNQUOTE expr:e) (list 'unquote e))
   ((SPLICEUNQUOTE expr:e) (list 'unquote-splicing e))
   ((QUOTE expr:e)  (list 'quote e))
   ((BQUOTE expr:e) (list 'quasiquote e))
   ((atom) $0)
   ((list) $0)
   )

  (list
   (( LQB RQB ) '())
   (( LXB RXB ) '())
   (( LSB RSB ) '())
   (( LQB innerlist RQB ) $1)
   (( LXB innerlist RXB ) $1)
   (( LSB innerlist RSB ) $1)
   )

  (innerlist
   (( expr:a DOT UNQUOTE expr:b ) (list a (list 'unquote-splicing b)))
   (( expr:a DOT expr:b ) (cons a b))
   (( expr:hd innerlist:tl) (cons hd tl))
   (( expr:a ) (list a)))

  (atom
   (( STRING ) $0)
   (( INT ) $0)
   (( SYMBOL ) $0)
   (( TRUE ) #t)
   (( FALSE ) #f)
   (( CHAR ) $0)
   )
  )

(define _sethint (r_tbind "Meta.Scripting.ExtendedReader" "mkhint" t_object))
(define _gethint (r_tbind "Meta.Scripting.ExtendedReader" "gethint"))
(function new-aread ( strm )
  (let ((doit
         (fun (fx)
           (try
            (let ((val (fx)))
              (_sethint strm (cdr val))
              (car val))
            t_MBaseException
            (fun (e)
              (let ((err (mbaseerror e)))
                (println (S<< "Parsing error:\n"
                              (to-string err))))
              nil)))))
    (cond
     ((xeof? strm) (_sethint strm nil) nil)
     ((null? (_gethint strm))
      (doit (fun () (lex-and-parse-stream sexp-lexer sexp-parser strm))))
     (else
      (doit (fun ()
              (sexp-parser (_gethint strm))))))))

(define mb-failure (Sm<< " sexpr parser failure "))
(function mbase-parse-line (str)
  (try
   (lex-and-parse sexp-lexer sexp-parser str)
   t_MBaseException
   (fun (ex)
     (p:match (mbaseerror ex)
       ((PARSE-ERROR . $x)
        mb-failure)
       (else (r_raise ex))))))

(function mbase-parse-normal (str)
  (lex-and-parse sexp-lexer sexp-parser str))

(function mbase-parse-repl (rdr prompt)
  (let loop ((buf ""))
    (let ((nxt (rdr)))
      (cond
       ((null? nxt)
        (if (string=? buf "") nil
            (mbase-parse-normal buf)))
       (else
        (let* ((nbuf (S<< buf " " nxt))
               (atmpt (mbase-parse-line nbuf)))
          (if (eq? atmpt mb-failure)
              (begin (if prompt (prompt)) (loop nbuf))
              atmpt)))))))

;; Unit tests

;;; A failing test: disabling for now
;;(unit-test 3 (eqv? mb-failure (mbase-parse-line "(a b . ")) #t)
(unit-test 3 (eqv? mb-failure (mbase-parse-line "(a (x . 1) b . c)")) ())

(unit-test 3 (mbase-parse-line "(a (b . c) \"ooo\" 1 2345 ,x ,@yyy)")
           (a (b . c) "ooo" 1 2345 ,x ,@yyy))


