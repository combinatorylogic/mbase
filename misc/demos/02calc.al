;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{02calc.al: extending the language}
;-
;-

;- Now we will introduce a somewhat more complex language with variables.
;- The design is similar to the previous version,
;- i.e., a two pass interpreter, with a parser generating an AST and an interpreter over the AST.

;-
;- The AST is just slightly different --- [[let]] and [[var]] variants are added to [[expr]] node:
(def:ast calc02 ( )
  (*TOP* <expr>)
  (expr
   (|
    (plus <expr:a> <expr:b>)
    (minus <expr:a> <expr:b>)
    (mult <expr:a> <expr:b>)
    (div <expr:a> <expr:b>)
    (let <ident:nm> <expr:val> <expr:body>)
    (var <ident:nm>)
    (const <number:v>))))

;- The interpreter is slightly more complicated, with an explicit recursion for [[let]] variant.
(function eval (ex)
  ; {\tt loop} is a recursive function, starting with an empty environment:
  (let loop ((env nil) (e ex))
    (calc02:visit expr e
;= We are using a different visiting strategy here: with [[DEEP]] it will process
;= inner nodes first, and with ``[[_]]'' it will not go into recursion for listed variants.
;= If [[else-deep]] is specified, [[DEEP]] behaviour will be turned on for the rest of
;= patterns.
      (expr _
        ((const v)
         (var  (lookup-env-car env nm))
         (let
            (loop `((,nm ,(loop env val)) ,@env)
                  body))
;= So here [[const]], [[var]] and [[let]] were processed with ``[[_]]'' strategy, and for [[let]],
;= internal subnodes [[val]] and [[body]] were processed by explicit calls to a [[loop]] function,
;= taking care of a correct environment contents. All the rest are processed recursively
;= with [[DEEP]]--strategy.
         (else-deep
          (
           (plus (f+ a b))
           (minus (f- a b))
           (mult (f* a b))
           (div (f/ a b))
           (else nil) ; To suppress a coverage warning
           )))))))

;{{
(define p.double
  (<r> ((p.digit +*) (?? ("." (p.digit +*))))
       -> list->string))
(define p.ident0
  (<r> p.alpha (p.alpha *)))
;}}

;- A lexer is different now, it
;- defines a syntax for identifiers and lists special identifier values which form keywords ([[let]] and [[in]]).
;- Also a new simple token [[EQ]] is introduced.
(make-simple-lexer calclexer
  ; Defines a single pair: a regular expression to match identifiers ({\tt p.ident0}) and a token name for them ({\tt var})
  (ident-or-keyword p.ident0 var)
  ; Gives a list of identifiers that should be keywords with identical token names
  (keywords let in)
  (simple-tokens
   "-" MINUS "(" LB ")" RB "=" EQ
   )
  (regexp-tokens
   (("+") -> list->symbol) OP1
   (("*" | "/") -> list->symbol) OP2
   p.double number)
  (ignore p.whitespace)
  )

;{{
(function getop (x)
  (case x
    ((+) 'plus) ((-) 'minus) ((*) 'mult) ((/) 'div)))
;}}

;- The parser is just a little bit different from the previous one, providing two more
;- patterns for an atomic expression entry [[fact]].
(bnf-parser ((expr calcparser))
  (expr
   ((term:l MINUS expr:r)   `(minus ,l ,r) )
   ((term:l OP1:o expr:r)   `(,(getop o) ,l ,r) )
   ((term)                   $0))
  (term
   ((fact:l OP2:o term:r)   `(,(getop o) ,l ,r))
   ((fact)                   $0))
  (fact
   ((let var:v EQ expr:e in expr:b)
    `(let ,v ,e ,b))
   ((var)                   `(var ,$0))
   ((LB expr:x RB)           x)
   ((number)                `(const ,(flt:parse $0)))
   ((MINUS fact:e)          `(minus (const ,(f# "0")) ,e)))
  )


;- And the usage is still the same:
(writeline (eval (lex-and-parse
                  calclexer
                  calcparser
                  "let x = 2*2 in let y = 1.1 in (2+x)/y")))
