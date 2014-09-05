;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{03calc.al: compiler}
;-
;-

;- In previous examples the AST was interpreted. Now we will translate it into MBase language so it 
;- can be compiled and executed as MBase code.
;-

;{{
(def:ast calc03 ( )
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
;}}

;- This transform is simple again, simpler than the 
;- previous interpreter, since it generates MBase code directly
;- and MBase itself will deal with variable bindings.
(function compile (ex)
    (calc03:visit expr ex
      (expr DEEP
	((const `(f# ,v))
         (var   nm)
         (let   `(alet ,nm ,val ,body))
         (plus  `(f+ ,a ,b))
         (minus `(f- ,a ,b))
         (mult  `(f* ,a ,b))
         (div   `(f/ ,a ,b))
         ))))

;{{
(define p.double
  (<r> ((p.digit +*) (?? ("." (p.digit +*))))
       -> list->string))
(define p.ident0
  (<r> p.alpha (p.alpha *)))

(make-simple-lexer calclexer
  (ident-or-keyword p.ident0 var)
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
;}}

;{{
(function getop (x)
  (case x
    ((+) 'plus) ((-) 'minus) ((*) 'mult) ((/) 'div)))
;}}

;- The parser is just slightly different, numbers in it are not parsed but
;- represented as strings instead --- MBase backend will parse them later.
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
   ((number)                `(const ,$0))
   ((MINUS fact:e)          `(minus (const "0.0") ,e)))
  )

;- And in order to add this language to MBase and to be able to use MBase as a
;- backend, we will define a macro. This example
;- illustrates how blurred the border between metaprogramming and
;- compilation is.
(macro calc03# (str)
  (compile (lex-and-parse calclexer calcparser str)))

;- The usage is quite trivial:
(writeline (calc03# "let x = 2*2 in let y = 1.1 in (2+x)/y"))

;- As a side effect of this way of compilation, all the MBase variables are visible:
(writeline (let ((x (f# "2.0")))
             (calc03# "(2+x*x)/1.1")))