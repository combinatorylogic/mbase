;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{01calc.al: two passes}
;-
;-

;- Now, to illustrate a more complex approach, we will separate
;- a parsing pass and an evaluation pass. To do this, we need to define an
;- abstract syntax tree structure. MBase provides a special mini--language
;- for defining algebraic data types and verifiable transforms over them: [[def:ast]]
;- macro.
(def:ast calc01 ( )
  (*TOP* <expr>) ; An entry node
  (expr ; Single variant node
   (|
    (plus <expr:a> <expr:b>)
    (minus <expr:a> <expr:b>)
    (mult <expr:a> <expr:b>)
    (div <expr:a> <expr:b>)
    (const <number:v>))))
;- If you have programmed in ML or Haskell, you will find the definition above somewhat familiar.
;- [[expr]] is a variant data type, and [[plus]], [[minus]], etc. are constructor
;- tags. [[expr]] is a recursive type, referencing to itself.


;-
;- We need such a definition not only for a documenting purposes, but also for
;- defining visitors and iterators over these AST structures.
;- The following function, [[eval]], takes [[calc01]] AST as a source and returns
;- a floating point number, an interpreted value of this tree. In languages like
;- ML you would have to write a recursive function and use pattern matching, but MBase
;- way is different --- a visitor recursive function will be generated automatically,
;- and you only have to declare certain nodes and variant entries transforms
;- explicitly.
(function eval (e)
 ; Visits {\tt e} assuming it contains {\tt expr} node:
  (calc01:visit expr e
   (expr DEEP ; Transforms {\tt expr} nodes using depth--first strategy
    ((const v) ; all {\tt expr} variants are listed here
     (plus (f+ a b))
     (minus (f- a b))
     (mult (f* a b))
     (div (f/ a b))
     ))))

;- Here and further we will omit definitions that are identical to previous
;- versions, giving only modified entries. Lexer is the same
;- as in [[00calc.al]].
;-

;{{
(define p.double
  (<r> ((p.digit +*) (?? ("." (p.digit +*))))
       -> list->string))

(make-simple-lexer calclexer
  (simple-tokens
   "-" MINUS "(" LB ")" RB
   )
  (regexp-tokens
   (("+") -> list->symbol) OP1
   (("*" | "/") -> list->symbol) OP2
   p.double number)
  (ignore p.whitespace)
  )

;}}

;- Our dispatch function is different now, it translates tokens into tags, not
;- functions.
(function getop (x)
  (case x
    ((+) 'plus) ((-) 'minus) ((*) 'mult) ((/) 'div)))

;- And the parser is different, it produces [[calc01]] AST instead of evaluating
;- values immediately.
(bnf-parser ((expr calcparser))
  (expr
   ((term:l MINUS expr:r)   `(minus ,l ,r) )
   ((term:l OP1:o expr:r)   `(,(getop o) ,l ,r) )
   ((term)                   $0))
  (term
   ((fact:l OP2:o term:r)   `(,(getop o) ,l ,r))
   ((fact)                   $0))
  (fact
   ((LB expr:x RB)           x)
   ((number)                `(const ,(flt:parse $0)))
   ((MINUS fact:e)          `(minus (const ,(f# "0")) ,e)))
  )

;- The usage is pretty much the same, we only add [[eval]] function call here:
(writeline (eval (lex-and-parse
                  calclexer
                  calcparser
                  "(2+2*2)/1.1")))
