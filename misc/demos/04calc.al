;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{04calc.al: IL compiler}
;-
;-
;- And finally, here is a ``real'' compiler which generates .NET IL instructions directly.
;- Surprisingly, it is almost as easy as the previous compiler that generated
;- a high level language code, thanks to the fact that .NET is a stack machine.

;{{
(def:ast calc04 ( )
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

;-
;- [[compile]] function transforms an AST into a flat list of IL instructions.
(function compile (ex)
    (calc04:visit expr ex
      (expr DEEP
	((const `((Ldc_R8 ,(flt:parse v))))
         (var   `((Ldloc (var ,nm))))
         (let   `((local ,nm ,t_Double)
                  ,@val
                  (Stloc (var ,nm))
                  ,@body))
         (plus  `(,@a ,@b (Add)))
         (minus `(,@a ,@b (Sub)))
         (mult  `(,@a ,@b (Mul)))
         (div   `(,@a ,@b (Div)))
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

(function getop (x)
  (case x
    ((+) 'plus) ((-) 'minus) ((*) 'mult) ((/) 'div)))

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
;}}

;- 

;- MBase has a
;- possibility to inline IL code ([[n.asm]]), so we will wrap our
;- compiler in a macro.
(macro calc04# (str)
  `(n.asm ()
      ,@(compile (lex-and-parse calclexer calcparser str))
      ; Return a boxed object:
      (Box ,t_Double)))

;- And usage is the same:
(writeline (calc04# "let x = 2*2 in let y = 1.1 in (2+x)/y"))

;- A curious reader can print out the compiled code:
(iter writeline 
      (compile 
       (lex-and-parse calclexer calcparser 
		      "let x = 2*2 in let y = 1.1 in (2+x)/y"
                      )))

