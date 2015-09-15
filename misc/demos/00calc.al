;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{00calc.al: immediate interpretation}
;-

;- This version of a calculator mini--language consists of a parser only.
;- This is the simplest, widely used design, which is suitable for
;- many types of interpreted DSLs.
;-

;- MBase provides a number of parsing tools. The most basic one is [[<r>]] macro, which
;- is mostly used in the form of short regular expressions that recognises lexemes. Our lexer will need
;- a regular expression to recognise the floating point numbers syntax, so here is a definition:
(define p.double
  (<r> ((p.digit +*) (?? ("." (p.digit +*))))
       -> list->string))
;- [[<r>]] is a special mini--language macro. It unrolls into a code, which uses recursive descent
;- parsing combinators. ``[[+*]]'' postfix operator generates ``one--or--many'' combinator, ``[[??]]'' is for
;- ``maybe'' combinator, ``[[->]]'' infix defines a transformation combinator for a parsing result,
;- in our case -- collecting recognised characters into a string.
;-
;- A higher level macro [[make-simple-lexer]] is provided for defining lexers.
;- It is suitable in case your language has whitespaces to
;- ignore (including comments), identifiers, keywords which forms a subset of identifiers, simple string lexemes
;- and regular expression recogniseable lexemes. Most languages fits into this scheme.
(make-simple-lexer calclexer

;= Simple lexemes are just strings. Definitions goes as pair, at first a string
;= and then a token. I.e., here ``[[-]]'' lexeme produces [[MINUS]] token.
  (simple-tokens
   "-" MINUS "(" LB ")" RB
   )
;= These are more complex cases, token recognisers are defined using the language of [[<r>]] macro.
;= Definitions are also paired in the same way as for [[simple-tokens]].
  (regexp-tokens
   (("+") -> list->symbol) OP1
   (("*" | "/") -> list->symbol) OP2
   p.double number)
;= And here are regular expressions to ignore totally, i.e. whitespaces:
  (ignore p.whitespace)
  )

;- Tokens from a lexer are represented as symbols (N.B. [[list->symbol]] used above), so we
;- can dispatch them into a floating point arightmetic functions easily:
(function getop (x)
  (case x
    ((+) f+) ((-) f-) ((*) f*) ((/) f/)))

;- And now we will implement a parser and an interpreter in single pass.
;- MBase provides a special mini--language for
;- defining simple parsers in BNF--like format. First argument of [[bnf-parser]] macro is a list
;- of entry points to be exported as functions. It means that [[calcparser]] will be defined for
;- a parser that accepts [[expr]] syntax. All the rest inside [[bnf-parser]] is a list of entries,
;- each entry contains patterns and expressions to be evaluated if a pattern is matched.
(bnf-parser ((expr calcparser))
;= Top level expression entry, recognising low priority operations (``[[+]]'', ``[[-]]'') first. ``[[MINUS]]'' is
;= a separate entity here because it can be seen as unary operation as well.
  (expr
   ((term:l MINUS expr:r)   (f- l r) )
   ((term:l OP1:o expr:r)   ((getop o) l r) )
   ((term)                   $0))
;= Intermediate expressions entry, dealing with highest priority operations (``[[*]]'', ``[[/]]'').
  (term
   ((fact:l OP2:o term:r)   ((getop o) l r))
   ((fact)                   $0))
;= Atomic expressions, including unary negation.
  (fact
   ((LB expr:x RB)           x)
   ((number)                (flt:parse $0))
   ((MINUS fact:e)          (f- (f# "0.0") e)))
  )

;- And now we can use this interpreter by calling a special function [[lex-and-parse]], which
;- applies a lexer to a string or a stream and applies a given parser to a resulting tokens stream.
(writeline
 (lex-and-parse
  calclexer
  calcparser
  "(2+2*2)/1.1"))
