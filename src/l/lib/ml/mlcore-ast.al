;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def:ast mlsrc ( )
  (*TOP* <topexpr>)
;= Top level expressions: type declarations, [[let]] definitions,
;= immediate expression statements and compiler control statements.
  (topexpr
   (|
    (mlffi <ident:nm> <type:tp> <ident:mbasefun>)
    (mlexport <ident:nm> <number:nargs> <string:exportname>)
    (mbinclude <string:fname>)
    (mlinclude <string:fname>)
    (mltype . <*typedef:d>)
    (mlannotate <ident:nm> <type:tp>)
    (mllet <ident:nm> <expr:value>)
    (mlletrec <ident:nm> <expr:value>)
    (mlletrecr . <*lrecdef:dfs>)
    (mlexpr <expr:value>)))
;= Expressions are of a same structure as in any other functional language,
;= with an addition of pattern matching. Normal [[if]] expressions are not
;= present here, since they can be represented as boolean constant pattern
;= matching. Imperative sequencing is present ([[begin]] statements).
  (expr
   (|

    (begin . <*expr:es>)

    (let <ident:nm> <expr:value> <expr:body>)
    (letrec <ident:nm> <expr:value> <expr:body>)
    (letrecr <*lrecdef:dfs> <expr:body>)
    (fun <ident:arg> <expr:body>)

;= Things to aid parsing, but should be removed before any further AST transforms
    (uncurriedfun <*ident:args> <expr:body>)
    (matchfun <*matchptn:ps>)
    (apply0 <expr:fn> . <*expr:args>)
    (makelist . <*expr:args>)
    (cons <expr:hd> <expr:tl>)
    (append <expr:a> <expr:b>)
    (if2 <expr:v> <expr:tr>)
    (if3 <expr:v> <expr:tr> <expr:fl>)


;= Normal nodes


    (apply <expr:fn> <expr:arg>)
    (match <expr:arg> . <*matchptn:ps>)
    (constr <ident:nm> . <*expr:args>)
    (tuple . <*expr:args>)

    (var <ident:nm>)
    (number <num:v>)
    (char   <chr:v>)
    (string <str:v>)
    (bool <bool:v>)
    (unit)
    ))

;= let ... and ... support:
  (lrecdef (<ident:nm> <expr:v>))

;= Pattern matching structures:
  (matchptn (<pattern:p> <expr:v>))
  (pattern
   (| (bind <ident:nm> <pattern:p>)
      (constr <ident:nm> . <*pattern:args>)
      (tuple . <*pattern:args>)
      (number <num:v>)
      (char   <chr:v>)
      (string <str:v>)
      (bool <bool:v>)
      (unit)
      (any)
;= And a couple of transitional nodes, same as for expressions:
      (cons <pattern:a> <pattern:b>)
      (bindany <ident:nm>)

      ))
;= Types in code are only referenced as tuples, named types with arguments
;= or type variables.
  (type (| (T <ident:nm> . <*type:args>)
           (TPL . <*type:args>)
           (F <type:t> <type:r>)
           (V <ident:nm>)))

;= Type definitions can produce name aliases, named tuples and variants.
  (typedef
   (| (alias (<ident:a> . <*type:args>)  <type:b>)
      (variant (<ident:a> . <*type:args>) . <*vardef:b>)))
  (vardef (<ident:cname> . <*type:tpl>))
  )

;{{
(draw:ast:graph mlsrc "mlsrc.dot")
;}}
