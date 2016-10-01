;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is an intermediate AST for PFront language. PFront parser (including
;; user-defined syntax) must return an AST compliant with this definition.
;; In case if some of the MBase features are not covered by the AST, it is
;; always possible to smuggle the untact MBase code within the 'lisp' expression
;; node.

(def:ast hlevel ()
  (*TOP* <*topexpr:es>)
  (topexpr
   (|
      (begin . <*topexpr:es>)

      (using <*ident:lst> . <*topexpr:es>)
      (topfunction <ident:nm> <*ident:args> <expr:body>)
      (lispmacro <ident:nm> <*ident:args> <expr:body>)
      (topdefine <ident:nm> <expr:val>)
      (ast3 <astopt:opt> <ident:nm> <*astpar:ps> <*astbody:defs>)
      (ast2 <astopt:opt> <ident:nm> <*astbody:defs>)
      (topsyntax <identpair:nm> <*ident:bor> <sytaxcode:c> <expr:body>)
      (othersyntax <ident:synt> <identpair:nm> <*ident:bor>
                   <sytaxcode:c> <expr:body>)
      (topmacro <identpair:nm> <*ident:bor> <sytaxcode:c> <expr:body>)
      (topparser <ident:name> <*ident:bor> . <*peg:nodes>)
      (macroext <ident:id> <any:code>)
      (hlinclude <string:s>)
      (litinclude <string:texnm> <string:s>)
      (lispinclude <string:s>)
      (topflush)

      (exemodule <ident:nm>)
      (module <ident:nm>)

      (defmodule <ident:nm> <*modexport:exs>)
      (usemodule <*indent:nms>)
      (endmodule)
      (endusing)


      (expr <expr:e>)
      ))

  (astpar (<ident:parent> . <*renamepair:args>))

  (astbody
   (| (alt <ident:nm> . <*astalt:alts>)
      (struct <ident:nm> <aststruct:s>)
      (addalt <ident:nm> . <*astalt:alts>)
      ))

  (astalt
   (| (withargs <ident:nm> <*astbind:args>)
      (single <ident:nm>)
      (remove <ident:nm>)
      ))
  (astbind
   (| (bindmany <ident:tp> <ident:nm>)
      (bindopt <ident:tp> <ident:nm>)
      (bindone <ident:tp> <ident:nm>)
      (dot <astbind:b>)
      (list . <*astbind:bs>)
      ))
  (aststruct
   (| (many <*astbind:args>)
      (single <astbind:b>)))

  (matchpattern (<mpattern:p> <expr:e>))
  (mpattern
   (| (cons <mpattern:a> <mpattern:b>)
      (bindas <ident:id> <mpattern:p>)
      (guard <ident:id> <mpattern:a> <expr:b>)
      (list . <*lmpattern:ps>)
      (nil)
      (quote <symbol:s>)
      (alist <ident:hd> . <*lmpattern:args>)
      (binding <ident:id>)
      (eq <ident:id>)
      (any)
      (number <number:n>)
      (string <string:s>)))
  (lmpattern
    (| (normal <mpattern:p>)
       (append <mpattern:p>)))

  (vtype (| (id <ident:tp>)
            (as <ident:nm>)
            (asonce <ident:nm>)
            (withdst <vtype:t> <ident:dst>)
            ))

  (visitptn
   (| (many <vtype:t> <ident:node> <*visitp:ps> <*visitelse:el>)
      (manyfun <vtype:t> <ident:node> <*ident:args> <*visitp:ps> <*visitelse:el>)
      (forall <vtype:t> <ident:node> <expr:e>)
      (single <vtype:t> <ident:node> <expr:e>)))

  (visitelse
   (| (velse <expr:ee>)
      (elsedeep <*visitp:ps> <*visitelse:es>)))

  (visitp (<ident:v> <expr:e>))

  (ecexpr
   (| (normal <expr:e>)
      (append <expr:e>)))

  (condpair (<expr:l> <expr:e>))
  (casepair (<*symbol:l> <expr:e>))

  (identptn
   (| (var <ident:nm>)
      (ptn <mpattern:f>)))

  (visitopt
   (|
    (recform)
    (dst <ident:id>)
    (listformsrc)
    (listformdst)
    ))
  (expr
   (| (cons <expr:a> <expr:b>)
      (easyconstr <symbol:tag> . <*ecexpr:args>)
      (append <expr:a> <expr:b>)
      (binop <op:o> <expr:a> <expr:b>)
      (leftass <expr:a>) ;; unconverted left-associative tree
      (unleft <expr:a>)
      (stopfix <expr:a>)
      (list . <*ecexpr:es>)
      (range <expr:a> <expr:b>)
      (accelement <ident:a> <ident:b>)
      (begin . <*expr:es>)
      (flatbegin . <*expr:es>)
      (loc <loc:L>)
      (call <expr:fn> . <*expr:args>)
      (match <expr:e> . <*matchpattern:ps>)
      (visit <*visitopt:os> <ident:ast> <ident:top> <expr:e> . <*visitptn:ps>)
      (viter <*visitopt:os> <ident:ast> <ident:top> <expr:e> . <*visitptn:ps>)
      (collector <ident:addname> <ident:getname> <expr:body>)
      (withast <ident:nm> <expr:e>)
      (withmacros <*lmacrodef:ds> <expr:e>)
      (withmetadata <expr:m> <expr:e>)
      (mknode . <*llpair:args>)
      (mksnode <ident:nd> . <*xllpair:args>)
      (mkvnode <ident:nd> <ident:tag> . <*xllpair:args>)
      (mkxnode <ident:tag> . <*xllpair:args>)

      (if3 <expr:e> <expr:tr> <expr:fl>)
      (if2 <expr:e> <expr:tr>)
      (cond <*condpair:es> <*expr:els>)
      (case <expr:e> <*casepair:es> <*expr:els>)
      (lambda <*ident:args> <expr:body>)
      (letloop <ident:nm> <*llpair:args> <expr:b>)
      (filter <identptn:n> <expr:e> <expr:b>)
      (mappend <identptn:n> <expr:e> <expr:b> <*ident:cn>)
      (map <identptn:n> <expr:e> <expr:b> <*ident:cn>)
      (iter <identptn:n> <expr:e> <expr:b> <*ident:cn>)



      (vquote <qexpr:e>)
      (qquote <qexpr:e>)
      (unquote <ident:v>)
      (def <ident:v> <expr:e>)
      (defformat <mpattern:f> <expr:e>)
      (let <ident:v> <expr:e> <*expr:rest>)
      (var <ident:v>)
      (char <char:ch>)
      (number <number:v>)
      (string <string:v>)
      (symbol <symbol:v>)
      (true) (false)
      (lisp <any:code>)
      (macroext <ident:id> <any:code>)

      (pftry <expr:e> <ident:nm> <ident:tp> <expr:proc>)

      ))

  (llpair (<ident:nm> <expr:v>))
  (xllpair (| (p <ident:nm> <expr:v>) (e <expr:v>)))
  (lmacrodef (| (def <ident:nm> <expr:v>)))

  )
