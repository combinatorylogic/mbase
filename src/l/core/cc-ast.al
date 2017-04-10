;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;-
;- \subsection{Core AST definition}
;-

;-
;- Here follows an AST definition which is used in all the stages prior to
;- Plain code generation. The same AST is used for both source code (produced in
;- the previous step out of macro expanded expressions) and
;- a lambda lifted code (see below).
;-

(def:ast cc:mbcoreast ( )
  (*TOP* <lifted>)
  (lifted  <*liftop:code>)
  (liftop
   (|
    (Init <ident:mode> <expr:e>)
    (Global <ident:name> <ident:usename> <expr:vl>)
    (Funref <ident:gloname> <fref:iname>)
    (Simple <ident:name> <ident:usename> <expr:e>)
    (Closure <ident:name> <ident:usename> <*ident:args> <expr:e>)
    ))
;= Generic expression structure, suitable for both lifted and source
;= expressions. Simple atoms goes first:
  (expr
   (| (Str <string:s>)
      (Num <number:n>)
      (FNum <fnumber:n>)
      (Chr <char:c>)
      (Bool <bool:b>)
      (Symbol <symbol:s>)
      (Var <ident:id> . <*metadata:md>)
      (Nil)

;= The following terminal variants appears only after local transforms and
;= should not be present in a source code stage:
      (Arg <ident:id>)
      (Glob <ident:id>)
      (Recref <ident:id>) ; not-yet-specialised recursive reference
      (Funref <ident:id>) ; Local function reference
      (Clenv <ident:id>) ; closure environment reference

;= More complex variants with recursion:
      (Quote <any:l>)
      (If <expr:e> <expr:iftr> <expr:iffl>)
      (Fun <ident:recname> <*fnarg:args> <expr:body>)
      (App <expr:fn> . <*expr:args>)
      (Set <ident:nm> <expr:vl>)
      (XSet <ident:tp> <ident:nm> <expr:vl>)

      (SLet <*letarg:defs> <expr:body>)
;= Generic letrec, to be specialised later for closures and non--closures.
      (SLetRec <*letarg:defs> <expr:body>)

      (MDAnnot <*mdannot:defs> <expr:body>)

;= Special stuff:
      (TryBlock <expr:body> <ctype:tcatch> <expr:actionfun>)
      (Switch <nopt:n> <expr:body> . <*expr:opts>)
      (Labels . <*lblexpr:es>)
      (Dummy)
      (Label <labelident:id>)
      (GotoLabel <labelident:id>)
      (FixLocal <ident:id> <ident:oldid>)
      (DebugPoint . <*debugdata:d>)
      (Ghost <expr:e>)

;= Performance stuff:
      (Car <expr:e>)
      (Cdr <expr:e>)
      (Cons <expr:a> <expr:b>)
      (Cons1 <expr:a>)
      (Cons0)
      
      (NullP <expr:a>)
      (PairP <expr:a>)
      (Not <expr:a>)
      (Eqv <expr:a> <expr:b>)
      (SetCar <expr:dst> <expr:e>)
      (SetCdr <expr:dst> <expr:e>)

      (IfNull <expr:e> <expr:iftr> <expr:iffl>)
      (IfPair <expr:e> <expr:iftr> <expr:iffl>)
      (IfEqv <expr:a> <expr:b> <expr:iftr> <expr:iffl>)

      (BinOp <arith:op> <expr:left> <expr:right>)
      ;TODO: break constant folding
      (NoConst <expr:a>)

;= Imperative looping hints:
      (While <ident:escapename> <expr:cnd> <expr:body>)
      (Escape <ident:escapename>) ; Won't work from a nested lambda!

;= And a grouping, of course, since it is an imperative language:
      (Begin . <*expr:es>)

;= High level forms to control the global environment (provided
;= with a top level macro expansion):
      (GSetFn <ident:recp> <ident:nm> <number:nargs> <expr:vl>)
      (GSetM  <ident:nm> <expr:vl>)
      (GSet   <ident:nm> <expr:vl>)

;= Hacking tool: target platform assembly
      (BackendAsm <asmtype:atp> <*useident:use> . <any:body>)

;= Introduced as an intermediate compilation construction:
      (PatchClosure <ident:nm> <ident:liftname> . <*captcharg:args>)

      ))

;= Function argument
  (fnarg (| (var <ident:nm> . <*metadata:md>)))

;= Utility nodes

  (captcharg (<number:n> <ident:ref>))
  (letarg (<ident:nm> <expr:value> . <*metadata:md>))
  (useident (<ident:old> <expr:new>))
  (lblexpr (<ident:label> <expr:e>))
  )

