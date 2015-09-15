;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{Flat intermediate code AST definition}
;-

(def:ast cc:flatast ( )
  (*TOP* <lifted>)
  (lifted <*liftop:code>)
  (liftop
   (|
    (Init <ident:mode> <block:e>)
    (Simple <ident:name> <ident:usename> <*ident:fargs> <block:e>)
    (Global <ident:name> <ident:usename> <block:e>)
    (Funref <ident:gloname> <ident:iname>)
    (Closure <ident:name> <ident:usename> <*ident:args>
             <*ident:fargs> <block:e>)
    ))
  (block <*code:cd>)
;= Expressions are atomic now, including a virtual reference to the stack.
;= Applications pop arguments from left to right.
  (expr
   (|
    (Var <ident:name>)
    (Arg <ident:name>)
    (Glob <ident:name>)
    (GlobRec <ident:name>)
    (Gencall <number:n>)
    (Clenv <ident:name>)
    (Meta <ipexpr:name>)   ;; not a value but a reference
    (IntPtr <ipexpr:name>)

    (Str <string:s>)
    (Num <number:n>)
    (NBNum <number:n>)
    (Chr <char:c>)
    (Bool <bool:b>)
    (Symbol <symbol:v>)
    (Quote <any:val>)
    (Nil)

    (Pop)))
;= Function body is also a list of flat assembly instructions.
  (code
   (|
    (Local <ident:name>)
    (FixLocal <ident:name> <ident:oldname>)
    (DebugPoint . <*debugdata:d>)
    (Label <label:lbl>)
    (Iflabel <label:lbl>)
    (Goto <label:lbl>)
    (Setlocal <ident:id> <expr:value>)
    (Setglobal <ident:nm> <expr:value>)
    (Setnewglobal <ident:nm> <expr:value>)
    (Push <expr:value>)
    (Patchclosure <ident:id> <ident:liftid> . <*patcharg:args>)
    (Switch <expr:val> . <*ident:labels>)

    (Pushapp <expr:fn> . <*expr:args>) ; Regular call
    (Setlocapp <ident:nm> <expr:fn> . <*expr:args>)
    (Retapp <expr:fn> . <*expr:args>)  ; A tail call
    (Dropapp <expr:fn> . <*expr:args>)
    (Asm <atarget:tgt> <*useident:use> . <any:body>)

    (ClosureDummy)
    (TryBegin)
    (CatchBegin <ctype:ex>)
    (TryEnd)

    (InitGlobalVar <expr:a1> <expr:a2> <expr:a3>)
    (InitGlobalFun <expr:a1> <expr:a2> <expr:a3>)
    (InitGlobalMac <expr:a1> <expr:a2>)

    (Car <expr:arg>)
    (Cdr <expr:arg>)
    (Cons <expr:a> <expr:b>)

    (GotoNull <expr:a> <label:lbl>)
    (GotoPairP <expr:a> <label:lbl>)
    (GotoEqv <expr:a> <expr:b> <label:lbl>)

    (BinOp <arith:op> <expr:left> <expr:right>)
    (IntBox <expr:arg>)
    (IntUnbox <expr:arg>)

    (Return <expr:value>)
    (Pop <expr:arg>)
    (Nop)
    (Gotoif <label:lbl> <expr:cnd>)
    (Gotoifnot <label:lbl> <expr:cnd>)
    ))
  (patcharg (<number:n> <ident:id>))
  (useident (<ident:old> <expr:new>))

  )