;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{An internal AST for PEGs}
;-

(def:ast packrat ()
  (*TOP* <ntexprs>)

  (ntexprs <*ntexpr:es>)

  (ntexpr
   (|
      (terminal <?ctortype:dtype> <id:ttype> <id:name> <expr:value>
                <dualcode:constr> . <*report:r>)
      (binaries <?ctortype:dtype> <id:ttype> <id:name> <expr:lr>
                <*binvar:vs> . <*report:r>)
      (with-ignore <id:igname>)
      (define <id:name> <*id:argnames> <expr:e> <dualcode:constr> . <*report:r>)
      (rule <id:name> <expr:e> <dualcode:constr>)
      (dynahook <id:name>)
      (targetast <ident:name>)
      ;; syntax sugar to be expanded into binaries
      (src-binaries <?ctortype:dtype> <id:ttype> <id:name>
                    <*srcbinvar:vs> . <*report:r>)

      ))

  (dualcode (<annot:a> <code:c>))
  (annot (.<*apair:as>))
  (apair (<ident:tg> <ident:v>))
  (binvar (<number:prec> <assoctp:assoc> <expr:op> <dualcode:constr> . <*report:r>))
  (srcbinvar (| (simple <expr:e>)
                (binary <number:prec> <assocptl:assoc> <expr:e>
                        <dualcode:constr>)))
  (code
   (|
      (var <id:name>)
      (const <id:s>)
      (fcall <id:fname> . <*code:ars>)
      (constr <id:cname> . <*carg:ars>)

      ;; The following three are only available in recform mode
      (dconstr <id:nname> <id:cname> . <*carg:ars>)
      (list . <*carg:ars>)
      (dauto . <*id:tagname>)

      (action <lisp:code>) ;; USE WITH CAUTION
      (auto . <*id:tagname>) ;; to be replaced with an automatically inferred code
      (nop)
      ))
  (carg
   (| (set <id:var> <code:val>)
      (append <id:var> <code:val>)
      ))

  (expr
   (| (seq . <*expr:es>)   ; E1 E2 ...
      (palt . <*expr:es>)  ; E1 / E2 ...
      (pdalt . <*expr:es>)  ; E1 / E2 ...

      (merge . <*expr:es>) ; merge alternative branches

      (andp <expr:e>)    ; & E
      (notp <expr:e>)    ; ! E
      (plus <expr:e>)    ; E +
      (star <expr:e>)    ; E *
      (maybe <expr:e>)   ; E ?
      (trivial <pred:p>) ; trivial recognisers (characters, strings, ...)
      (withignore <id:terms> <expr:e>)

      ; if E fails, fail unconditionally with a message
      (ordie <expr:e> <msg:m> . <*code:args>)

      (withfilter <any:f> <expr:e>)

      (bind-terminal <id:fname> <id:tname> . <*id:rec>)
      (bind <id:name> <expr:e>)
      (terminal <id:name> . <*id:rec>)
      (simple   <id:name>)
      (lift <?ctortype:dtype> <expr:e> <dualcode:c> . <*report:r>)
      (rule <id:name> <str:body>)
      (macroapp <id:name> . <*expr:args>)

      (action <id:name> <any:args>) ; Action over environment,
                                    ; does not affect parsing

      (highorder <*id:args> <id:maker>)
      (check <id:name>)
      (hint <id:name> <*any:args>)  ; backend-specific hint, ignored by parser
      )))

(def:ast pktrivial ()
  (*TOP* <pred>)
  (pred
   (| (char <int:chr>)
      (anychar)
      (range <int:from> <int:to>)
      (or . <*pred:ps>)
      (string . <lstring:str>)
      (sstring <string:str>)
      (fail) ; always fails
      )))

