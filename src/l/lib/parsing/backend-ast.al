;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This ast definition is for reference only, it is not used by the MBase backend
; (but might be useful for, say, emacs or C backends).

(def:ast pegtarget ()
  (*TOP*  <*topexpr:es>)
 
  (pegexpr 
   (| (peg-ignore <*term:ts> <pegexpr:code>)
      (peg-node <pegexpr:code>)
      (peg-if <pegexpr:quest> <pegexpr:tru> <pegexpr:fals>)
      (peg-check <pegexpr:first> <pegexpr:next>)

      (peg-dummy)
      (peg-dummy-back)
      (peg-loop <pegexpr:code>)
      (peg-bind <bool:mlt> <ident:nm> <pegexpr:e>)
      
      ;; Term calls
      (peg-call-gen <ident:id> <bool:rec>)
      (peg-call-terminal-gen <ident:id> <bool:rec>)
      (peg-call-simple <ident:id>)
      (peg-call-terminal <ident:id>)


      (peg-trivial <pred:code>)

      ;; GADT-alike stuff, incomplete
      (peg-check-collect <pegexpr:first> <pegexpr:rest>)
      (peg-merge-altbranches <pegexpr:e>)
      
      ;; "FFI" stuff, potentially not so portable
      (peg-call-checker <ident:checker>)
      (peg-call-highorder <*ident:args> <ident:makerfun>)

      ;;;;;
      (peg-fail)
      ))

  (pred
   (| (char <int:chr>)
      (anychar)
      (range <int:from> <int:to>)
      (or . <*pred:ps>)
      (string . <lstring:str>)
      (sstring <string:str>)
      (fail)
      ))

 
  (termfun
   (| (peg-term-function <range:rng>
                         <ident:nm> <ttype:tp>
                         <constr:tc>
                         <pegexpr:ic>
                         <any:report>)
      (none)
      ))

  (termfundef (<ident:nm> <termfun:df>))

  (topexpr
   (| (peg-parser <bool:exportp>
                  <ident:topentry>
                  <*ident:parents>
                  <*termfundef:defs>
                  <*ident:exports>
                  <*ident:dhooks>)
      (none)
      ))
  )
      
      
(function peg-backend-verify (topexpr)
  (pegtarget:visit topexpr topexpr
     (pegexpr DEEP (forall node))))

