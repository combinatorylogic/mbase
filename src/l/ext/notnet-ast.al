;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(Section "Not.Net target language: low level \NET{} imperative functionality")

;;;; Typed low-level language - no inference, just propagation.
(def:ast lltnet ()
  (*TOP* <class>)
  (class ((<ident:name> <ident:parent> . <*ident:interfaces>)
         . <*classmember:members>))
  (classmember
   (|
    (method <*access:acc> (<type:ret> <ident:name> . <*mtdarg:args>)
            <mtdbody:body>)
    (field <*access:acc> <type:tp> <ident:name> <nnnexpression:init>)
    (initfield <*access:acc> <ident:name> . <*mess:mess>)
    (custom <any:thing>)
    ))

  (mtdbody
   (|
    (tvar <type:tp> <ident:name> <expression:value> . <*mtdbody:in>)
    (tvardef <type:tp> <ident:name> . <*mtdbody:in>)
    (var <ident:name> <expression:value> . <*mtdbody:in>)
    (vvar <mtdinfo:mtd> <*expression:args> <ident:name> . <*mtdbody:in>)

    (nop)

    (debugpoint . <*debugdata:d>)

    (begin . <*mtdbody:b>)
    (return <expression:e>)
    (tailreturn <expression:e>)
    (leave <expression:e>)
    (vreturn)

    (if <expression:value> <mtdbody:iftr> . <*mtdbody:iffls>)
    (case <expression:value> .  <*casebody:b>)

    (tryblock <mtdbody:body> (<ident:nm> <type:mask>) <mtdbody:bcatch>)

    (throw <expression:exc>)

    (label <ident:nm>)
    (goto <ident:nm>)
    (gotoif <expression:value> <ident:nm>)
    (gotoiff <expression:value> <ident:nm>)

    (set <expression:lvalue> <expression:e>)
    (xset <lvaluet:lvalue> <expression:e>)
    (e <expression:e>)
    (subst <expression:e>)

    ;;; Higher level expressions to be expanded before any other steps:
    (for ((<ident:var> <expression:init>)
          <expression:step>
          <expression:cont>)
         <mtdbody:body>)
    (foreach (<ident:var> <expression:init>) <mtdbody:body>)
    (while <expression:cont>
           <mtdbody:body>)
    (dowhile <expression:cont>
             <mtdbody:body>)

    (embedd-asm . <somestuff:code>)

    (pass-expr <expression:e>)
    ))

  (lvaluet (<type:tp> . <lvalue:v>))
  (lvalue ;; used at the last compilation stage only
   (|
    (vl <ident:nm>) ; local var
    (f <expression:e> <fldinfo:fld>)
    (sf <fldinfo:fld>)

    (vf <ident:nm> <fldinfo:fld>)
    (vaf <type:tp> <expression:e> <expression:idx> <fldinfo:fld>)

    (ar <expression:e> <expression:idx>)
    ))

  (vvaluet (<type:tp> . <vvalue:v>))
  (vvalue ;; used at the last compilation stage only
   (|
    (vl <ident:nm>) ; local var
    (ar <expression:e> <expression:idx>)
    (lbox <expression:e>)
    ))

  (expression
   (|
    (pass <verbatim:v>) ;; fall through

    (am <bool:tailp> <expression:e> <mtdinfo:mtd> . <*expression:args>)
    (avm <bool:tailp> <vvaluet:e> <mtdinfo:mtd> . <*expression:args>)
    (asm <bool:tailp> <mtdinfo:mtd> . <*expression:args>)
    (csm <bool:tailp> <mtdinfo:mtd> . <*expression:args>)

    (newarr . <*expression:args>)
    (newtarr <type:tp> . <*expression:args>)
    (allocarr <type:tp> <expression:size>)

    (v <ident:nm>)

    (ref <ident:nm>)

    ; To appear later after the scope and type resolving
    (vl <ident:nm>) ; local var
    (va <ident:nm>) ; method arg
    (ldthis)
    ;

    (f <expression:e> <fldinfo:fld>)
    (funref <mtdinfo:mtd>)
    (newdelegate <type:tp> <type:h> <ident:name> <expression:e>)

    (sf <fldinfo:fld>)
    (ar <expression:e> <expression:idx>)

    (ic <string:v>)
    (i64c <string:v>)
    (fc <string:v>)
    (dfc <string:v>)
    (sc <string:v>)
    (enumc <type:tp> <ident:val>)
    (null)
    (true)
    (false)

    (bin op <expression:l> <expression:r>)
    (un op <expression:v>)

    (type <type:tp> <expression:e>)
    (typecast <type:tp> <expression:e>)
    (typetoken <type:tp>)
    (fieldtoken <expression:e>)

    (typeof <expression:e>)
    (istype <expression:e> <type:tp>)

    (marshal <type:tp> <expression:e>)

    (embedded-begin <*mtdbody:bs> <expression:e>)
    (embedded-begin-1 <mtdbody:bdy>)

    ))

  (casebody (<constant:c> <mtdbody:body>))

  (constant
   (|
    (ic <string:v>)
    (fc <string:v>)
    (sc <string:v>)))

  (type
   (|
    (void)
    (ptr)
    (int)
    (short)
    (long)
    (char)
    (byte)
    (float)
    (double)
    (string)
    (object)
    (bool)
    (unknown)
    (this)
    (array <type:t>)
    (generic <string:path> . <*type:args>)
    (boxed <type:t>)
    (ref <type:t>)
    (T . <string:path>)))

  (mtdarg (<type:t> <ident:nm>))

  (access (| (private) (public) (protected) (static) (virtual)))

  (mtdinfo ((<type:t> <type:ret> . <*type:args>) <ident:name>))
  (fldinfo ((<type:t> <type:val>) <ident:name>))

  )

