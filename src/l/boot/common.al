;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \section{Stage 1 --- common to all future passes}
;-
;- This sequence controls the compilation of the core library.
;- It is used in interpretation stage as well as in all the consequent
;- compiled environment bootstrap builds.
;-

;- Compiler default options, etc.:
(include "../options.al")

;- By default, exit on a first error message
(ctimex (define debug-compiler-failure #t))

;- MBase way of communicating compiler internal errors, via MBaseException
(include "../core/ccerrors.al")

;- Parsing combinators library
(include "../core/parsing.al")

;- Basic pattern matching
(include "../core/pmatch.al")

;- Emitting IL via System.Reflection.Emit
(include "../core/emit.al")

;- There are two different modes of asmlib embedding. First is to compile it as a separate
;- dll, and second is to compile it into MBaseBin.dll in the last stage.

(expand-if (not (shashget (getfuncenv) 'asmlib-final))
  (include "../core/asmlib_common.al")
  (add-assembly "bootasm.dll")
  (define generic_pfx "bootasm.")
  (using ("bootasm")
         (include "../core/asmlib_load.al")))

(expand-if (shashget (getfuncenv) 'asmlib-final)
  (include "../core/asmlib_common.al")
  (define generic_pfx "MBaseBin.")
  (include "../core/asmlib_load.al"))

;- Common to both approaches are asmlib definitions:
(include "../core/asmlib_hooks.al")

;- Slightly misleading name: actually it is a global environment lock definition.
(include "../core/environment.al")

;- Several aux macros
(include "../core/utils.al")

;- A legacy macro, which may be possibly revived later.
(macro notaruntime rest `(top-begin ,@rest))

;- Support for documentation generation. It is actually needed only at the final
;- stage.
(include "../core/doc.al")

;- Basic support for records (array-based)
(include "../core/records.al")

;- The core AST processing language definition: the new compiler is based on it.
(include "../core/ast.al")

;- Simple lexer wrapper, on top of parsing combinators library.
(include "../core/lexing.al")

;- LL(*) parser, used by list comprehensions and infix syntax.
(include "../core/fsmparser.al")

;- List comprehensions library
(include "../core/list.al")

;- A simple environments handling library - used by the new compiler
(include "../core/envhandling.al")

;- Syntax case library
(include "../core/syntax-case.al")


; Register scheduling -
;   used only at the last stage
(ctime (if (shashget (getfuncenv) 'stage-final)
           `(include "../core/graphsort.al")
           `(begin)))

(include "../core/liveness.al")
(include "../core/coloring.al")