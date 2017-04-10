;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{05calc.al: register machine target}
;-

;- The most complicated form of compilation in this tutorial will be targeting
;- a register machine.
;-
;- We are using an imaginable architecture, which resembles the design of
;- some of the popular register architectures. It has only three floating
;- point registers called R1, R2 and R3. We will also refer to a frame pointer
;- FP and a stack pointer SP. Mov* instructions may have memory or register as destination.
;- If mov* instruction destination is a register, a source could be a constant.
;- For arithmetic operations, only registers could be destination and registers and memory could
;- be used as a source.
;-

;{{
(def:ast calc05 ( )
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
;- Compilation will need a number of intermediate representations. First
;- intermediate AST will be defined for ``imperativised'' version of a code,
;- distinguishing operators from expressions. The same
;- representation will be used for a flattened 3--address code as well.
(def:ast calc05x ( )
  (*TOP* <topexprs>)
  (topexprs <*topexpr:es>)
  (topexpr
   (|
    (def <ident:nm> <expr:val>)
    (return <expr:val>)))
  (expr
   (|
    (plus <expr:a> <expr:b>)
    (minus <expr:a> <expr:b>)
    (mult <expr:a> <expr:b>)
    (div <expr:a> <expr:b>)
    (var <ident:nm>)
    (const <number:v>))))

;- Nested [[let]]--s should be transformed into a flat sequence of
;- definitions. A ``splitter'' pattern is used here. As in [[02calc.al]],
;- the visitor stops on [[let]] nodes, and for each sub--expression [[loop]]
;- function produces a pair: a transformed sub--expression and a list of raised definitions.
;- Since only [[let]] can raise, it efficiently moves all the
;- variable declarations to the flat top level. This pattern is very common
;- in code transforms.
(function stage0 (ex)
  (alet res
   (let loop ((e ex))
    (collector (raiseadd raiseget)
     (cons ; return a pair: expression itself and raised definitions list
      (calc05:visit expr e
        (expr _
          ((let
               (let ((nval (loop val))
                     (nbody (loop body)))
                 (iter raiseadd (cdr nval))
                 (raiseadd `(def ,nm ,(car nval)))
                 (iter raiseadd (cdr nbody))
                 (car nbody)))
           (else-deep ((else node))))))
      (raiseget))))
   `(,@(cdr res)
     (return ,(car res)))))

;- [[stage1lift]] is a helper function which lifts all the complex sub--expressions of a
;- given expression, using a provided [[add]] collector. Another common
;- trick is used here: temporary node renaming. When a visitor starts on [[ex]],
;- it assumes that it is a node of a virtual type [[d_expr]], inheriting all
;- the properties of [[expr]]. But all the [[expr]] sub--nodes referenced from
;- this node are still of [[expr]] type, and processed with [[expr]] pattern.
;- This means that an outer expression is untouched, and all the sub--expressions
;- are raised unless they are variables. For example, {\tt (plus (const ...) (const ...))}
;- will be translated into {\tt (plus (var ...) (var ...)} with two raised variables
;- bindings for constants.
(recfunction stage1lift (add ex)
  (calc05x:visit d_expr ex
        ((d_expr expr) DEREF (forall node))
        (expr DEEP
           ((var node)
            (else (alet nm (gensym)
                        (add `(def ,nm ,(stage1lift add node)))
                        `(var ,nm)))))))

;- The next function will lift inner complex expressions in sequence:
;- innermost lifts will be processed first.
(function stage1 (tex)
  (collector (add get)
    (foreach (ex tex)
      (add (calc05x:visit topexpr ex
              (expr _
                    (forall (stage1lift add node))))))
    (get)))

;- [[stage2schedule]] function compiles a flat code into an MBase register scheduling
;- mini--language in order to
;- get an optimal variables distribution. It is easy since we only have
;- one datatype: a double precision floating point number.
(function stage2schedule (tex)
  (calc05x:visit topexprs tex
    (topexpr DEEP
      ((def `(genkill ((V ,nm double)) ,val))
       (return `(gen ,@val))))
    (expr DEEP
      ((const nil)
       (var `((V ,nm double)))
       (plus `(,@a ,@b))
       (minus `(,@a ,@b))
       (mult `(,@a ,@b))
       (div `(,@a ,@b))))))

;- Now we will rename variables using the provided register scheduling plan.
;- A context sensitive [[ast:mknode]]
;- macro is used here. It is useful only withing transforms that translate an AST into
;- another AST of the same structure. [[ast:mknode]] generates another instance of a currently processed node
;- or variant, changing only named sub--node values.
(function stage2 (tex)
  (let* ((plan (stage2schedule tex))
         (all (r3:allocateregisters nil (r3:lgraphs plan)))
         (newnm (fun (n) (hashget all n))))
    (calc05x:visit topexprs tex
       (expr DEEP
         ((var (ast:mknode (nm (newnm nm))))
          (else node)))
       (topexpr DEEP
         ((def (ast:mknode (nm (newnm nm))))
          (else node))))))

;= Perform all stages above in the right order:
(function compile (e)
  (stage2 (stage1 (stage0 e))))

;- Our imaginable register target machine has only 3 floating point registers,
;- and all other variables should be spilled into a stack frame.
;-
;- Here we will just count the register use frequency in order to decide what to
;- spill.
(function stage3count (es)
  (with-hash (ht)
    (calc05x:iter topexprs es
      (expr DEEP
         ((var (alet h0 (alet h00 (ht> nm) (if h00 h00 0))
                     (ht! nm (+ h0 1))))
          (else nil))))
;= At this point [[ht]] contains variable usage frequencies, so we can now sort the
;= list in the descending order.
    (qsort (fun (a b) (> (car a) (car b)))
           (hashmap (fun (nm cnt) `(,cnt ,(Sm<< nm))) ht))))

;- If we have just 3 variables, all are mapped to registers. Otherwise
;- one register is reserved for spills, and two others are bound. This is
;- not the most efficient way, but all the complex optimisations are
;- beyond the scope of this tutorial.
;-

;= In a simple case variables are just renamed to registers.
(function stage3rename (es mp)
  (with-hash (ht)
    (iter-over mp (fmt (nm vl) (ht! nm vl)))
    (calc05x:visit topexprs es
        (topexpr DEEP
          ((def (ast:mknode (nm (ht> nm))))
           (else node)))
        (expr DEEP
          ((var (ast:mknode (nm (ht> nm))))
           (else node))))))

;= Otherwise we have to perform a spilling. R3 is a temporary substitution for variables that
;= are spilled out.
(function stage3spills (es r1 r2 sps)
  (collector (add get)
  (with-hash (ht)
    (iter-over sps (fmt (nm num) (ht! nm `(SP ,num))))
    (ht! r1 'R1)
    (ht! r2 'R2)
    (calc05x:visit topexprs es
        (topexpr DEEP
           ((def (if (list? (ht> nm))
                     (begin
                       (add `(def R3 ,val))
                       (add `(mov R3 ,(ht> nm))))
                     (add (ast:mknode (nm (ht> nm))))))
            (else (add node))))
        (expr DEEP
           ((var (ast:mknode (nm (ht> nm))))
            (else node)))))
  (get)))

;= [[stage3]] function decides wheter it is renaming or spilling. It returns a pair:
;= null or a number of spilled variables and the generated intermediate code.
(function stage3 (es)
  (alet count (stage3count es)
    (cond
     ((< (length count) 4)
      (cons nil
       (stage3rename es (zip (map cadr count)
                             '(R1 R2 R3))))
      )
     (else
      (format count ((_ r1) (_ r2) . spilled)
        (cons (length spilled)
         (stage3spills es r1 r2
                       (zip (map cadr spilled)
                            (fromto 0 (length spilled)))))
        )))))

;- The code above has generated a slightly modified version of our AST, introducing
;- a new toplevel instruction, so here we are patching it to reflect this
;- changes.
(def:ast calc05t ( (calc05x) )
  (topexpr
   (|
    (mov <ident:n1> <ident:n2>)
    (def <ident:nm> <expr:val>)
    (return <expr:val>))))

;- [[stage4var]] emits a register or frame pointer
;- reference argument.
(function stage4var (nm)
  (p:match nm
    ((SP $cnt) ; spilled into stack
     (if (> cnt 0)
         (S<< "[FP:" (* 8 (cadr nm)) "]")
         "[FP]"))
    (else (S<< nm))))

;- Next function emits simple instructions.
(function stage4emit (instr tgt)
  (calc05x:visit d_expr instr
    ((d_expr expr) DEREF
      ((var
        (if (eqv? nm tgt) "NOP"
            (S<< "MOVR8 " (stage4var nm) ", " tgt)))
       (const (S<< "MOVR8 #F" v ", " tgt))
       (plus (S<< "ADDR8 " a ", " b ", " tgt))
       (minus (S<< "SUBR8 " a ", " b ", " tgt))
       (mult (S<< "MULR8 " a ", " b ", " tgt))
       (div (S<< "DIVR8 " a ", " b ", " tgt))))
    (expr DEEP
       ((var (stage4var nm))
        (else nil)))
    ))

;- And to bind everything together, to emit correct stack frame initialisation
;- and return statement:
(function stage4 (cnt es)
  (collector (add get)
    (if cnt (begin
              (add "MOVL SP, FP")
              (add (S<< "ADDL SP, " (* 8 cnt) ", SP"))))
    (foreach (e es)
      (calc05t:iter topexpr e
        (topexpr _
         ((mov (add (S<< "MOVR8 " (stage4var n1)
                         ", "  (stage4var n2))))
          (def (add (stage4emit val nm)))
          (return (begin
                    (add (stage4emit val 'R1))
                    (if cnt (add "MOVL FP, SP"))
                    (add "PUSHR8 R1")
                    (add "RET")))))))
    (get)))

;- [[emit]] is the interface function which takes a parsed AST and
;- returns a list of assembler language strings.
(function emit (src)
  (alet c (stage3 (compile src))
     (stage4 (car c) (cdr c))))

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
   ((MINUS fact:e)          `(minus (const "0") ,e)))
  )
;}}

;- And we can now print out the compiled code. Unlike other examples,
;- there is no actual
;- evaluation here, our target machine does not exist in reality.
(define code
  (S<< "let x = let t = 1+1+1 in t*t in "
         "let y = 1.1+(-x*t) in "
         "(1-y)+x/2+x*x-(2+x)/y"))

(define src
  (lex-and-parse calclexer calcparser code))

(iter println (emit src))
