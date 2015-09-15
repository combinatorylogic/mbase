;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(n.module Tutorial exe) ;; to build an .exe module

;; Ignore this please:
(define chN (cons nil 0))
(macro /@ rest
  (set-cdr! chN (+ (cdr chN) 1))
  `(println (S<< ,(S<< "\nChapter " (->s (cdr chN)) ":\n\t") ,(strinterleave (map to-string rest) " ") "\n")))
(macro // rest `(println ,@rest))
(macro /// rest `(print ,@rest))
;;;;;;;;;;;;;;;;;;;;;;

(/@ Arithmetics.)
;
;   Arithmetic operations are functions, so a function application syntax
;   should be used: (<function> <argument>*)

(/// "2+2=")
(// (+ 2 2)) ; applying function '+' to constant arguments 2 and 2.

(/// "(2*10)+15=")
(// (+ (* 2 10) 15)) ; function arguments are expressions

(/@ Defining functions.)

(function square (x) (* x x))  ; defining a new function

(/// "2^2=")
(//  (square 2))

(/// "10^2 + (2+2)^2=")
(//  (+ (square 10) (square (+ 2 2))))

(/@ Variables.)

; In the following example variables are defined in the same context. It means that we can not use 'a' value in the definition of 'b'.
(let ((a 2)
      (b 8))
  (/// "a*b=")
  (//  (* a b)))

; To define variables in a sequence (allowing reference to local names previously bound in the same block of declarations) you can use 'let*':

(let* ((a 2)
       (b (* a 2))
       (c (* b 2))
       (d (* c 2)))
   (///  "d=") (// d))

; To define a global variable, use (define ...):

(define Hundred 100)

(///  "Doubled hundred=") (// (* Hundred 2))

; Please note - both local and global variables are read-only. Redefinition will not affect any of the previous values.

(function testHundred (x) (* x Hundred))

(define Hundred 55)

(/// "1000 or 550? ") (// (testHundred 10))
(/// "New hundred= ") (// Hundred)

(/@ Lists.)

; To suppress the evaluation of expression, use a quotation mark:

(/// "List: ")
(writeline '(1 2 3 4 5))

(/// "Another list: ")
(writeline '(+ 2 2))

; Lists are built of the "cons" cells (AKA pairs).
; The constant pair syntax is as follows: (<head> . <tail>)

(/// "Pair: ") (writeline '(1 . 2))

(/// "(1 . (2 . (3 . (4 . ())))) =") (writeline '(1 . (2 . (3 . (4 . ())))))

; As you can see from this example, lists are a sort of syntax sugar, which allows you to enter easily the sequence of pair tails.

(/// "(1 . ((2 . ()) . 3))=") (writeline '(1 . ((2 . ()) . 3)))

; There are three basic functions to operate the lists: 'cons' which constructs a pair,
;  'car' which returns a pair head, and 'cdr' returning a tail.

(/// "Head of (1 2 3)=") (writeline (car '(1 2 3)))

(/// "Tail of (1 2 3)=") (writeline (cdr '(1 2 3)))

(/// "Cons cell made of 1 and (2 3): ") (writeline (cons 1 '(2 3)))

; There is also a number of derivative functions. For example, 'car' of 'cdr' is called 'cadr',
; 'cdr' of 'car' - 'cdar'. So, to get the 3rd element of the list, use 'caddr' function.

(/// "caddr of '(1 2 3 4) is: ") (writeline (caddr '(1 2 3 4)))

(/// "third tail of '(1 2 3 4 5) is: ") (writeline (cdddr '(1 2 3 4 5)))

(/@ Recursion and iteration.)

; The following function will count the number of elements of the list:

(recfunction list-length (lst)
   (if (null? lst) 0
       (+ 1 (list-length (cdr lst)))))

; Here the predicate function 'null?' returns true when argument is a null pointer.
; Null is normally represented as '()'.
; 'if' construction has 2 or 3 arguments: (if <condition> <iftrue> [<iffalse>])

(/// "length of '(1 2 3 4 5) is: ") (// (list-length '(1 2 3 4 5)))

; Now, what will happen if the list passed to 'list-length' is REALLY long?
; Obviously, we will simply run out of stack space - 'list-length' will first reach the
; end of the list, and then go back, adding 1's for each level of recursion.
;
; To avoid this, you must use tail recursion - i.e., the recursive function call
; value must be returned immediately.

(recfunction list-length-inner (acc lst)
  (if (null? lst)
      acc
      ;; otherwise
      (list-length-inner (+ acc 1) (cdr lst))))

(function list-length (lst) (list-length-inner 0 lst))

(/// "length of long list is: ") (// (list-length (formap (i 1 1000) i)))

; Here we had to define two functions. But it is possible to define a recursive function
; inside any expression:

(function list-length (lst0)
  (let list-length-inner ((acc 0)
                          (lst lst0))
    (if (null? lst)
        acc
        ;; otherwise
        (list-length-inner (+ acc 1) (cdr lst)))))

(/// "length of long list is: ") (// (list-length (formap (i 1 1000) i)))

; In trivial cases there is no need to use the recursion directly. There is a set
; of iterators and special forms to choose from.

(/// "list of numbers from 1 to 100: ")
(writeline (formap (i 1 101) i))

(/// "5 stars: ") (for (i 0 5) (print "*")) (println "")

(/// "list of some even numbers: ")
(writeline (map (fun (x) (* x 2))
                (formap (i 0 11) i)))

(/// "Sum of numbers from 5 to 10: ")
(writeline (foldl + 0 (formap (i 5 11) i)))

(/// "Product of numbers from 5 to 10: ")
(writeline (foldl * 1 (formap (i 5 11) i)))

; So we can use 'map', 'iter', 'iteri', 'foldl', 'foldr' iterators
; and 'for' and 'formap' macros to express an iteration without recursion.

; For example, the list-length function can be rewritten as follows:
(function list-length (lst)
   (foldl (fun (acc el) (+ acc 1)) 0 lst))

(/// "List length again: ") (// (list-length (formap (i 0 1000) "AAA")))

(/@ More lists.)

; We have learned how to compose and decompose lists using basic functions and c**r
; derivatives. Now let's consider some higher level constructions:

(/// "a+b for the list (x y (a) . b): ")
(// (format '(0 1 (2) . 3) (_ _ (a) . b) (+ a b)))

; 'format' macro provides an easy way to decompose lists and to access their elements
; without building lengthy c**r sequences. Now let's compose a list:

(/// "Composed list: ")
(writeline `(This list is composed. 2 + 2 = ,(+ 2 2)))

(/// "Another one composed list: ")
(writeline
   `(A list of some odd numbers:
      ,@(formap (i 0 10) (+ 1 (* i 2))) and more of this kind))

; Here we have used a quasiquote. It works like a normal quote, but you can turn the evaluation
; back again by unquoting or splice unquoting (, and ,@ prefixes).

(/@ Macros.)

; Since the expressions of the language are lists, we can apply our skills in lists
; composing and decomposing and write the code programmatically. We will use macros
; for it.

(macro cut expression
  (let* ((slots (filter (fun (x) (eq? x '<>)) expression))
         (nslots (length slots))
         (args (formap (i 0 nslots) (gensym))))
    `(lambda ,args
        ,(let loop ((e expression) (a args))
            (if (null? e) nil
                (if (eq? (car e) '<>)
                    (cons (car a) (loop (cdr e) (cdr a)))
                    (cons (car e) (loop (cdr e) a))))))))

; To see how your macro works you can use 'expand' function:
(/// "(cut * 2 <>)=") (writeline (expand '(cut * 2 <>)))

; An example of the macro 'cut' application:

(/// "Odd numbers ")
(writeline
  (map (cut * 2 <>) (formap (i 0 10) i)))

; Another example, this time a simple one:

(macro example (description expression)
   `(begin ,@(if description `((println ,description)) nil)
           (print (to-string (quote ,expression)))
           (print "  =   ")
           (writeline ,expression)))

(example "Arithmetic example: " (+ 2 2))
(example () (formap (i 0 10) '*))

(example "Macro expansion:" (expand '(let ((a 2) (b 2)) (+ a b))))
(example "Macro expansion:" (expand '(let* ((a 2) (b 2)) (+ a b))))

; A more complicated example: contract programming.

(macro cfunction (name args contract . body)
   (let* ((argcs (filter list? args))
          (argz (map (fun (x) (if (list? x) (car x) x)) args))
          (argver `(and ,@(map cadr argcs)))
          (msg (string-append "Error in " name))
          (rname (gensym)))
     (println (buildstring "Defining function with contract: " name))
     `(function ,name ,argz
        (begin
           (if ,argver
             (let ((,rname (begin ,@body)))
               (if (,contract ,rname)
                   ,rname
                   (begin (println ,msg) nil)))
             (begin (println ,msg) nil))))))

; Now note that "Defining function..." message appears only during compilation.
; If you run the compiled code later, you will not see the message.

(cfunction test ((i (> i 0))) (cut > <> 0)
    (* 8 (- i 10)))

(example () (test 4))
(example () (test 25))
(example () (test 0))

; That is how the language can be extended. In fact, almost all the constructions of
; the language are implemented as macros. The core language itself is very simple
; and limited.

(/@ Parsing.)


; It is common to use recursive descent parsing to implement a lexing
; pass.
(define lexr (<r> (  ("("   T> LB I) ;; constant tokens
                   | (")"   T> RB I)
                   | (",@"  T> UNSP I)
                   | (","   T> UNQ I)
                   | ("'"   T> QUO I)
                   | ("`"   T> BQUO I)
                   | (_ p.space )  ;; ignore whitespaces
                   | ("."   T> DOT I)
                   | ((p.alpha +*)             ;; idents
                                        T> SYM list->symbol))
                  ))

; For parsing pass we will use an embedded LL(1) parsers generator:
(bnf-parser
    ((nde parse-sexpr))
    (nde
       ((UNSP nde:x)       `(unquote-splicing ,x) )
       ((UNQ  nde:x)       `(unquote ,x)          )
       ((QUO  nde:x)       `(quote ,x)            )
       ((BQUO  nde:x)      `(quasiquote ,x)       )
       ((SYM:x)             x                     )
       ((LB RB)             nil                   )
       ((LB ndx:x RB)       x                     ))
    (ndx
       ((nde:a DOT nde:b)   (cons a b)            )
       ((nde:a ndx:b)       (cons a b)            )
       ((nde:x)             (list x)              ))
    )

(example () (lex-and-parse lexr parse-sexpr "(a b (c . d)  `(,@o . e))"))
; Lexers and parsers may return errors:
(example () (try (lex-and-parse lexr parse-sexpr "(a b . (.) . c)")
                 t_MBaseException
                 mbaseerror))
(example () (try (lex-and-parse lexr parse-sexpr "(a & b)")
                 t_MBaseException
                 mbaseerror))

(/@ More macros.)

; quicksort with 'do-where' macro

(recfunction sort (cf lst)
  (if (null? lst) nil
      (do (append (sort cf (car lr)) (cons a (sort cf (cdr lr))))
          (where (a (car lst)) (b (cdr lst))
                 (lr (tailsplit (cut cf <> a) b))))))

;; 'sort' function is naturally polymorphic:
(example () (sort < '(9 7 3 4 5 7 3 1 4 9 2 3)))

(example () (sort string<?
                  '(
                    "Some" "words" "to" "sort" "in" "an" "alphabetical" "order"
                   )))


; simplified quick sorting with list comprehensions

(recfunction qsort2 (lst)
  (p:match lst
    (() nil)
    (($piv . $rst)
     `( ,@(qsort2 (<L> l | l <- rst & (<= l piv)))
        ,piv
        ,@(qsort2 (<L> r | r <- rst & (>  r piv)))))
    (else lst)))

(example () (qsort2 '(9 8 7 6 5 7 8 9)))

; more list comprehensions

(function fff (n)
  (<L> `(,a ,b ,c) | a <- (<..> 1 n) | b <- (<..> a n) | c <- (<..> b n)
         & (eq? (* c c) (+ (* a a) (* b b)))))

(example ()
   (fff 100))

; letrec macro

(example ()
   (letrec ((a (fun (x) (print 'a) (if (> x 1) (b (- x 1)))))
            (b (fun (x) (print 'b) (if (> x 1) (a (- x 1))))) )
     (a 10) 'done))

; CPS-Basic, just for fun

(letrec (
         ( LINE-10 (fun () (set! a 0)           (LINE-15)) )
         ( LINE-15 (fun () (set! a (+ (! a) 1)) (LINE-20) ))
         ( LINE-20 (fun () (print  HELO)        (LINE-30)) )
         ( LINE-30 (fun () (println  "world!")  (LINE-40)) )
         ( LINE-40 (fun () (if (> (! a) 3)      (LINE-50)
                                                (LINE-15) )))
         ( LINE-50 (fun () nil))
         ( HELO    "Hello, ")
        )
   (LINE-10) )

(/@ Embedded Prolog.)

; Append

(define query "append([1,2,3],X,[1,2,3,3,2,1]).")

(/// "(append '(1 2 3) X) = '(1 2 3 3 2 1); X = ")
(format
 (simple-prolog nil query)
 (((_ x)))
 (// (prolog-print x)))


(/@ Abstract Syntax Trees: another kind of declarative programming.)

; N.B.: AST definitions exist in compilation time context only.
;       You need to include them in any new module.

(def:ast arithm ()
  (*TOP* <expr>) ;; always define a 'TOP' pseudo-node, only nodes related to
                 ;;  'TOP' will be included!
  (expr
     (| (APP fn <expr:e1> <expr:e2>)
        (VAR name)
        (CONST val))))

; This is a simple stack machine compiler for the 'arithm' AST.
(define avisitor
  (ast:visit arithm expr
     (expr DEEP  ;; the left-depth-first rule to be used
       ((CONST   `((push ,val)))
        (VAR     `((pvar ,name)))
        (APP     `(,@e1 ,@e2 (call ,fn))) ))))

(example () (avisitor '(APP + (CONST 2) (APP * (APP / (VAR x) (VAR y)) (CONST 8)))))


(/@ Humor: playing Peano with decimals.)

; Definitions for Peano numbers and operations on them

(define Zero nil)
(function Succ (x) (cons nil x))
(define One (Succ Zero))

(function IFun (acc _) (Succ acc))
(function DFun (acc _) (cdr acc))

(define Plus append)

(function Mult (a b)
   (foldl (fun (acc _) (Plus b acc)) nil a))

(function Minus (x y) (foldl DFun x y))
(function MinusOne (x) (cdr x))

(function Div (x y)
  (let loop ((t x)
             (n Zero))
     (let ((z (let loop1 ((v t)
                          (z y))
                 (if (null? v) (cons nil z)
                   (if (null? z) (cons v nil)
                       (loop1 (cdr v) (cdr z))
                       )))))
       (if (null? (cdr z))
           (if (null? (car z)) (cons (Succ n) nil)
                               (loop (car z) (Succ n)))
           (cons n t)))))

; An ordered list of known decimal digits:
(define digits '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

; Decimal system base:
(define Ten (foldl IFun Zero digits))

(function ToDigit (x)
   (car (foldl DFun digits x)))

(function FromDigit (x)
   (foldl IFun Zero (map-stop (cut eq? <> x) digits)))

; Parse a decimal string into a Peano number
(function ParseNumber (str)
  (cdr (foldl (fun (acc dig)
               (cons (Mult (car acc) Ten)
                     (Plus (cdr acc) (Mult (car acc) dig))))
              (cons One Zero)
              (map (@ FromDigit char->string) (reverse (string->list str))))))

(function DecimalNumber (x)
  (foldl string-append ""
         (let loop ((p One)
                    (t x)
                    (r nil))
            (if (null? t) (if (null? r) '("0") r)
              (format (Div t Ten) (nt . dig)
                 (loop (Mult p Ten)
                       nt
                       (cons (ToDigit dig) r)))))))

; Demo usage:
(recfunction Fact (x)
   (if (null? x) One
                 (Mult x (Fact (MinusOne x)))))

(print "9! = ")
(println (DecimalNumber (Fact (ParseNumber "9"))))

(print "125 * 987 = ")
(println (DecimalNumber (Mult (ParseNumber "125")
                              (ParseNumber "987"))))

(/@ Inline assembly.)

; We use 'ctime' macro here to evaluate method and type refererences during compilation.
(function asmtest (x)
 (ctime
   `(n.asm (x)
      (Ldstr "...")
      (expr x)
      (Castclass ,t_string)
      (Call ,(r_mtd "System.String" "Concat" t_string t_string))
      (Call ,(r_mtd "System.Console" "WriteLine" t_string))
      (Ldnull)
    )
 ))

(asmtest "Test!")


(/@ Inline higher level CLI)

(function notnettest ( s x)
  (not.nethr ((int x) (string s))
     (System.Console@WriteLine (concat s "0x" ((* x x)@ToString "X")))
     (return (new Meta.Scripting.Pair ((object)x) ((object)(+ x x))))
     )
  )

(println "Not.Net mini-language:")
(writeline (notnettest "String: " 10))

(using (System Meta.Scripting)

 (not.class SimpleStructure  (extend ValueType)
    (field int x (public))
    (field int y (public))
    )

 (not.class SimpleClass
    (method ((public) (static)) object test ((int i))
       (s1 = (new SimpleStructure))
       (s1#x <- i)
       (s1#y <- (* 2 (s1#x)))
       (print (concat "s1: " ((s1#x)@ToString) "," ((s1#y)@ToString)))
       (print ((Math@Sin (* d3.14 d2.1))
               @ToString "e3"))
       (return ((object)s1))
      ))

)

(writeline ((r_tsbind C_SimpleClass "test" int) 25))

(/@ High order functions.)

; Since functions and closures are first class objects here, we can build new functions from the
; existing ones.

; 'mkmultiplier' returns a function which multiplies its argument by n.
(function mkmultiplier (n)
   (fun (x) (* x n)))

(example "Closure:" (map (mkmultiplier 5) '(1 2 3)))

; functions can be composed:
(example "Composition:" (map (@ (mkmultiplier 5) (cut - 0 <>)) '(1 2 3)))

; and despatched, of course:

(function despatch (sym)
  (case sym
   ((+) +) ((*) *) ((/) /) ((-) -) ))

(function polish (lst)
   (car (foldl (fun (stk op)
                    (if (symbol? op)
                        (format stk (a b . rst)
                                (cons ((despatch op) b a) rst))
                        (cons op stk)))
               nil
               lst)))

(example "RPN calculator:" (polish '(2 2 + 2 * 10 -)))

(/@ More imperative coding.)

(function primes (N) ;; not a very efficient implementation
  (let* ((v (mkvector (fromto 0 N)))
         (I (/ N 2)))
    (for (i 2 I)
        (for (k (+ i i) N i) (aset v k 0)))
    (filter (cut > <> 0) (a->l v))) )

(example "Prime numbers:" (primes 100))


(recfunction sieve (l)
  (p:match l
    (($hd . $tl)
     `(,hd
       ,@(sieve (<L> n | n <- tl & (> (% n hd) 0)))))
    (else nil)
    ))

(function funprimes (N)
  (sieve (fromto 2 (+ N 1))))

(example "Functional prime numbers:" (funprimes 100))

