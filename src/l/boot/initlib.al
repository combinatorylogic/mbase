;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;
;; Library of useful macros and
;; functions which are not required for a bootstrap-1.
;;;;;;;;;;;;;

;; Bootstrap sequence, once boot.al is loaded:
;;
;;   boot.alc,initlib.al -> lib.alc
;;   boot.alc,lib.alc,dotnetlib.al -> lib1.alc
;;   boot.alc,lib.alc,lib1.alc,makeasmlib.al -> initial asmlib
;;   boot.alc,lib.alc,lib1.alc,common.al -> libc.alc
;;   boot.alc+lib.alc+lib1.alc+libc.alc = boot2.alc
;;   boot2.alc,stagec.al -> libnet.alc
;;   boot2.alc,libnet.alc,stage2.al -> boot2c.dll
;;   boot2c.dll,libnet.alc,stage3.al -> boot3.dll
;;   boot3.dll,stage4.al -> boot4.dll
;;   boot4.dll,stage5.al -> MBaseBin.dll

;; A very important metaprogramming feature: substitutes inplace with the value of the inner
;; expression. N.B.: an inner expression is always interpreted, even in the natively compiled
;; world.
(macro ctime (expr)
  (read-int-eval expr))

;; Quote the result of the expression evaluation. Useful sometimes.
(macro ctimeq (expr)
  (list 'quote (read-int-eval expr)))

;; Evaluate (interpretation!!!) something in the compilation time and substitute with nothing.
;; Useful to define macros you don't want to go into the compiled code;
(macro ctimex (expr)
  (begin (read-int-eval expr) `(begin)))

;; let's redefine length immediately

(function length (lst)
  (let loop ((l lst) (n 0))
    (if (null? l) n
        (loop (cdr l) (+ n 1)))))

(unit-test 1 (length '(1 2 3 4)) 4)
(unit-test 1 (length '()) 0)
(unit-test 1 (length '(a b)) 2)

(macro force-class-flush _
  `(begin))

(define I (lambda (BLOCK) BLOCK))

(unit-test 1 (I 1) 1)

;; temporary ccerror implementation
(function ccerror (arg)
   (println (to-string `(ERROR: ,arg))))

(macro assert (cnd)
  (if ##option-asserts
      `(if ,cnd nil (ccerror (list 'ASSERT: (quote ,cnd))))
      '(Nop)))

(macro generic-assert (messg cnd)
  (if ##option-asserts
      `(if ,cnd nil (ccerror (list (quote ,messg) (quote ,cnd))))
      '(Nop)))

(function assert-function (nm x)
  (let* ((cnd (cadr x)))
    `(generic-assert ,(string->symbol (string-append "ASSERT-AT-function:" (any->string nm)))
                     ,cnd)))

(function assert-macro (nm x)
  (let* ((cnd (cadr x)))
    `(generic-assert ,(string->symbol (string-append "ASSERT-AT-macro:" (any->string nm)))
                     ,cnd)))

(macro with-macros (ml . body)
  (let* ((macs (mkshash))
         (rie (shashget (getfuncenv) 'read-int-eval)))
    (map (lambda (x)
            (let* ((nam (car x))
                   (val (cadr x)))
              (shashput macs nam (rie val))))
          ml)
    `(inner-expand-with ,macs (begin ,@body))))

(macro #eval (rest)
  `(read-int-eval ,rest))

(macro expand-if (cnd . body)
  (let ((vlu ((shashget (getfuncenv) 'read-int-eval) cnd)))
    (if vlu `(top-begin ,@body) '(top-begin ))))

(macro expand-if-else (cnd  btru bfals)
  (let ((vlu ((shashget (getfuncenv) 'read-int-eval) cnd)))
    (if vlu btru bfals)))

(macro cli-only body
  `(expand-if (shashget (getfuncenv) 'compiled-environment) ,@body))

(macro int-only body
  `(expand-if (not (shashget (getfuncenv) 'compiled-environment)) ,@body))

(macro cli-mode (cbody _else_ ibody)
  `(expand-if-else (shashget (getfuncenv) 'compiled-environment)
                   (top-begin ,@cbody)
                   (top-begin ,@ibody)))

(int-only
  (macro topblock body `(top-begin ,@body))
  )

(macro ageto rest `(aget ,@rest))

(define *documentation* (mkhash))


(cli-mode
 (
  (function doc.add (channel rest)
    (hashput *documentation* 'D (cons (list channel rest) (hashget *documentation* 'D)))
    nil
    )
  )
 else
 (
  (function doc.add (channel rest)
    nil)
  )
 )



(macro Section rest
  (doc.add 'def `(section ,@rest))
  '(begin)
)

(macro Par rest
  (doc.add 'def `(par ,@rest))
  '(begin)
)

(function doc.isdoc? (body)
  (if (> (length body) 1)
      (if (not (null? (car body)))
          (cond
            ((string? (car body)) (list (car body)))
            ((and (list? (car body)) (string? (caar body))) (car body))
            (else nil)))))

(function cuttail (lst)
  "Returns the copy of the list [lst] without the last head."
        (let loop ((l lst))
                (if (null? l) nil
                        (if (null? (cdr l)) nil
                                (cons (car l) (loop (cdr l)))))))

(topblock
  (define *current-module-env* (noconst (cons nil nil)))
  (function bootlib:name-in-a-module-path (path nm)
    (string->symbol
     (let loop ((p path) (s (symbol->string nm)))
       (if p
           (string-append (string-append (car p) ":")
                          (loop (cdr p) s))
           nm))))
  (function bootlib:name-in-a-module (nm)
    (let* ((path (car *current-module-env*))
           (exports (if (cdr *current-module-env*)
                        (car (cdr *current-module-env*))
                        nil)))
      (if path
          (if (memq nm exports)
              (bootlib:name-in-a-module-path path nm)
              (bootlib:name-in-a-module-path (append path '(private)) nm)
              )
          nm)))
  (function bootlib:push-module-namespace (nm)
    (set-car! *current-module-env* (append (car *current-module-env*) (cons nm nil)))
    (set-cdr! *current-module-env* (append (noconst (cons nil nil))
                                           (cdr *current-module-env*))))
  (function bootlib:pop-module-namespace ()
    (set-car! *current-module-env* (cuttail (car *current-module-env*)))
    (set-cdr! *current-module-env* (cdr (cdr *current-module-env*))))
  (function bootlib:add-exports (lst)
    (set-car! (cdr *current-module-env*)
              (append lst (car (cdr *current-module-env*)))))
  (function bootlib:get-module-namespace ()
    (car *current-module-env*))

)

; extend some core macros
(topblock
(macro function (nm0 args . body)
  (let ((nm (name-in-a-module nm0))
        (d (doc.isdoc? body)))
    `(with-macros ((assert (lambda (x) (assert-function (quote ,nm) x) ))
                   (this-context (lambda (x) (quote (quote (function ,nm))))))
        ,(if d
             (begin
               (doc.add 'def `(function ,nm ,args ,@d))
               `(core.function ,nm ,args ,@(cdr body)))
             `(core.function ,nm ,args ,@body)))
    ))

(macro recfunction (nm0 args . body)
  (let ((nm (name-in-a-module nm0))
        (d (doc.isdoc? body)))
    `(with-macros ((assert (lambda (x) (assert-function (quote ,nm) x)))
                   (this-context (lambda (x) (quote (quote (function ,nm))))))
       ,(if d
            (begin
              (doc.add 'def `(function ,nm ,args ,@d))
              `(core.recfunction ,nm ,args ,@(cdr body)))
            `(core.recfunction ,nm ,args ,@body)))))

(macro macro (nm0 args . body)
  (let ((nm (name-in-a-module nm0))
        (d (doc.isdoc? body)))
    `(with-macros ((assert (lambda (x) (assert-macro (quote ,nm) x)))
                   (this-context (lambda (x) (quote (quote (macro ,nm))))))
       ,(if d
            (begin
              (doc.add 'def `(macro ,nm ,args ,@d))
              `(core.macro ,nm ,args ,@(cdr body)))
            `(core.macro ,nm ,args ,@body)))))

(macro define (nm0 . body)
  (let ((nm (name-in-a-module nm0))
        (d (doc.isdoc? body)))
     (if d
        (begin
          (doc.add 'def `(define ,nm ,@d))
          `(core.define ,nm ,@(cdr body)))
       `(core.define ,nm ,@body))))
)

(Section "Essential \\LonePrim{} definitions")

(Par
 "Here follows some essential definitions that are required by the"
 "consequent \\NET{} bindings initialisation code, so no logic or structure is present here yet."
 )

(topblock
(macro buildstring lst
  "Creates a string builder for a given list of arguments."
   (if (null? lst) ""
       (if (null? (cdr lst)) `(build-any->string ,(car lst))
           `(string-append (build-any->string ,(car lst)) (buildstring ,@(cdr lst))))))

(unit-test 1 (buildstring "a" "b" "c") "abc")

(macro build-any->string (arg)
  ("Same as buildstring, but all non--string arguments are"
   "wrapped into [any->string].")
  (if (string? arg) arg
      `(any->string ,arg)))

(function char->string (ch)
  "Makes a string of one char."
  (list->string (list ch)))

(define Nop (lambda () nil))
)

(topblock

(macro fun (args . body)
  "A shorter form for lambda."
  `(lambda ,args (begin ,@body)))

;; a classic functional multiplication
(function @ (f g)
    "Functional composition: returns a function $\\lambda~x~.~f~(g~x)$"
   (lambda (x) (f (g x))))

;; generic iteration
(recfunction iter (f l)
  "Imperative iteration, applying [f] to all the [l] elements."
   (if (null? l) nil (begin (f (car l)) (iter f (cdr l)))))

;; left-fold
(recfunction foldl (f i l)
  "Folds [l] with a given [f] and an initial accumulator value [i]."
   (if (null? l) i (foldl f (f i (car l)) (cdr l))))

(unit-test 1 (foldl * 1 '(1 2 3 4 5)) 120)

;; right-fold
(recfunction foldr (f i l)
  "Right-folds [l] with a given [f] and an initial accumulator value [i]."
  (if (null? l) i
      (f (car l) (foldr f i (cdr l)))))

(unit-test 1 (foldr * 1 '(1 2 3 4 5)) 120)

;; toplevel filtering
(recfunction filter (f l)
  "Filters a list using a given predicate function."
   (if (null? l) nil
      (if (list? l)
        (let ((cl (car l)))
          (if (f cl) (cons cl (filter f (cdr l))) (filter f (cdr l))))
        nil
        )))

(unit-test 1 (filter (fun (x) (> x 1)) '(0 10 1 -5 20 2)) (10 20 2))

(recfunction find (f l)
  "Returns the first value conforming to a given predicate or nil."
   (if (null? l) nil
       (if (list? l)
           (if (f (car l)) (car l)
               (find f (cdr l))))))

(unit-test 1 (find (fun (x) (> x 1)) '(0 -5 1 4 5 6 20)) 4)


(recfunction lasthead (l)
  "Returns the last head of a list or nil."
  (cond
    ((null? l) l)
    ((null? (cdr l)) (car l))
    (else (lasthead (cdr l)))))

(unit-test 1 (lasthead '(1 2 3 4)) 4)


;; flatten a list to one single level
(recfunction flatten (l)
  "Returns a flat list of all atoms in [l]."
  (cond
    ((null? l) nil)
    ((list? l)
     (if (list? (car l))
           (append (flatten (car l)) (flatten (cdr l)))
           (cons (car l) (flatten (cdr l)))))
    (else l)))

(unit-test 1 (flatten '((((1) 2 ((3) 4)) 5) 6)) (1 2 3 4 5 6))

(recfunction first (i l)
  "Returns first [i] elements of a given list [l]."
   (if (> i 0) (cons (car l) (first (- i 1) (cdr l)))
               nil))

(unit-test 1 (first 3 '(a b c d e)) (a b c))

;;  zip two lists (using cons on elts)
(recfunction czip (a b)
  "Returns the list of ($a_i$ . $b_i$) for all elements of [a] and [b]."
   (if (or (null? a) (null? b)) nil
       (cons (cons (car a) (car b)) (czip (cdr a) (cdr b)))))

(unit-test 1 (czip '(1 2 3) '(a b c)) ((1 . a) (2 . b) (3 . c)))

(recfunction zip (a b)
  "Returns the list of ($a_i${} $b_i$) for all elements of [a] and [b]."
   (if (or (null? a) (null? b)) nil
       (cons (list (car a) (car b)) (zip (cdr a) (cdr b)))))

(unit-test 1 (zip '(1 2 3) '(a b c)) ((1 a) (2 b) (3 c)))

(unit-test 1 (cuttail '(1 2 3)) (1 2))

(recfunction lasttail (a)
  "Returns the last non--[nil] tail of the list [a]."
  (if (null? a) nil
    (if (null? (cdr a)) a
       (lasttail (cdr a)))))

(unit-test 1 (lasttail '(1 2 3)) (3))

;; imperative iteration with a counter
(function iteri (f l)
  ("Performs an imperative iteration over [l] elements, giving an"
   "element number as the first argument to the function [f].")
   (let loop ((i 0) (x l))
      (if (null? x) nil
          (begin
             (f i (car x))
             (loop (+ i 1) (cdr x))))))


(function mapi (f l)
  "Maps [l] elements via f($i$,$l_i$) function, where $i$ is an element number."
   (let loop ((i 0) (x l))
      (if (null? x) nil
          (cons
             (f i (car x))
             (loop (+ i 1) (cdr x))))))

(recfunction nth (i l)
 "Returns an [i]'th element of the list [l]."
  (if (= i 0) (car l) (nth (- i 1) (cdr l))))

(unit-test 1 (nth 2 '(x a b c d)) b)

;; shortcuts to improve code readability in case of a lengthy iterator function
(function iter-over (l f) "[iter] with swapped arguments." (iter f l))
(function map-over (l f) "[map] with swapped arguments." (map f l))

(unit-test 1 (map-over '(1 2 3) (fun (x) (* x x))) (1 4 9))

;; gensym with a counter.

(define *gensym-counter-storage*
  (noconst
   (cons nil
        (ctime
         (let* ((tst
                (shashget (getfuncenv) '*gensym-counter-storage*)))
           (if tst (+ 100 (cdr tst)) 10000))))))

(function gensym-counter-set (n)
  (let ((st (shashget (getfuncenv) '*gensym-counter-storage*)))
    (if (> n (cdr st))
        (set-cdr! st n)
        (set-cdr! st (+ 10000 (+ (cdr st) n))))))

(function gensymp ( pfx )
  (let ((st (shashget (getfuncenv) '*gensym-counter-storage*)))
    (let ((x (+ 1 (cdr st))))
      (set-cdr! st x)
      (string->symbol (string-append pfx (number->string x))))))

(function gensym ()
  ("Returns a unique symbol every time it is called."
   "Uniqueness is guaranteed within one run only.")
  (let* ((dpfx0 (shashget (getfuncenv) '*globals*))
         (dpfx1 (if (null? dpfx0) "Z"
                    (let ((nn (hashget dpfx0 '_global_mname)))
                      (if nn nn "Z")))))
    (gensymp dpfx1)))

;; case macro - Scheme-like behaviour

(macro gencase (expr . cases)
  (let ((a (gensym)))
    `(let ((,a ,expr))
       (cond
        ,@(map
           (lambda (cas)
             (let ((cs (car cas)))
               `(
                 ,(cond
                   ((eqv? cs 'else) 'else)
                   ((symbol? cs) `(eqv? ,a (quote ,cs)))
                   ((null? (cdr cs)) `(eqv? ,a (quote ,(car cs))))
                   ((list? cs) `(or ,@(map (fun (x)
                                             `(eqv? ,a (quote ,x))) cs)))
                   (else 'CASE-ERROR))
                 ,@(cdr cas))))
           cases)))))

(macro case (expr . cases)
  ("Selects an action depending on [expr] symbol value "
   "(using [eqv?] to compare)."
   "[["
   "<case>:"
   "   ((<symbol>*) <expression>*)"
   " | (else <expression>*)"
   "]]"
   )
  `(gencase ,expr ,@cases))

(unit-test 1 (case 'a ((x y) 1) ((c d) 2) ((a b) 3)) 3)

)

;; redefine c**r's

  (ctimex
   (function mkc**r (pth)
    (string->symbol
      (buildstring "bootlib:c" (foldl string-append "" (map any->string pth))
                   "r"))))

  (ctimex
   (function mkc**r-b (pth)
    (let loop ((p pth))
      (if (null? p) 'x
       `(
         ,(case (car p)
           ((a) 'car)
           ((d) 'cdr))
         ,(loop (cdr p)))))))

  (ctimex
   (macro mkc**r-s (s n)
    (cons 'top-begin
    (let loop ((p (list s)))
       (if (> (length p) n) nil
        (let ((p1 (cons 'a p))
              (p2 (cons 'd p)))
         `(
           (define ,(mkc**r p1) (fun (x) ,(mkc**r-b p1)))
           (define ,(mkc**r p2) (fun (x) ,(mkc**r-b p2)))
           ,@(loop p1)
           ,@(loop p2))))))))

(topblock
 (mkc**r-s a 3)
 (mkc**r-s d 3)
)



(cli-only
 (ctimex
  (begin
   (define __modn (! _global_mname))
   (define __mod (! _global_module)))))

(define *globals* (mkhash)) ;; variables pool

;; sets a "global variable" value
(macro set! (nm vl)
   `(hashput *globals* (quote ,nm) ,vl))

;; refers to the "global variable" value
(macro ! (nm)
   `(hashget *globals* (quote ,nm)))

(cli-only
  (ctimex
    (begin
     (set! _global_mname __modn)
     (set! _global_module __mod))))

;; Macro version of functional multiplication - accepts an arbitrary number of functions
(macro M@ funs
  ("This is a macro version of the functional composition [@] with an arbitrary number of arguments."
   "E.g., [(M@ f g h)] is equal to [(fun (x) (f (g (h x))))]."
   )
   (let ((nm (gensym)))
     `(lambda (,nm) ,(let loop ((x funs))
                        (if (null? x) nm
                            (list (car x) (loop (cdr x))))))))


(macro return (x)
  "Convinience macro, just expands into [x]."
  x)

