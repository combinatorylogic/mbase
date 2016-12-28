;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file provides the reasonable minimum of the MBase language
;; functionality. Its only purpose is to bootstrap itself in an interpretation
;; mode.
;;
;; At the beginning of the interpretation of this file we have a simple L1->L0
;; translator (providing only variable substitution). Macros are explicitly
;; disabled before the beginning of the bootstrap sequence.
;;
;; After the newly compiled boot.alc is loaded, we have the language L1M which
;; is capable of processing the next stage bootstrap file.
;;
;; Definitions are not grouped: language is growing incrementally here.
;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap library provides only car and cdr.
;; Now it is a time to define some more forms which we will use later:
;;

(def 'bootlib:cddr (lambda (x) (cdr (cdr x))))
(def 'bootlib:caddr (lambda (x) (car (cddr x))))
(def 'bootlib:cadddr (lambda (x) (car (cdr (cddr x)))))
(def 'bootlib:cadr (lambda (x) (car (cdr x))))
(def 'bootlib:cdar (lambda (x) (cdr (car x))))
(def 'bootlib:cdddr (lambda (x) (cdr (cddr x))))
(def 'bootlib:caar (lambda (x) (car (car x))))
(def 'bootlib:cadar (lambda (x) (car (cdr (car x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap runtime knows nothing about the recursion (to keep the interpreter
;; as simple as possible). So here we're introducing the Y-combinator.
;; Since runtime only allows a fixed number of application arguments, we will
;; need different Y-combinators for functions with a different number of args.
(def 'bootlib::Y (lambda (x)
            ((lambda (procedure)
               (x (lambda (arg) ((procedure procedure) arg))))
             (lambda (procedure)
               (x (lambda (arg) ((procedure procedure) arg)))))))
;; But for time being we only need Y0, Y1 and Y2.
(def 'bootlib::Y2 (lambda (x)
             ((lambda (procedure)
                (x (lambda (arg1 arg2) ((procedure procedure) arg1 arg2))))
              (lambda (procedure)
                (x (lambda (arg1 arg2) ((procedure procedure) arg1 arg2)))))))
(def 'bootlib::Y0 (lambda (x)
            ((lambda (procedure)
               (x (lambda () ((procedure procedure)))))
             (lambda (procedure)
               (x (lambda () ((procedure procedure))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list macro is useful for writing the bootstrap macros: we don't have a
;; quasiquotation yet.
(defmacro 'list nil)
(defmacro 'bootlib:list
  (lambda (args)
    (if (null? (cdr args)) (quote nil)
        (cons (quote cons)
              (cons (car (cdr args))
                    (cons
                     (cons (quote list)
                           (cdr (cdr args)))
                     nil
                     ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; language evolving: trivial let* macro.

(defmacro 'let*
  (lambda (arg)
    (if (null? (car (cdr arg)))
        (cons 'begin (cdr (cdr arg)))
        ((lambda (first next)
           (list (list (quote lambda)
                       (list (car first))
                       (cons (quote let*)
                         (cons
                           next
                           (cdr (cdr arg)))))
                 (car (cdr first))))
         (car (car (cdr arg)))
         (cdr (car (cdr arg)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; now let's simplify the function definition

(defmacro 'core.function
  (lambda (arg)
    (let* ((fn (cadr arg))
           (argg (caddr arg))
           (body (cdddr arg)))
      (list 'def (list 'quote fn)
            (list 'lambda
                  argg
                  (cons 'begin body))))))

(defmacro 'function (lambda (arg) (cons 'core.function (cdr arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; and a basic 'define' macro:
(defmacro 'core.define
  (lambda (arg)
    (list 'def (list 'quote (cadr arg))
          (caddr arg))))

(defmacro 'define (lambda (arg) (cons 'core.define (cdr arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tail recursive list length function: we will need it soon.
;; N.B.: the bootstrap interpreter is not properly tail recursion friendly.
;;   CLI native compiler, on the other hand, provides the tail recursion optimisation,
;;   so we need it this way for the compilation stage bootstrap
(define length
  (let* ((l00
          (:Y2 (lambda (l0)
                 (lambda (n l)
                   (if (null? l) n
                       (l0 (+ n 1) (cdr l))))))))
    (lambda (l) (l00 0 l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A macro to generate n-ary Y-combinators.
(defmacro 'bootlib::Yn
  (lambda (args)
    (let* ((nu (cadr args))
           (alist
            ((:Y (lambda (mklist)
                   (lambda (n)
                     (if (> n 0)
                         (cons (string->symbol
                                (string-append
                                 "arg-"
                                 (number->string n)))
                               (mklist (- n 1)))
                         nil))))
             nu))
           (tmpl
            (list 'lambda
                  '(procedure)
                  (list
                   'x (list 'lambda alist
                            (cons '(procedure procedure) alist))))))
      (list 'lambda
            '(x)
            (list
             tmpl
             tmpl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prefill some more Y-combinators
(def 'bootlib::Y1 :Y)
(def 'bootlib::Y3 (:Yn 3))
(def 'bootlib::Y4 (:Yn 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generates a recursive definition using the Y-combinator.
(function  bootlib:rec-helper (fn argg body)
  (list 'core.0.reclambda
  fn
  argg
  (cons 'begin body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; now we can define n-ary recursive functions with a better syntax
(defmacro 'core.recfunction
  (lambda (arg)
    (let* ((fn (cadr arg))
           (argg (caddr arg))
           (body (cdddr arg)))

      (list 'def
            (list 'quote fn)
            (bootlib:rec-helper fn argg body)
      ))))


(defmacro 'recfunction (lambda (arg) (cons 'core.recfunction (cdr arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The most basic implementation of a map function:
(recfunction inner.map (f l)
  (if (null? l)
      nil
      (cons (f (car l)) (inner.map f (cdr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Of course, on later bootstrap stages we will need a better implementation,
;;  so here is a re-definition to be overriden.
(define map inner.map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Append function is initially needed for a quasiquotation
(recfunction inner.append (a b)
  (if (null? a) b
      (cons (car a)
            (inner.append (cdr a) b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-cdr! and set-car! are initially bound to the runtime functions,
;;   and at a later stage they're replaced with inline IL macros.
(define set-cdr! corelib:set-cdr!)
(define set-car! corelib:set-car!)
(define append inner.append)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unfortunately, we can't use quasiquotation to define the quasiquotation -
;; macros are wiped out from the initial bootstrap file.
(function quasiquote-quote (cl)
   (if (if (string? cl) #t (if (number? cl) #t))
       cl
       (cons 'quote (cons cl nil))
))

(function unqo-check (lst)
 (let* ((unq-check (:Y1 (lambda (unq-check)
   (lambda (l)
     (if (null? l) nil
      (if (symbol? l)
        (if (eqv? l 'unquote) #t
            (if (eqv? l 'unquote-splicing) #t
                nil))
        (if (list? l)
           (if (unq-check (car l)) #t
               (unq-check (cdr l)))
           nil
       ))))))))
    (unq-check lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following code looks awful. At this stage our
;; language is still extremely limited, and so we can't do any
;; nice code formatting tricks. This quasiquotation itself gives
;; an extremely powerful tool for simplifying the code, so the rest
;; of the bootstrap sequence is much more readable.
(defmacro 'quasiquote
  (lambda (arg)
    (let* ((loop
            (:Y1 (lambda (loop)
                   (lambda (l)
                     (if (null? l) 'nil
                         (if (pair? l)
                           (if (unqo-check l)
                             (let* ((cl (car l)))
                               (if (pair? cl)
                                   (if (eqv? (car cl) 'unquote)
                                       (list 'cons
                                             (cadr cl)
                                             (loop (cdr l)))
                                       (if (eqv? (car cl) 'unquote-splicing)
                                           (if (null? (cdr l))
                                             (cadr cl)
                                             (list 'append
                                                   (cadr cl)
                                                   (loop (cdr l))))
                                           (list 'cons
                                                 (loop cl)
                                                 (loop (cdr l)))))
                                   (list 'cons (quasiquote-quote cl)
                                         (loop (cdr l)))
                                   ))
                                 (list 'quote l)
                             )
                             (list 'quote l))))))))
      (loop (cadr arg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now we can have a better macro definition facility:
(defmacro 'core.macro
  (lambda (args)
    (let* ((name (cadr args))
           (argl (caddr args))
           (body (cdddr args)))
      `(defmacro (quote ,name)
         (lambda
             (macroarg) ;; give it a fixed name...
           ;; generate an arglist bindings:
           (let*
               ,((:Y3 (lambda (gen)
                        (lambda (prv n al)
                          (if (null? al)
                              nil
                              (if (pair? al)
                                  (let* ((nm (string->symbol
                                              (string-append
                                               "*t--t-*-"
                                               (number->string n)))))
                                    (cons
                                     `(,nm (cdr ,prv))
                                     (cons
                                      `(,(car al) (car ,nm))
                                      (gen nm (+ n 1) (cdr al))
                                      )))
                                  ;; not a pair: 'rest' args list
                                  (list `(,al (cdr ,prv)))
                                  )))))
                 'macroarg
                 0
                 argl)
             (begin ,@body)))))))

(defmacro 'macro (lambda (arg) (cons 'core.macro (cdr arg))))

(macro noconst (x) x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A complimentary 'let0' macro on top of lambda:
(macro let0 (bindings . body)
  `((lambda ,(inner.map car bindings) (begin ,@body))
    ,@(inner.map cadr bindings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Scheme-style 'let' macro
(macro let rest ; yep, list of args
       (if (symbol? (car rest)) ;; 'let loop' form
           (let* ((nm (car rest))
                  (arb (cadr rest))
                  (body (cddr rest))
                  (ars (inner.map car arb))
                  (ini (inner.map cadr arb)))
             `(,(bootlib:rec-helper nm ars body) ,@ini))
           ;; a normal 'let' - pass to 'let0' macro
           `(let0 ,@rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cond macro - we've got only 'if' in a bootstrap interpreter
(macro cond args ; list of args
       (let loop ((a args))
         (if (null? a) nil
             (let ((ca (car a)))
               (if (eqv? 'else (car ca)) `(begin ,@(cdr ca))
                   `(if ,(car ca) (begin ,@(cdr ca))
                        ,(loop (cdr a))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Proper logic: and, or and not macros
(macro and (l . r)
     (if (null? r) l
       (let ((rr (if (null? (cdr r))
                     (car r)
                     `(and ,(car r) ,@(cdr r)))))
         `(if ,l ,rr nil))))

(macro or (l . r)
     (if (null? r) l
       (let ((rr (if (null? (cdr r))
                     (car r)
                     `(or ,(car r) ,@(cdr r)))))
         `(if ,l #t ,rr))))


(function bootlib:fand (a b) (and a b))
(function bootlib:for (a b) (or a b))

(function bootlib:not (b) (if b nil #t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; memq function, needed by a bootstrap compiler
(recfunction bootlib:memq (o l)
  (if (null? l) nil
      (if (eqv? o (car l)) l
    (bootlib:memq o (cdr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here follows the bootstrap compiler itself,
;;;  a port from the original Scheme implementation

(recfunction bootlib:lookup-env (env v)
  (if (null? env) nil
      (if (eqv? (car env) v)
    v
    (bootlib:lookup-env (cdr env) v))))

(recfunction bootlib:get-free (env l)
  (cond
   ((null? l) l)
   ((symbol? l)
    (let ((v (bootlib:lookup-env env l)))
      (if (null? v) (list l) nil)))
   ((list? l)
    (cond
     ((eqv? 'lambda (car l))
      (bootlib:get-free (inner.append (cadr l) env) (cddr l)))
     ((eqv? 'core.0.reclambda (car l))
      (bootlib:get-free (inner.append (cons (cadr l) (caddr l)) env) (cdddr l)))
     ((eqv? 'quote (car l)) nil)
     (else (inner.append (bootlib:get-free env (car l))
       (bootlib:get-free env (cdr l))))))
   (else nil)))

(function bootlib:unifiq (l)
  (let ((ht (mkshash)))
    (let loop ((v nil)
               (x l))
      (if (null? x) v
          (let ((cx (car x)))
            (if (shashget ht cx)
                (loop v (cdr x))
                (begin
                  (shashput ht cx cx)
                  (loop (cons cx v)
                        (cdr x)))))))))


;; initialize the macro storage
(define *menv* (getmacroenv))

(recfunction bootlib:lookup-env-car (env v)
  (if (null? env) nil
      (if (eqv? (caar env) v) (cadar env)
    (bootlib:lookup-env-car (cdr env) v))))

(recfunction bootlib:lookup-env-car-d (env v)
  (if (null? env) nil
      (if (eqv? (caar env) v) (cdar env)
    (bootlib:lookup-env-car-d (cdr env) v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; L1->L0 Compiler.
;;
;; We must keep it as simple as possible.
;; It only converts the local lambda parameters to the frame refs and
;; unrolls simple macros.

(function bootlib:hashget-mod (ht nm)
  (let* ((chk0 (shashget ht nm)))
    (if chk0 chk0
        (let ((prefixes (shashget (getfuncenv) '*MODULE-SEARCH-PATH*)))
          (if prefixes
              (let loop ((l prefixes))
                (if l
                    (let* ((nnm (string->symbol (string-append (symbol->string (car l))
                                                               (string-append ":" (symbol->string nm)))))
                           (chk (shashget ht nnm)))
                      (if chk chk
                          (loop (cdr l))))
                    ))))
        )))

(recfunction bootlib:hashget-seq (hl v)
  (cond
   ((null? hl) nil)
   ((list? hl)
    (if (null? (cdr hl))
        (hashget-mod (car hl) v)
        (let ((tst (shashget (car hl) v)))
          (if (null? tst) (bootlib:hashget-seq (cdr hl) v)
              tst))))
   (else (hashget-mod hl v))))


(recfunction bootlib:expand0 (compile mcenv l)
  (cond
   ((null? l) l)
   ((symbol? l)
    (if (corelib:symbol-starts-with '## l)
        (let ((v (hashget-seq mcenv l))) (if v ((hashget-seq mcenv l)) 'nil))
        l
        ))
   ((list? l)
    (let ((cl (car l)))
      (cond
       ((eqv? cl 'quote) l)
       ((eqv? cl 'inner-expand-with)
        (let ((nenv (cons (cadr l) mcenv))
              (code (caddr l)))
          (bootlib:expand0 compile nenv code)))
       ((eqv? cl 'inner-expand-first)
        (bootlib:expand0 compile mcenv
                         (inner.map (lambda (v) (bootlib:expand0 compile mcenv v)) (cdr l))))
       (else
        (let ((sh (if (symbol? cl)
                      (hashget-seq mcenv cl)
                      nil)))
          (if sh
              (try
               (try
                (let ((res (sh l)))
                  (cond
                   ((and
                     (list? res)
                     (eqv? 'top-begin (car res)))
                    (compile nil res)
                    )
                   (else (bootlib:expand0 compile mcenv res))))
                t_MBaseException
                (lambda (x)
                  (begin
                    (println "Exception")
                    (println
                     ((shashget (getfuncenv) 'to-string)
                      ((shashget (getfuncenv) 'mbaseerror) x)))
                    (println " while expanding:")
                    (println
                     ((shashget (getfuncenv) 'to-string) l))
                    `(Error)

                    ))
                )
               t_Exception
               (lambda (x)
                 (begin
                   (println "Unexpected exception while expanding: ")
                   (println
                    ((shashget (getfuncenv) 'to-string) l))
                   (println
                    ((shashget (getfuncenv) 'to-string) x))
                   `(Error)

                   )
                 )
               )
              (inner.map (lambda (ll) (bootlib:expand0 compile mcenv ll)) l)
              ))))))
   (else l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some basic in-place macros support.
(macro #define (nm vl)
 `(defmacro
    (quote ,(string->symbol (string-append "##" (symbol->string nm))))
    (let ((vl ,vl)) (lambda () vl))))

(macro #fdefine (nm vl)
  `(defmacro
     (quote ,(string->symbol (string-append "##" (symbol->string nm))))
     (lambda () ,vl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module lookup support
(function bootlib:core:lookup-global (nm)
  (let ((chk0 (shashget (getfuncenv) nm)))
    (if chk0 nm
        (let ((prefixes (shashget (getfuncenv) '*MODULE-SEARCH-PATH*)))
          (if prefixes
              (let loop ((l prefixes))
                (if l
                    (let* ((nnm (string->symbol (string-append (symbol->string (car l))
                                                               (string-append ":" (symbol->string nm)))))
                           (chk (shashget (getfuncenv) nnm)))
                      (if chk
                          nnm
                          (loop (cdr l))))
                    nm))
              nm)))))

;; Free<->bound variables substitution - the heart of the L1->L0 compiler.
(recfunction bootlib:postcompile (env l)
  (cond
   ((null? l) l)
   ((symbol? l)
    (let ((v (lookup-env-car-d env l)))
      (if (null? v)
          (core:lookup-global l) ;; A global
          (let ((tg (cadr v)))
            (if (eqv? tg 'env) (list 'env-ref (car v))
                (list 'arg-ref (car v))))
          )))
   ((list? l)
    (let ((cl (car l)))
      (cond
       ((eqv? 'core.0.reclambda cl)
        (let ((fv (unifiq (get-free nil l)))
              (narg (length (caddr l))))
          ;; build translation from env to fv (new env!):
          (let ((ttt
                 (let loop ((f fv)
                            (i 1)
                            (ne nil)
                            (sl nil)
                            )
                   (if (null? f) (list i ne sl)
                       (let ((v (lookup-env-car-d env (car f))))
                         (if (null? v) ;; global, skip it
                             (loop (cdr f) i ne sl)
                             ;; otherwise - substitute
                             (loop (cdr f) (+ i 1)
                                   (cons (list (car f) i 'env) ne)
                                   (inner.append sl (list v)))))))))
            (list 'lambda (cons narg (cons '(0 self) (caddr ttt)))
                  (bootlib:postcompile
                   (cons (list (cadr l) 0 'env)
                         (inner.append
                          (let loop ((i 0)
                                     (v (caddr l)))
                            (if (null? v) nil
                                (cons (list (car v) i 'arg) (loop (+ i 1) (cdr v)))))
                          (cadr ttt)))
                   (cadddr l)))
            )))
       ((eqv? 'lambda cl)
        (let ((fv (unifiq (get-free nil l)))
              (narg (length (cadr l))))
          ;; build translation from env to fv (new env!):
          (let ((ttt
                 (let loop ((f fv)
                            (i 0)
                            (ne nil)
                            (sl nil)
                            )
                   (if (null? f) (list i ne sl)
                       (let ((v (lookup-env-car-d env (car f))))
                         (if (null? v) ;; global, skip it
                             (loop (cdr f) i ne sl)
                             ;; otherwise - substitute
                             (loop (cdr f) (+ i 1)
                                   (cons (list (car f) i 'env) ne)
                                   (inner.append sl (list v)))))))))
            (list 'lambda (cons narg (caddr ttt))
                  (bootlib:postcompile
                   (inner.append
                    (let loop ((i 0)
                               (v (cadr l)))
                      (if (null? v) nil
                          (cons (list (car v) i 'arg) (loop (+ i 1) (cdr v)))))
                    (cadr ttt))
                   (caddr l)))
            )))
       ((eqv? 'quote cl) l)
       ((eqv? 'begin cl)
        (if (null? (cdr l)) 'nil
            (if (null? (cddr l)) (bootlib:postcompile env (cadr l))
                (cons 'begin (inner.map (lambda (x) (bootlib:postcompile env x)) (cdr l))))))
       ((eqv? 'top-bdone cl) (cons 'top-begin (cdr l)))
       ((eqv? 'top-begin cl) l)
       ((eqv? 'notaruntime cl) (bootlib:postcompile env (cons 'begin (cdr l))))
       ((eqv? 'topblock cl) (bootlib:postcompile env  (cons 'begin (cdr l))))
       (else
        (inner.map (lambda (x) (bootlib:postcompile env x)) l)))))
   (else l)))

(define *current-macro-env-holder* (noconst (cons nil nil)))
(function set-macro-env (env)
  (corelib:set-car! *current-macro-env-holder* env))

(function getcurmacroenv ()
  (car *current-macro-env-holder*))

(set-macro-env (getmacroenv)) ;; default is the global one


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler interface functions to be used from the outside
(recfunction bootlib:compile (env l)
 (cond
  ((and (list? l) (eqv? 'top-begin (car l)))
   (cons 'top-bdone
   (inner.map (lambda (x) (let ((cc (bootlib:compile nil x)))
          (begin (eval cc) cc)))
        (cdr l))))
  (else
   (bootlib:postcompile env (expand0 bootlib:compile (getcurmacroenv) l)))))

(function bootlib:expand (l) (expand0 compile (getcurmacroenv) l))

(define screen-symbol-stub (noconst (cons nil nil)))

(function screen-symbol (s)
  (let ((st (car screen-symbol-stub)))
    (if st (st s)
  s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An s-expressions printer for dumping the compilation results -
;;   the L0 interpreter does not provide it
(function atom-to-string (a)
  (cond
   ((null? a) "()")
   ((string? a) (string-append
                 "\""
                 (string-append (corelib:string-escape a)
                                "\"")))
   ((symbol? a) (symbol->string (screen-symbol a)))
   ((number? a) (number->string a))
   ((char? a)
    (string-append
     "#\\"
     (cond
      ((eq? a #\Newline) "Newline")
      ((eq? a #\Tab) "Tab")
      ((eq? a #\Space) "Space")
      ((eq? a #\LBR) "LBR")
      ((eq? a #\RBR) "RBR")
      ((eq? a #\Semicolon) "Semicolon")
      ((< (ascii a) (ascii #\Space)) (string-append "ASCII" (any->string
                                                             (ascii a))))
      (else a))))
   ((boolean? a) (if (r_debool a) "#t" "#f"))
   (else (string-append "?:" (any->string a)))))

;;
(recfunction innerlist (outerlist q l)
  (string-append (outerlist q (car l))
     (if (null? (cdr l))
         ""
         (if (pair? (cdr l))
       (string-append " " (innerlist outerlist q (cdr l)))
       (string-append " . " (atom-to-string (cdr l)))))))

;; Separate defines to keep the stupid text editors happy with (-) balance.
(define lbr "(") (define rbr ")")

;;
(recfunction outerlist (q l)
  (cond
   ((null? l) "()")
   ((list? l)
    (if (and (cdr l) (list? (cdr l)))
        (cond
         ((and (= q 1) (eqv? 'quote (car l)))
          (string-append "'" (outerlist 0 (cadr l))))
         ((and (= q 1) (eqv? 'quasiquote (car l)))
          (string-append "`" (outerlist 2 (cadr l))))
         ((and (= q 2) (eqv? 'unquote (car l)))
          (string-append "," (outerlist 1 (cadr l))))
         ((and (= q 2) (eqv? 'unquote-splicing (car l)))
          (string-append ",@" (outerlist 1 (cadr l))))
         (else
          (string-append lbr (string-append (innerlist outerlist q l) rbr))
          ))
        (string-append lbr (string-append (innerlist outerlist q l) rbr))))
   (else (atom-to-string l))))

;; Generic S-Expression printer.
(function to-string (l) (outerlist 1 l))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now the L1->L0 compiler is implemented in L1, so we can add two interface
;; functions: read-compile-eval to be called from the runtime each time it reads a complete list
;; and read-compile-eval-dump to be called when bootstraping the compiler.

(function read-compile-eval0 (lst)
  (let ((expr (compile nil lst))) ;; compile it
    (let ((res (eval expr)))
      (cons expr res))))

(function bootlib:core:read-int-eval (lst)
  (cdr (read-compile-eval0 lst))) ;; compile, run, return the result

(define bootlib:core:read-int-eval-hook (cons core:read-int-eval nil))

(function read-int-eval (lst)
  ((car core:read-int-eval-hook) lst))

(define read-compile-eval read-int-eval)

(recfunction to-string-top (l)
  (cond
    ((and (list? l) (or (eqv? 'top-bdone (car l)) (eqv? 'top-begin (car l))))
     (let loop ((str "")
                (ll (cdr l)))
         (if (null? ll) str
            (loop (string-append str (string-append
                                      (let ((nxt (to-string-top (car ll))))
                                        (if nxt nxt "")
                                        )"\n")) (cdr ll)))))
    ((or (null? l) (not (list? l))) nil)
    (else (outerlist 1 l))))

(function read-compile-eval-dump (lst) ;; compile, run, return the compiled string
   (to-string-top (car (read-compile-eval0 lst))))

;; Dummies, to be replaced later
(macro unit-test body `(begin ))
(macro unit-test-defn body `(begin ))
(macro mkref rest (if rest `(cons ,(car rest) nil) `(cons nil nil)))
