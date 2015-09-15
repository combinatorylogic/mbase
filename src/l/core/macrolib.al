;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(Section "\\Lone{} basic macros")

;; format macro - the most primitive form of it.

(macro ___xfmt0 (aarg formt . body)
  (let ((arg (gensym)))
   `(let ((,arg ,aarg))
      ,(let loop ((f formt) (p arg))
                 (cond
                   ((null? f) `(begin ,@body))
                   ((list? f)
                     (let ((na (gensym)))
                        `(let ((,na (cdr ,p))
                             ,@(if (and (symbol? (car f)) (not (eqv? (car f) '_)))
                                `((,(car f) (car ,p))) nil))
                            ,(loop (cdr f) na))))
                   ((symbol? f)
                         `(let ((,f ,p)) ,@body))
                   (else `(begin ,@body)))))))

;; extended format macro - allow nested lists
(macro format (aarg formt . body)
   ("Binds a pattern to an argument value. No checks are done, the value is expected to conform the format."
    "A pattern language is following:"
    "[["
    " <pattern>:"
    "    <ident>                  - bind this place to given variable"
    "  | ()                       - ignore the contents"
    "  | (<pattern> . <pattern>)  - patterns for head and tail of the list"
    "]]"
    )
   (if (and (list? formt) (> (length (filter list? formt)) 0))
       (let* ((fmt1 (let loop ((fm formt) (substs '()) (res '()))
                       (if (null? fm) (cons res substs)
                          (if (list? fm)
                           (if (list? (car fm))
                               (let ((sm (gensym)))
                                    (loop (cdr fm) (cons (list sm (car fm)) substs)
                                                   (append res (list sm))))
                               (loop (cdr fm) substs (append res (list (car fm)))))
                           (cons (append res fm) substs)))))
              (fmt2 (car fmt1))
              (substs (cdr fmt1)))
         `(___xfmt0 ,aarg ,fmt2 ,(let loop ((s substs)) (if (null? s) `(begin ,@body)
                              `(format ,(caar s) ,(cadar s) ,(loop (cdr s)))))))
       ;; otherwise it is just flat
       `(___xfmt0 ,aarg ,formt ,@body)))


(macro fmt (formt . body)
  "Creates a function accepting an argument of a given format, see [format] macro for details."
   (let ((arg (gensym))) `(fun (,arg) (format ,arg ,formt ,@body))))
(macro fmt0 (formt . body) (let ((arg (gensym))) `(fun (,arg) (___xfmt0 ,arg ,formt ,@body))))

(macro funct (nm args . body)
  "Creates a global function accepting an argument of a given format, see [fmt] and [format] for details."
  `(define ,nm (fmt ,args ,@body)))

;; case-car + format macro
; elt: (carvlu[s] format body)

(macro fccase.inner (arg . elts)
  (let ((s (gensym)))
  `(let* ((,s ,arg))
     (gencase (car ,s)
       ,@(map (lambda (elt)
                (if (eqv? (car elt) 'else) elt
                  (format elt (cv frmt . bdy)
                     `(,cv ((fmt ,frmt ,@bdy) (cdr ,s))))))
            elts)))))

(macro fccase (arg . elts)
 ("Select a format and action using [arg] head value."
   "Format is applied to the [arg] tail."
   "[["
   "<elt>:"
   "  ((<symbol>*) <format> <expr>*)"
   "]]"
   "See [format] macro for details."
   )
  `(fccase.inner ,arg ,@elts))

(macro letf (fs . body)
  (
   "Binds values to formats."
   "Usage:"
   "[["
   " (letf ((<format> <value>)*) <expr>*)"
   "]]"
   )
   (if (null? fs)
      `(begin ,@body)
      (format fs ((f1 v1) . rest)
         (if (list? f1)
            `(format ,v1 ,f1 (letf ,rest ,@body))
            `(let ((,f1 ,v1)) (letf ,rest ,@body))))))

;; helper function
(recfunction -do-counter-for- (f from to step)
  (if (< from to)
      (begin (f from) (-do-counter-for- f (+ from step) to step))
      nil))


(int-only

(function -do-counter-formap- (f from to step)
    (let loop ((i from))
       (if (< i to) (cons (f i) (loop (+ i step))) nil)))

)

(cli-only

(function -do-counter-formap- (f from to step) ;; constant stack space formap function
  (if (>= from to) nil
    (let ((p0 (cons nil nil)))
      (let loop ((p p0)
                 (i from))
         (if (>= i to) nil
            (begin
              (set-car! p (f i))
              (let ((nxt (+ i step)))
                (if (>= nxt to) nil
                  (let ((np (cons nil nil)))
                    (set-cdr! p np)
                    (loop np nxt)))))))
       p0)))
)


(macro with-syms (sl . body)
  "Binds [(gensym)]--generated values to the variables listed in sl."
  `(let ,(map (fun (x) `(,x (gensym))) sl) ,@body))


;; imperative counter for loop
(macro for (aft . body)
  ("Iterates the [body] expressions with a counter."
   ""
   "Usage:"
   "[["
   "(for (<var> <number-from> <number-to>) <expr>*)"
   "]]"
   )
   (format aft (arg from . to)
      `(-do-counter-for- (lambda (,arg) (begin ,@body)) ,from ,(car to)
          ,(if (null? (cdr to)) 1 (cadr to))
       )))

(macro formap (aft . body)
  (
   "Iterates the [body] expressions with a counter, makes a list of their values."
   ""
   "Usage:"
   "\begin{lstlisting}"
   "(formap (<var> <number-from> <number-to>) <expr>*)"
   "\end{lstlisting}")
  (format aft (arg from . to)
      `(-do-counter-formap- (lambda (,arg) (begin ,@body)) ,from ,(car to)
          ,(if (null? (cdr to)) 1 (cadr to))
  )))

(function reverse (lst)
   "Returns the reversed list."
  (let inner ( (l lst)
               (acc nil) )
    (cond
     ((null? l) acc)
     (else (inner (cdr l) (cons (car l) acc))))))

(function fromto (a b)
   "Creates a list of numbers from a to b (exclusive)"
   (formap (i a b) i))

(macro foreach rest
  ("Iterates over a given list."
   ""
   "Usage:"
   "[["
   " (foreach (<var> <expr1>) <expr>*)"
   "]]"
   "[<expr1>] value must be a list, the body is evaluated for each list element."
   )
  (format rest ((id lst) . body)
     `(iter (fun (,id) ,@body) ,lst)))

(macro foreach-count rest
  (format rest ((id lst cnt) . body)
     (with-syms (ret dcnt)
        `(let ((,dcnt (mkref 0)))
           (foreach (,id ,lst)
             (let* ((,cnt (deref ,dcnt))
                    (,ret (begin ,@body)))
               (r! ,dcnt (+ ,cnt 1))
               ,ret))))))

(macro foreach-map rest
  ("Iterates over a given list, making a list of body evaluation values."
   ""
   "Usage:"
   "[["
   " (foreach-map (<var> <expr1>) <expr>*)"
   "]]"
   "[<expr1>] value must be a list, the body is evaluated for each list element."
   )
  (format rest ((id lst) . body)
     `(map (fun (,id) ,@body) ,lst)))

(macro foreach-map-count rest
  (format rest ((id lst cnt) . body)
     (with-syms (ret dcnt)
        `(let ((,dcnt (mkref 0)))
           (foreach-map (,id ,lst)
             (let* ((,cnt (deref ,dcnt))
                    (,ret (begin ,@body)))
               (r! ,dcnt (+ ,cnt 1))
               ,ret))))))

(macro foreach-mappend-count rest
  (format rest ((id lst cnt) . body)
     (with-syms (ret dcnt)
        `(let ((,dcnt (mkref 0)))
           (foreach-mappend (,id ,lst)
             (let* ((,cnt (deref ,dcnt))
                    (,ret (begin ,@body)))
               (r! ,dcnt (+ ,cnt 1))
               ,ret))))))

(macro foreach-map-filter rest
  (format rest ((id lst) query . body)
    (with-syms (loop iarg)
      `(let ,loop ((,iarg ,lst))
         (if (null? ,iarg) nil
            (letf ((,id (car ,iarg)))
             (if ,query (cons (begin ,@body) (,loop (cdr ,iarg)))
                        (,loop (cdr ,iarg)))))))))

(macro foreach-mappend-filter rest
  (format rest ((id lst) query . body)
    (with-syms (loop iarg)
      `(let ,loop ((,iarg ,lst))
         (if (null? ,iarg) nil
            (letf ((,id (car ,iarg)))
             (if ,query (append (begin ,@body) (,loop (cdr ,iarg)))
                        (,loop (cdr ,iarg)))))))))

(macro foreach-mappend rest
  (format rest ((id lst) . body)
    `(foldl append '() (foreach-map (,id ,lst) ,@body))))


;; (do ... (where ...))

(macro do rest
  (
   "Expands into [let] expression. Usage:"
   "[["
   "(do <expr> (where (<name> <expr>)*))"
   "]]"
   )
  (letf(((b1 br)
         (let loop ((x rest) (c nil))
           (cond
             ((null? x) (list (reverse c) nil))
             ((and (list? (car x))
                   (eqv? 'where (caar x)))
              (list (reverse c) (cdr (car x))))
             (else (loop (cdr x) (cons (car x) c)))))))
     (if (null? br) `(begin ,@b1)
                    `(let* ,br ,@b1))))

(function hashput-seq (me k v)
   (if (list? me) (ohashput (car me) k v)
                  (ohashput me k v)))

(function debugmacro (name)
  ("Turns on a debugging output for a given macro.")
  (let ((old (hashget-seq (getcurmacroenv) name)))
     (let ((fn (fun (ek)
                  (println (buildstring "Expanding: " (to-string ek)))
                  (let ((res (old ek)))
                     (println (buildstring "Got: " (to-string res)))
                     res))))
        (hashput-seq (getcurmacroenv) name fn))))


(recfunction split (fn lst)
  ("Splits [lst] list with a given predicate function [fn]."
   "Returns a pair of lists, where the first element is a list of elements for which [fn] gives [#t],"
   "and the second one "
   "contains all other elements of [lst]."
   )
  (let loop ((l lst) (a nil) (b nil))
     (if (null? l) (cons (reverse a) (reverse b))
         (let ((x (car l)))
             (if (fn x) (loop (cdr l) (cons x a) b)
                        (loop (cdr l) a (cons x b)))))))


(function tailsplit (fn lst)
  "Same as [split], tail recursive version."
   (let* ((a (cons 1 nil))
          (b (cons 1 nil))
          (ar (cons 1 a))
          (br (cons 1 b)))
     (foreach (e lst)
         (if (fn e)
           (begin
             (let* ((xa (cdr ar))
                    (na (cons e nil)))
               (set-cdr! xa na)
               (set-cdr! ar na)))
           (begin
             (let* ((xb (cdr br))
                    (nb (cons e nil)))
               (set-cdr! xb nb)
               (set-cdr! br nb)))))
     (cons (cdr a) (cdr b))))

(macro mtailsplit2 (fn a1 lst)
  (with-syms (lst1)
  `(let* ((,lst1 ,lst)
          (a (cons 1 nil))
          (b (cons 1 nil))
          (ar (cons 1 a))
          (br (cons 1 b)))
     (foreach (e ,lst1)
         (if (,fn ,a1 e)
           (begin
             (let* ((xa (cdr ar))
                    (na (cons e nil)))
               (set-cdr! xa na)
               (set-cdr! ar na)))
           (begin
             (let* ((xb (cdr br))
                    (nb (cons e nil)))
               (set-cdr! xb nb)
               (set-cdr! br nb)))))
     (cons (cdr a) (cdr b)))))

(macro cut expression
  ("A simple alternative for currying. Creates a lambda function for a given expression with"
   "[<>]'s substituted as arguments."
   ""
   "E.g., [(cut + 2 <>)] expands into [(fun (x) (+ 2 x))]."
   )
  (let* ((slots (filter (fun (x) (eq? x '<>)) expression))
         (nslots (length slots))
         (args (formap (i 0 nslots) (gensym))))
    `(lambda ,args
        ,(let loop ((e expression) (a args))
            (if (null? e) nil
                (if (eq? (car e) '<>)
                    (cons (car a) (loop (cdr e) (cdr a)))
                    (cons (car e) (loop (cdr e) a))))))))

(recfunction qsort (cf lst)
  "Sorts a list using a given comparison function."
  (if (null? lst) nil
      (let ((a (car lst)) (b (cdr lst)))
         (do (append (qsort cf (car lr)) (cons a (qsort cf (cdr lr))))
             (where (lr
                      (tailsplit (cut cf <> a) b)))))))

(macro mqsort (cf lst)
  "Sorts a list using a given comparison function."
  (with-syms (loop a b lst1 lr )
  `(let ,loop ((,lst1 ,lst))
     (if (null? ,lst1) nil
       (let ((,a (car ,lst1)) (,b (cdr ,lst1)))
         (do (append (,loop (car ,lr)) (cons ,a (,loop (cdr ,lr))))
             (where (,lr
                     (mtailsplit2 ,cf ,a ,b)))))))))

(function interleave (lst del)
  "Makes a list of [lst] elements interleaved with [del]'s."
  (if (null? lst) nil
      (cdr (foldl (fun (acc x) (cons del (cons x acc))) nil (reverse lst)))))

(macro attempt (frst scnd)
  (let ((s (gensym)))
    `(let ((,s ,frst))
        (if (car ,s) (cdr ,s)
            ,scnd))))

(function map-stop (fn lst)
   (let loop ((l lst))
       (if (null? l) nil
          (if (fn (car l)) nil
             (cons (car l) (loop (cdr l)))))))

;;;

(macro <> args
  (
   "[(<> arg ... fun)] unrolls into [(fun arg ...)]."
   "It is useful with large function definitions (like AST visitors)."
   )
   (let* ((fn (car (lasttail args)))
          (xl (cuttail args)))
     (cons fn xl)))

(macro S<< args
  "A short form for [(buildstring ...)]"
   `(buildstring ,@args))

(macro Sm<< args
  "Same as [(string->symbol (S<< ...))]"
  `(string->symbol (S<< ,@args)))

;;;

(macro ifnull (c v)
  "If [c] is null, do [v], otherwise return [c] value."
   (with-syms (x)
      `(let ((,x ,c))
          (if (null? ,x) ,v ,x))))

;;; while

(macro whiledo (exp . body)
;  ""
   (with-syms (loop arg exit)
      `(let* ((,exit (cons #t nil))
              (break (fun () (set-car! ,exit nil) nil)))
          (let ,loop ((,arg ,exp)) (if (and (car ,exit) ,arg) (begin ,@body (,loop ,exp)) nil)))))

(macro dowhile (body exp)
;  ""
   (with-syms (loop arg exit)
      `(let* ((,exit (cons #t nil))
              (break (fun () (set-car! ,exit nil) nil)))
          (let ,loop ((,arg nil)) (if (and (car ,exit) ,arg) (begin ,@body (,loop ,exp)) nil)))))

(macro whilemap (exp . body)
;  ""
   (with-syms (loop arg exit)
      `(let* ((,exit (cons #t nil))
              (break (fun () (set-car! ,exit nil) nil)))
          (let ,loop ((,arg ,exp)) (if (and (car ,exit) ,arg) (cons (begin ,@body) (,loop ,exp)) nil)))))

(macro mapwhile (body exp)
;  ""
   (with-syms (loop arg exit)
      `(let* ((,exit (cons #t nil))
              (break (fun () (set-car! ,exit nil) nil)))
          (let ,loop ((,arg nil)) (if (and (car ,exit) ,arg) (cons (begin ,@body) (,loop ,exp)) nil)))))

(macro try-some body
  "Executes [body] expressions one by one until non--nil value is returned."
  (if (null? body) '(quote nil)
      (if (null? (cdr body)) (car body)
          (with-syms (a)
             `(let ((,a ,(car body)))
                (if ,a ,a (try-some ,@(cdr body))))))))

(macro when (cnd . body)
  "Equivalent to ([if] [cnd] (begin [body]))"
  `(if ,cnd (begin ,@body)))

(macro unless (cnd . body)
  "Equivalent to ([if] [cnd] nil (begin [body]))"
  `(if ,cnd nil (begin ,@body)))

(macro #+1 (expr)
  "[expr] + 1"
  `(+ ,expr 1))

(macro #-1 (expr)
  "[expr] - 1"
  `(- ,expr 1))

(macro list-cons lst
  (cond
   ((null? lst) 'nil)
   ((null? (cdr lst)) (car lst))
   ((null? (cddr lst)) `(cons ,(car lst) ,(cadr lst)))
   (else  `(cons ,(car lst) (list-cons ,@(cdr lst))))))

(macro do-after (last first)
  `(begin
     ,first
     ,last))


(macro type-case (t . args)
  (with-syms (s)
  `(alet ,s (r_GetType ,t)
     (cond ,@(foreach-map (a args)
               (if (eqv? (car a) 'else) a
                   `((t_eq ,s ,(car a))
                     (begin ,@(cdr a)))))))))

(macro type-case-hier (t . args)
  (with-syms (s)
  `(alet ,s (r_GetType ,t)
     (cond ,@(foreach-map (a args)
               (if (eqv? (car a) 'else) a
                   `((t_assignable? ,(car a) ,s)
                     (begin ,@(cdr a)))))))))


(macro module (name . body)
  (let loop ((b body)
             (e nil)
             (u nil))
    (if b
        (let* ((ab (car b)))
          (if (list? ab)
              (let* ((aab (car ab)))
                (if (eqv? aab 'using)
                    (loop (cdr b) e (append (cdr ab) u))
                    (if (eqv? aab 'export)
                        (loop (cdr b) (append (cdr ab) e) u)
                        `(inner.module-using-export ,name ,e ,u ,@b))))))
        '(begin ))))


(macro inner.module-using-export (name exs uss . body)
  `(top-begin
     (inner.module-using-export-push ,name ,exs ,uss)
     ,@body
     (inner.module-using-export-pop)))

(macro inner.module-using-export-push (name exs uss)
  (let* ((makepath (lambda (path)
                     (string->symbol
                      (let loop ((p path))
                        (if p
                            (string-append (string->symbol (car p))
                                           (if (cdr p)
                                               (string-append ":"
                                                              (loop (cdr p)))
                                               ""))
                            "")))))
         (sympath (append (bootlib:get-module-namespace) (list name)))
         (path (makepath sympath))
         (privatepath (makepath (append sympath '(private)))))
    `(ctimex (begin
               (bootlib:push-module-env)
               (bootlib:push-module-namespace (quote ,name))
               (bootlib:add-exports (quote ,exs))
               (bootlib:add-module-env (quote (,path ,@uss)))
               (bootlib:add-module-env (quote (,privatepath)))))))

(macro inner.module-using-export-pop ()
  `(begin
     (force-class-flush)
     (ctimex (begin
               (bootlib:pop-module-namespace)
               (bootlib:pop-module-env)))
     ))
