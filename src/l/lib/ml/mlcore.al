;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \begin{document}
;-
;- \section{Embedded ML implementation}
;-
;- Here follows a simple implementation of the ML language, featuring a
;- Hindley-Milner type inference and a not--quite--optimising compiler via
;- MBase metaprogramming\footnote{Functions are implicitly curried, which leads
;- to a high number of closure captures and affects performance.}.
;-
;-
;- \subsection{AST and type inference}
;-
;- An abstract syntax tree for ML represents several different entities:
;- a top level expression (some of them are just instructions to the compiler
;- engine), a normal ML expression, a pattern and a type.
;-

(include "./mlcore-ast.al");-I

;-
;- A usual MBase approach to typing includes defining a transitional tagged AST,
;- where each expression (each entity which could be typed) receives a unique tag.
;- These tags will be used as free variables in a type equations system.
;-

(def:ast mltagged ( (mlsrc (expr oexpr)
                           (pattern opattern)) )
  (expr (<tag:t> . <oexpr:e>))
  (pattern (<tag:t> . <opattern:p>)))

;= Transformation visitor is simple, we will just use a general purpose unique name
;= generation function [[(gensym)]] here.

(function tagify ( src )
  (mlsrc:visit topexpr src
    (expr DEEP (forall (cons (gensym) node)))
    (pattern DEEP (forall (cons (gensym) node)))))

;-
;- It is obvious that we need an environment to be able to resolve types from
;- type constructors and original type definitions from aliases. Simplest
;- way of dealing with environments is to put everything in a single hash table.
;-
;- We will use the same environment to keep types of global definitions as well.
;-

(function mlgettype (env nm)
  (hashget env (S<< "type: " nm)))

(function mlgetnametype (env nm)
  (hashget env (S<< "typeof: " nm)))

;- Type variables should be unique for newly introduced equations.
(function mltypesrename (typs)
  (with-hash (ht)
  (foreach-map (typ typs)
  (mlsrc:visit type typ
     (type DEEP
           ((V (alet x (ht> nm)
                     (ast:mknode
                      (nm
                       (if x x (alet nn (gensym)
                                     (ht! nm nn)
                                     nn))))))
            (else node)))))))

(function mltyperename (typ)
  (car (mltypesrename (list typ))))

;- Converting a type into a Prolog term:
(function mlprologtype (typ)
  (mlsrc:visit type typ
     (type DEEP
       ((V nm)
        (F `(fun ,t ,r))
        (TPL `(tuple ,@args))
        (T `(,nm ,@args))))))

;- Extracting type arguments
(function mltypeargs (vtyp)
  (mlsrc:visit type vtyp
    (type _
      ((T args)
       (else (ccerror `(ML:TYPEARGS ,vtyp)))))))

;= [[mlgetconstr]] returns a pair: value type and constructor argument
;= types, with properly renamed type variables.
(function mlgetconstr (env nm)
  (alet r (hashget env (S<< "constr: " nm))
     (if (not r)
         (ccerror `(ML:CONSTR ,nm)))
     (mltypesrename r)))

(function mlnodetype (t) (car t))
(function mlargstypes (t) (cdr t))

;-
;- Now we have everything that is necessary to produce type equations out of
;- a given ML source code.
;-
;- The MBase embedded Prolog simplified syntax is used here for representing
;- equations. Only one predefined Prolog term [[equals]] is used,
;- all other terms are representing type names. All the equations forms
;- a list of goals for Prolog to solve, it is similar to what an original
;- Hindley-Milner algorithm is doing.
;-

;= Small macros to make the code shorter:
(macro eq-self (v)
  `(add (list 'equals (caar stack) ,v)))
(macro eq-selfq (v)
  `(eq-self (quote ,v)))

;= Processing pattern expressions:
(function add-pattern-equation (env add atag rtag p)
  (format p (ptn (xt . _))
;= [[rtag]] is a tag of the whole [[match]] expression,
;= [[atag]] is a tag of the corresponding action expression for a given
;= pattern.
    (add `(equals ,rtag ,xt))
    (add `(equals ,atag ,(car ptn)))
;= Iterates deep into a pattern, producing a list of pattern bound variables
;= equations.
    (mltagged:iter pattern ptn
      (opattern DEEP
        ((bind (begin
                 (add `(equals ,nm ,(car p)))
                 (eq-self nm)
                 ))
         (number (eq-selfq (number)))
         (char   (eq-selfq (char)))
         (string (eq-selfq (string)))
         (bool (eq-selfq (bool)))
         (unit (eq-selfq (unit)))
         (constr
          (let* ((ptype (mlgetconstr env nm))
                 (ntype (mlnodetype ptype))
                 (atypes (mlargstypes ptype)))
            (eq-self (mlprologtype ntype))
            (if (not (= (length atypes) (length args)))
                (ccerror `(ML:TYPING:CONSTR:NUMBER:OF:ARGS ,nm ,(length args))))
            (iter-over (zip (map mlprologtype atypes) (map car args))
                       (fmt (x y) (add `(equals ,x ,y))))))
         (tuple
          (eq-self `(tuple ,@(map car args))))
         (else nil)
         )))
    ))

;- For all the expression types other than pattern matching:
(function mlequations ( env tsrc ieqs )
  (collector (add get)
;= Some equations may be inherited from a top level
;= expression ([[let rec]] in particular)
    (iter add ieqs)
;= All other equations should be derived in an undefined order from the given
;= source tree.
    (mltagged:iter expr tsrc
      (oexpr DEEP
;= [[let]] defines a variable with the same type as its value (obviously)
;= and the whole expression type is the same as the type of the [[let]] body.
        ((let
             (add `(equals ,nm ,(car value)))
             (eq-self (car body))
             )
;= [[let rec]] is the same as [[let]], all the difference was resolved during
;= renaming stage.
         (letrec
             (add `(equals ,nm ,(car value)))
             (eq-self (car body))
             )
         (letrecr
             (iter-over dfs
               (fmt (nm value)
                  (add `(equals ,nm ,(car value)))))
             (eq-self (car body)))
;= Imperative sequence returns the last expression value, so the type of
;= [[begin]] expression is the same as the type of its last subexpression.
         (begin
           (eq-self (caar (lasttail es))))
;= [[fun]] expression has a functional type, argument type is bound to
;= argument name and value type is the same as a function body type.
;= Since functions are curried, each function have only one argument.
         (fun
             (eq-self `(fun ,arg ,(car body))))
;= [[apply]] first argument must be a function taking an argument of the same
;= type as the second argument of [[apply]] and returning the same type as
;= bound to the whole [[apply]] expression.
         (apply
             (add `(equals ,(car fn) (fun ,(car arg) ,(caar stack)))))
;= Constants obviously have constant types.
         (number (eq-selfq (number)))
         (char   (eq-selfq (char)))
         (string (eq-selfq (string)))
         (bool (eq-selfq (bool)))
         (unit (eq-selfq (unit)))
;= Variable type is bound to its name. Some variables are global, in
;= this case type is taken from the environment.
         (var
          (alet a (mlgetnametype env nm)
            (if a
                (eq-self (mlprologtype
                          (mltyperename a)))
                (eq-self nm))))
;= Constructor types are always known from environment definitions.
         (constr
          (let* ((ptype (mlgetconstr env nm))
                 (ntype (mlnodetype ptype))
                 (atypes (mlargstypes ptype)))
            (eq-self (mlprologtype ntype))
            (if (not (= (length atypes) (length args)))
                (ccerror `(ML:TYPING:CONSTR:NUMBER:OF:ARGS ,nm ,(length args))))
            (iter-over (zip (map mlprologtype atypes) (map car args))
                       (fmt (x y) (add `(equals ,x ,y))))))
;= Tuple constructor generates a new anonymous type
         (tuple
          (eq-self `(tuple ,@(map car args))))
;= And pattern matching is served in a separate function, see
;= the definition above.
         (match
          (let* ((atag (car arg))
                 (rtag (caar stack)))
            (foreach (p ps)
              (add-pattern-equation env add atag rtag p))))
         )))
;= Now the collector contains all the equations
    (get)))

;-
;- Prolog representation of a type is different from our inner format, so
;- here is a reverse transformation:
(recfunction prolog->type (tp)
  (p:match tp
    ((str tuple . $rest)
     `(TPL ,@(map prolog->type rest)))
    ((str fun . $rest)
     `(F ,@(map prolog->type rest)))
    ((str $$M:nm . $rest)
     `(T ,nm ,@(map prolog->type rest)))
    ((var ($n . $$M:nm)) `(V ,(Sm<< nm "_" n)))))

;-
;- Equations are already in the embedded Prolog format, so, they can be
;- immediately evaluated (as a single goal with a default facts database).
;-

(function solve-equations ( eqs )
  (alet res (simple-prolog-l nil eqs)
    ; we'll pick only the first solution
    (if (null? res) (ccerror `(ML:TYPING ,@eqs)))
    (with-hash (ht)
      (iter-over (car res)
          (fmt (nm tp)
               (ht! nm (prolog->type tp))))
      ht)))

;- A hash table obtained from the above function now could be used to
;- convert all tags to types in a tagged AST.

(function patch-types (tsrc ht)
 (use-hash (ht)
  (mltagged:visit expr tsrc
     (expr DEEP (ast:mknode (t (ht> t))))
     (pattern DEEP (ast:mknode (t (ht> t)))))))

;- Previous definitions worked with simple expressions. Now, to serve top level
;- expressions properly:

(function ml-typing (env texpr)
  (mltagged:visit topexpr texpr
    (topexpr _
      ((mlletrec
        (ast:mknode
         (value (patch-types
                 value
                 (solve-equations
                  (mlequations env value
                    `((equals ,nm ,(car value)))))))))
       (mlletrecr
        (let* ((eqns (map-over dfs
                      (fmt (nm value)
                        `(equals ,nm ,(car value)))))
               (alleqns
                (foreach-mappend (d dfs)
                  (format d (nm value)
                          (mlequations env value eqns))))
               (res (solve-equations alleqns)))
          (ast:mknode
           (dfs (map-over dfs
                   (fmt (nm value)
                        `(,nm
                          ,(patch-types
                            value res))))))))
       (else
        (mltagged:visit topexpr node
          (expr _
                (patch-types node (solve-equations
                    (mlequations env node nil))))))
       ))))

;-
;- Now, an AST will be slightly different, with types instead of tags:
(def:ast mltyped ( (mltagged) )
  (expr (<type:t> . <oexpr:e>))
  (pattern (<type:t> . <opattern:p>)))


(include "./mlcore-parser2.al");-I
;;; (include "./mlcore-parser.al");-I


;-
;- \subsection{Compiler}
;-
;- Now we have an AST definition, the typing engine and the parser. One last
;- thing to implement is a compiler --- code generation out of a typed AST,
;- and some preparatory steps after parsing: scope handling and environment
;- handling.
;-
;- \subsubsection{Scope management}
;-
;- We will start with scope handling here. In order to avoid name clashes,
;- all the local names will be replaced with unique symbols, with respect to
;- the local scoping rules. This step is to happen right after parsing and
;- before type inference. Transformation is relatively complicated, because
;- there is a diverse set of AST nodes which might introduce new variables into
;- a local scope.
;-

;= Each pattern may have bindings, and this bindings should be renamed
(function ml-rescope-pattern (add ptn)
  (with-hash (ht)
   (mlsrc:visit pattern ptn
      (pattern DEEP
         ((bind (let* ((tst (ht> nm))
                       (nnm (if tst tst (alet nnm (gensym)
                                              (ht! nm nnm)
                                              (add `(,nm ,nnm))
                                              nnm))))
                  (ast:mknode (nm nnm))))
          (else node))))))

;= The next function renames all the locally bound identifiers inside a simple
;= expression.
(function ml-rescope-expr ( expr )
  (let loop ((e expr) (subst nil))
    (mlsrc:visit expr e
      (expr _
;= [[fun]] node introduces new name --- [[arg]].
        ((fun
           (let* ((nname (gensym))
                  (nsubst `((,arg ,nname) ,@subst)))
             (ast:mknode (arg nname) (body (loop body nsubst)))))
;= [[let]] node introduces new name --- [[nm]].
         (let
           (let* ((nname (gensym))
                  (nsubst `((,nm ,nname) ,@subst)))
             (ast:mknode (nm nname)
                         (value (loop value subst))
                         (body (loop body nsubst)))))
;= [[letrec]] node introduces new name --- [[nm]], it is visible both in body
;= and value subnodes.
         (letrec
           (let* ((nname (gensym))
                  (nsubst `((,nm ,nname) ,@subst)))
             (ast:mknode (nm nname)
                         (value (loop value nsubst))
                         (body (loop body nsubst)))))
;= [[letrecr]] node introduces a bunch of mutually referenced new names.
         (letrecr
          (let* ((nnames (map (fun (_) (gensym)) dfs))
                 (nsubst (append
                           (zip (map car dfs) nnames) subst)))
            (ast:mknode (dfs (foreach-map (d (zip nnames dfs))
                               (format d (nnm (nm value))
                                  `(,nnm ,(loop value nsubst)))))
                        (body (loop body nsubst)))))
;= Match may introduce names inside patterns, see [[ml-rescope-pattern]] for
;= details.
         (match
            (ast:mknode
             (arg (loop arg subst))
             (ps (foreach-map (pt ps)
                   (format pt (p v)
                    (collector (add get)
                     (let* ((np (ml-rescope-pattern add p))
                            (nsubst (append (get) subst)))
                       (list np (loop v nsubst)))))))))
;= And [[var]] nodes may contain references to locally bound symbols
         (var
          (let* ((t (lookup-env-car subst nm)))
            (if t (ast:mknode (nm t)) node)))
;= All other variatns of [[expr]] can be safely processed with a DEEP--strategy.
         (else-deep
          (forall node)))))))

;= Get rid of uncurried functions:
(function ml-curry-expr ( expr )
  (mlsrc:visit expr expr
     (expr DEEP
       ((uncurriedfun
         (let loop ((a args))
           (p:match a
             (() body)
             (($hd . $tl) `(fun ,hd ,(loop tl))))))
        (makelist
         (let loop ((e args))
           (p:match e
             (() `(constr Nil))
             (($hd . $tl) `(constr Cons ,hd ,(loop tl))))))
        (cons
         `(constr Cons ,hd ,tl))
        (append
         `(apply (apply (var append) ,a) ,b))
        (matchfun
         (alet farg (gensym)
               `(fun ,farg (match (var ,farg) ,@ps))))
        (if2
          `(match ,v ((bool #t) ,tr) ((bool #f) (unit))))
        (if3
         `(match ,v ((bool #t) ,tr) ((bool #f) ,fl)))
        (apply0
         (let loop ((a args) (f fn))
           (p:match a
             (($v) `(apply ,f ,v))
             (($ah . $r)
              (loop r `(apply ,f ,ah))))))
        (else node)))
     (pattern DEEP
        ((bindany
          `(bind ,nm (any)))
         (cons
          `(constr Cons ,a ,b))
         (else node)))
       ))

;= And a simple interface function to rescope top level expressions:
(function ml-rescope ( texpr )
  (mlsrc:visit topexpr texpr
     (expr _ (forall
              (ml-rescope-expr
                      (ml-curry-expr
                       node))))))

;-
;- \subsubsection{Types pretty printing}
;-

;= A greek alphabet for type variables
(define neat-args
  '(alpha beta gamma delta epsilon zeta eta
    theta iota kappa lambda mu nu xi omicron
    pi rho sigma tau upsilon phi chi psi omega))

;= Prepare a type for pretty--printing: rename type variables
;= using a sequence of greek letters.
(function mlpprint-type-prepare (tpy)
  (let* ((na (cons nil neat-args))
         (nexta (fun ()
                  (if (null? (cdr na)) (gensym)
                      (let* ((nn (cadr na)))
                        (set-cdr! na (cddr na))
                        nn)))))
    (with-hash (ht)
       (mlsrc:visit type tpy
         (type DEEP
            ((V (alet nn (ht> nm)
                      (if nn `(V ,nn)
                          (alet nx (nexta)
                             (ht! nm nx)
                             `(V ,nx)))))
             (else node)))))))

;= Auxillary function --- pretty print a left value
(function dexl (x)
  (p:match x
    (($a) (S<< "(" a ")"))
    ($b b)))

;= Auxillary function --- pretty print a right value
(function dexr (x)
  (p:match x
    (($a) a)
    ($b b)))

;= Pretty print a type, omitting brackets around right values:
(function mlpprint-type (tpy)
  (dexr
   (mlsrc:visit type (mlpprint-type-prepare tpy)
     (type DEEP
        ((V (S<< "'" nm))
         (F (list (S<< (dexl t) " -> " (dexr r))))
         (TPL (list (strinterleave (map dexl args) " * ")))
         (T (S<< (strinterleave (map dexl args) " ") " " nm)))))))

;-
;- \subsubsection{Environment updates}
;-
;- ML types are handled in an environment in cached form --- types could be
;- fetched by the variant constructor names, by type names or through a chain
;- of aliases. It means that each type should be prepared in a special
;- way before it can be added to the environment.
;-

;= Follow a chain of aliases for a given type:
(recfunction mllookuptype (env nm nargs)
  (alet r (hashget env (S<< "aliastype: " nm))
    (if (not r) `(T ,nm ,@nargs)
        (format r (args ntype)
           (if (not (= (length args) (length nargs)))
               (ccerror `(ML:ALIAS (,ntype ,args) -> (,nm ,nargs))))
           (mllookuptype env ntype args)))))

(function ml-follow-aliases ( env td )
  (mlsrc:visit typedef td
     (type DEEP
       ((T (mllookuptype env nm args))
        (else node)))))

;= Introduce new types into environment:
(function register-deftype ( env typedefs )
  (collector (add get)
   (foreach (t typedefs)
    (mlsrc:iter typedef t
       (typedef _
          ((alias (begin
                    (add `(,a ,b))
                    (hashput env (S<< "typedef: " a) node)
                    (hashput env (S<< "type: " a) `(,args ,b))
                    (hashput env (S<< "aliastype: " a) `(,args ,b))
                    )
                  )
           (else nil)))))
   ;todo: deal with aliases
   (foreach (t typedefs)
     (mlsrc:iter typedef (ml-follow-aliases env t)
       (typedef _
          ((alias nil)
           (variant
            (begin
             (hashput env (S<< "typedef: " a) node)
             (hashput env (S<< "type: " a) `(,args ,a))
             (hashput env (S<< "revconstr: " a) (map car b))
             (alet ntype `(T ,a ,@args)
              (iter-over b
                (fmt (cname . tpl)
                     (hashput env (S<< "constr: " cname)
                              `(,ntype ,@tpl)))))))))))))

;= Register a global name type (a variable or a function)

(function register-globname ( env name type )
  (if (not (hashget env (S<< "rename: " name)))
      (begin
        (println (S<< name "::" (mlpprint-type type)))
        (hashput env (S<< "rename: " name) (gensym))
        (hashput env (S<< "typeof: " name) type))))

;= Register an internal name for ML global variable or function name.
(function mlrenamed (env name)
  (alet res (hashget env (S<< "rename: " name))
        (if res res name)))

;= Despatch the top level expression for registration.
(function register-top (env tlev)
  (mltyped:iter topexpr tlev
     (topexpr _
       ((mllet (register-globname env nm (car value)))
        (mlletrec (register-globname env nm (car value)))
        (mlletrecr
         (foreach (d dfs)
           (format d (nm value)
                   (register-globname env nm (car value)))))
        (else nil)
        ))))


;-
;- \subsubsection{Code generation}
;-
;- Since it is just a toy language, we will not implement any complicated code
;- generation (i.e., no currying optimisations at all). Our compiler will
;- generate simple MBase code directly, we will even reuse MBase own pattern
;- matching. Tagged unions are also represented in inefficient way --- as MBase
;- lists (and as a side effect --- this toy ML will be compatible with MBase
;- ASTs).
;-

(function ml-deref-force (v)
  (p:match v
    ((Deref $f)
     (let ((vl (f nil)))
       (set-car! v 'Ref)
       (set-car! (cdr v) vl)
       vl))
    ((Ref $vl) vl)))

;= Compile simple expression:
(function compile-ml ( env typed )
  (mltyped:visit expr typed
    (expr DEEP e) ; get rid of types
    (pattern DEEP p)
;= Expressions:
    (oexpr DEEP
      ((let
           `(alet ,nm ,value ,body))
       (letrec
           (p:match value
             ((fun $arg . $r)
              `(let ((,nm (inner.reclambda ,nm ,arg ,@r)))
                 ,body))
             (else
              `(letrec ((,nm ,value)) ,body))))
       (letrecr
           `(letrec ,dfs ,body))
       (begin
           `(begin ,@es))
       (fun
           `(fun (,arg) ,body))
       (apply
           `(,fn ,arg))
;= More expression types:
       (constr
        (if (eqv? nm 'Deref)
           `(list 'Deref (fun (_) ,(car args)))
           `(list (quote ,nm) ,@args)))
       (tuple
           `(list ,@args))
       (var (mlrenamed env nm))
       (number v)
       (char   v)
       (string v)
       (bool (p:match v
               (true #t)
               (false #f)
               (else v)))
       (unit 'nil)
       (match
        `(p:match ,arg ,@ps))))
;= Pattern body, compiling into [[p:match]] format.
    (opattern DEEP
     ((bind ; TODO: support other but 'any' binds
       (p:match p
         ((any) (Sm<< "$" nm))
         (else
          `(,(Sm<< "$$AS:" nm) ,@p))))
      (constr
       (if (eqv? nm 'Deref)
           `($$FF ml-deref-force ,@(car args))
           `(,nm ,@args)))
      (tuple args)
      (number v)
      (char   `($$F (fun (x) (eq? x ,v))))
      (string v)
      (bool   `($$F (fun (x) ,(if v '(if x #t nil)
                                    '(if x nil #t)))))
      (unit  '())
      (any '$_)
      ))))

;= A special case: generating a curried "binding" for MBase function.
(function compile-ml-ffi ( tp fname)
  (let loop ((t tp) (args nil))
    (p:match t
      ((F $frm $too)
       (loop too (cons (gensym) args)))
      (else
       (let iloop ((a args))
         (p:match a
           (($a . $tl) `(fun (,a) ,(iloop tl)))
           (() `(,fname ,@args))))))))

;= Compiling a top level expression
(function compile-ml-top ( env tped )
  (mltyped:visit topexpr tped
    (topexpr DEEP
      ((mlffi `(define ,(mlrenamed env nm)
                 ,(compile-ml-ffi tp mbasefun)))
       (mlexport
        (alet args (formap (i 0 nargs) (gensym))
         `(define ,(Sm<< exportname)
            (fun ,args
              ,(let loop ((a args) (res (mlrenamed env nm)))
                 (p:match a
                   (($hd . $tl) (loop tl `(,res ,hd)))
                   (else res)))))))
       (mbinclude
        (alet res `(include ,fname)
          res))
       (mllet `(define ,(mlrenamed env nm) ,value))
       (mlletrec (p:match value
                   ((fun ($a) . $b)
                    `(recfunction ,(mlrenamed env nm) (,a) ,@b))
                   (else
                    `(define ,(mlrenamed env nm) ,value))))
       (mlletrecr
        (let* ((body
                (foreach-map (d dfs)
                  (format d (nm value)
                          (p:match value
                            ((fun ($a) . $b)
                             `(,(mlrenamed env nm) (fun (,a) ,@b)))
                            (else
                             `(,(mlrenamed env nm) ,value))))))
               (tn (gensym)))
          `(top-begin
             (define ,tn
               (letrec ,body
                 (list ,@(map car body))))
             ,@(mapi
                 (fun (i v)
                    `(define ,(car v) (nth ,i ,tn)))
                 body
                 ))))
       (mlexpr value)
       (else nil)
       ))
;= Each expression found under the top level is compiled prior to compiling
;= the top level one.
    (expr _
          (compile-ml env node))))

;= And finally, the main entry point for our ML implementation: reads
;= a sequence of expressions and returns a list of MBase statements to compile.
(function ml-driver ( env toplevs0 )
  (collector (add get)
  (let loop ((toplevs toplevs0))
;= First pass: register known types, process file includes, compile binding
;= functions.
   (foreach (t toplevs)
    (try
     (mlsrc:iter topexpr t
      (topexpr _
        ((mltype (register-deftype env d))
         (mlffi  (begin
                   (register-globname env nm tp)
                   (add (compile-ml-top env node))
                   ))
         (mlexport (begin
                     (add (compile-ml-top env node))))
         (mlannotate (register-globname env nm tp))
         (mbinclude (add (compile-ml-top env node)))
         (mlinclude (loop
                     (peg:easyparse peg_mbaseml
                                    (peg:file->stream fname))
                     ))
         (else nil))))
     t_MBaseException
     (fun (x)
       (writeline `(ERROR IN ,t : ,(mbaseerror x))))))
;= Second pass: perform the type inference and compile all the definitions and
;= expressions.
   (foreach (t toplevs)
    (try
     (mlsrc:iter topexpr t
      (topexpr _
         ((mltype nil)
          (mlffi nil)
          (mlannotate nil)
          (mlinclude nil)
          (mbinclude nil)
          (mlexport nil)
          (else
           (let* ((src (ml-rescope node))
                  (tpy (ml-typing env (tagify src))))
             (register-top env tpy)
             (add (compile-ml-top env tpy)))))))
     t_MBaseException
     (fun (x)
       (writeline `(ERROR IN ,t : ,(mbaseerror x))))))
;= Return the collected compiled result:
  (get))))

;-
;- And a macro interface to [[ml-driver]]:
;-

(function ml-inner (envnm src)
  (try
   (let* ((src1
           (peg:easyparse peg_mbaseml src))
          (res (ml-driver (shashget (getfuncenv) envnm) src1)))
     `(top-begin
        ,@res))
   t_MBaseException
   (fun (e)
     (writeline `(TOPLEVEL-ERROR: ,(mbaseerror e)))
     `(top-begin ))))

(macro ml (src)
  (ml-inner 'ml:env (peg:str->stream src))
  )

(macro ml-file (src)
  (ml-inner 'ml:env (peg:file->stream src)))

;-
;- There must be a possibility to pass an ML environment from a DLL.
;-

(macro ml-saveenv ()
  (let ((mlenv (shashget (getfuncenv) 'ml:env)))
   `(let ((mlenv (shashget (getfuncenv) 'ml:env))
          (mlprev (shashget (getfuncenv) 'ml:prev)))
     (iter (fmt (a b) (if (not (hashget mlprev a))
                       (begin
                         (hashput mlenv a b)
                         (hashput mlprev a #t))))
           (quote
            ,(hashmap (fun (a b) `(,a ,b)) mlenv))))))


;-
;- Value pretty printing
;-

(function ml-pprint-value (vlu)
  (let loop ((v vlu) (i #f) (tl #f))
    (p:match v
      ((Cons $a (Nil)) (S<< (if tl "" "[")
                            (loop a #f #f)
                            (if tl "" "]")))
      ((Cons $a $b) (S<< (if tl "" "[") (loop a #f #f) ";" (loop b #f #t)
                         (if tl "" "]")))
      ((Nil) "[]")
      (($$M:tag . $rest)
       (S<< (if i "(")
            tag (if rest
                    (S<< " " (strinterleave (map (cut loop <> #t #f) rest) " "))
                    "")
            (if i ")")))
      ($$L:rest
       (S<< (if i "(")
            (if rest (strinterleave (map (cut loop <> #t #f) rest) " "))
            (if i ")")))
      ($$S:str
       (S<< "\"" str "\""))
      (() "NIL?")
      (else
       (if (char? v) (S<< "'" v "'") (->s v))))))

;-
;- Value lispification
;-

(function ml-to-lisp (vlu)
  (let loop ((v vlu))
    (p:match v
      ((Cons $a $b) (cons (loop a) (loop b)))
      ((Nil) nil)
      (($$M:tag . $rest) `(,tag ,@(map loop rest)))
      ($$L:tpl (map loop tpl))
      (else v))))


;-
;- \end{document}
;-