;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;-
;- \subsection{Transforming the Core language to AST}
;-

;- Core language is what you get after all macros are expanded. It is still
;- not quite suitable for compilation, and here we'll transform it into a
;- proper, strict AST form. Some error checking is performed here, giving
;- level CC01 exceptions.
;-

;= A helper function: check if argument is a symbol.
(function cc:check-sym (s)
  (if (not (symbol? s))
      (cc:comperror `(CC01:SYM ,s)))
  s)

(function cc:nl? (v)
  (if (null? v) #t
      (if (list? v) #t
          nil)))

(function cc:chkappend (l r)
  (if (and (cc:nl? l) (cc:nl? r))
      (append l r)
      (if (cc:nl? l)
          (cc:comperror `(CC01:LIST-OR-NULL ,r))
          (cc:comperror `(CC01:LIST-OR-NULL ,l)))))

;= A helper function: check if a lambda arguments list contains only symbols.
(function cc:check-args (lst)
  (if (filter (M@ not symbol?) lst)
      (cc:comperror `(CC01:ARGS ,lst)))
  lst)

;- core $\to${} AST transformation is split into several parts, otherwise the
;- function grows too big (and an interpreted mode compiler is simply unable to
;- handle it).

(function cc:core->ast-part3 (env loop s)
  (p:match s
      ((inner.return $v) (loop v))
      ((inner.labels . $lbls)
       `(Labels ,@(map-over lbls
                    (fmt (l e)
                       `(,l ,(loop e))))))
      ((inner.switch $expr . $opts)
       `(Switch () ,(loop expr) ,@(map loop opts)))
      ((inner.nswitch $expr . $opts)
       `(Switch #t ,(loop expr) ,@(map loop opts)))
;= Arithmetics and stuff
      ((inner.arithop $op $left $right)
       `(BinOp ,(cc:check-sym op) ,(loop left) ,(loop right)))
      ((inner.car $e)
       `(Car ,(loop e)))
      ((inner.cdr $e)
       `(Cdr ,(loop e)))
      ((inner.set-car! $dst $e)
       `(SetCar ,(loop dst) ,(loop e)))
      ((inner.set-cdr! $dst $e)
       `(SetCdr ,(loop dst) ,(loop e)))
      ((inner.cons $a $b)
       `(Cons ,(loop a) ,(loop b)))
      ((inner.not $a)
       `(Not ,(loop a)))
      ((inner.eqv? $a $b)
       `(Eqv ,(loop a) ,(loop b)))
      ((inner.null? $a)
       `(NullP ,(loop a)))
      ((inner.pair? $a)
       `(PairP ,(loop a)))

      ((inner.fixlocal $id)
       `(FixLocal ,id ,id))

      ((inner.debugpoint . $x)
       `(DebugPoint ,@x))

      ((inner.ghost $x)
       `(GhostValue ,x))

      ((inner.noconst $x)
       `(NoConst ,(loop x)))

;= Use with caution
      ((inner.verbatimcore ($_ $x)) x)

      ((inner.localvar $x) `(Var ,x))

;= Function application syntax:
      (($expr . $rest)
       `(App ,(loop expr) ,@(map loop rest)))
;= Literals:
      ($$S `(Str ,s))
      ($$N `(Num ,s))
      (nil '(Nil))
      ($$M (if (memq s env) `(Var ,s)
               `(Var ,s)))
      (()  '(Nil))
      (else
       (cond
        ((char? s) `(Chr ,s))
        ((boolean? s) `(Bool ,s))
;= Nothing of the above - an obvious error to report:
        (else (cc:comperror `(CC01:BASE ,s of ,(r_GetType s))))
        ))
    ))

;-
;- Next part of the same process
;-

(function cc:core->ast-part2 (env loop_outer s)
  (let ((loop (lambda (s) (loop_outer env s)))
        (mklloop (lambda (args)
                   (let ((newenv (cc:chkappend args env)))
                     (lambda (s)
                       (loop_outer newenv s))))))
  (p:match s
    ;= With, begin:
      ((inner.let $args . $body)
       (let ((lloop (mklloop (map car args))))
       `(SLet ,(map-over args
                  (fmt (nm vl)
                    `(,(cc:check-sym nm) ,(lloop vl))))
             (Begin ,@(map lloop body)))))
      ((inner.letrec $args . $body)
       (let ((lloop (mklloop (map car args))))
       `(SLetRec ,(map-over args
                            (fmt (nm vl)
                                 `(,(cc:check-sym nm) ,(lloop vl))))
             (Begin ,@(map lloop body)))))
      ((inner.begin . $rest)
       `(Begin ,@(map loop rest)))
;= Imperative looping support constructions:
      ((inner.new.while $nm $cnd . $rest)
       (let ((lloop (mklloop (list nm))))
       `(While ,(cc:check-sym nm) ,(lloop cnd)
               (Begin ,@(map lloop rest)))))
      ((inner.new.escape $nm)
       `(Escape ,(cc:check-sym nm)))
;= Platform assembly code:
      ((inner.backend.asm $use . $body)
       `(BackendAsm ,(map-over use
                         (fun (s) (let ((v (cc:check-sym s)))
                                    `(,v (Nil)))))
                    ,@body))
      ((inner.label $name)
       `(Label ,(cc:check-sym name)))
      ((inner.goto $name)
       `(GotoLabel ,(cc:check-sym name)))
      ((inner.setloc $name $val)
       `(Set ,(cc:check-sym name) ,(loop val)))
      (else (cc:core->ast-part3 env loop s))
      )))

;= Force the compiler to finish collecting the current class
(force-class-flush)

;= Convert Core expression to AST:
(function cc:core->ast ( src )
  (let loop_outer ((env nil) (s src))
  (let ((loop (lambda (s) (loop_outer env s)))
        (mklloop (lambda (args)
                   (let ((newenv (cc:chkappend args env)))
                     (lambda (s)
                       (loop_outer newenv s))))))
    (p:match s
;= Lambda, if, quote, set and let:
      ((inner.lambda $args . $body)
       (let ((lloop (mklloop args)))
       `(Fun () ,(cc:check-args args)
            (Begin ,@(map lloop body)))))
      ((inner.reclambda $name $args . $body)
       (let ((lloop (mklloop args)))
       `(Fun ,(cc:check-sym name) ,(cc:check-args args)
            (Begin ,@(map lloop body)))))
      ((inner.if $cnd $tr $fl)
       `(If ,(loop cnd) ,(loop tr) ,(loop fl)))
      ((inner.if $cnd $tr)
       `(If ,(loop cnd) ,(loop tr) (Nil)))
;= Quoted stuff
      ((quote ()) '(Nil))
      ((quote $any) `(Quote ,any))
;= Setloc, try
      ((inner.set $nm $vl)
       `(Set ,(cc:check-sym nm) ,(loop vl)))
      ((inner.try $body $ex $act)
       `(TryBlock ,(loop body) ,ex ,(loop act)))
;= Toplevel definition:
      ((def $$S:nm $vl) (loop `(def (quote ,(Sm<< nm)) ,vl)))
      ((def (quote $nm) $vl)
       (p:match vl
         ((inner.lambda $args . $rest)
          `(GSetFn nonrec ,(cc:check-sym nm) ,(length args) ,(loop vl)))
         (else
          `(GSet ,(cc:check-sym nm) ,(loop vl)))))
      ((inner.define $nm $vl)
       (let ((lloop (mklloop (list nm))))
       (p:match vl
         ((inner.lambda $args . $rest)
          `(GSetFn nonrec ,(cc:check-sym nm) ,(length args) ,(lloop vl)))
         (else
          `(GSet ,(cc:check-sym nm) ,(lloop vl))))))
      ((inner.defrec $nm $vl)
       (let ((lloop (mklloop (list nm))))
       (p:match vl
         ((inner.lambda $args . $rest)
          `(GSetFn rec ,(cc:check-sym nm) ,(length args) ,(lloop vl)))
         (else
          `(GSet ,(cc:check-sym nm) ,(lloop vl))))))
;= Macro definition
      ((inner.defmacro $nm $vl)
       (alet nm1 (p:match nm (($_ $n) n) (else nm))
             `(GSetM ,nm1 ,(loop vl))))
;= Embedded assembly
      ((inner.asm $type $args . $abody)
       `(BackendAsm ,type ,(map (fun (a) (list a `(Var ,a)))
                       (cc:check-args args)) ,@abody))

      (else (cc:core->ast-part2 env loop_outer s))
      ))))

(function cc:core-prepare-topdefs (src)
  (let ((ninit (fun (nm)
                 (if (shashget (getfuncenv) nm) nil
                     (shashput (getfuncenv) nm '*DUMMY*)))))
    (cc:mbcoreast:iter expr src
      (expr DEEP
        ((GSet (ninit nm))
         (GSetFn (ninit nm))
         (else node))))))

(include "./cc-sanitise.al")

(function cc:core-sanitise-tokens (src)
  ;;; TODO! Discover all the emit-compatible entities and
  ;;;   replace them with something serialisable.
  (cc:mbcoreast:visit expr src
     (expr DEEP
        ((BackendAsm
          (ast:mknode (body (cc:core-sanitise-tokens-inner body))))
         (else node)))))

