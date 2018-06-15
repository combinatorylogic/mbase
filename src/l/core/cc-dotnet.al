;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{Generating \NET{} code}
;-

;-
;- Emit a symbol initialisation code, if necessary.
;-

(function cc:emit-symbol ( lenv value )
  (alet chk (cc:localenv:symbol lenv value)
    (if chk `((Ldsfld ,chk))
        (let* ((fnm (gensym))
               (cod `((Ldstr ,(S<< value))
                      (Call ,mtd_MkSymbol)
                      (Stsfld (field ,fnm)))))
          (cc:localenv:addinit lenv 'zero cod)
          (cc:localenv:addfield lenv fnm 'symbol)
          (cc:localenv:regsymbol lenv value fnm)
          `((Ldsfld (field ,fnm)))))))

(set-car! cc:emit-symbol-stub cc:emit-symbol)

;-
;- Emit a constant list generator code
;-

(function cc:emit-list ( lenv atom value valstr )
  (alet res
;= Building an actual generator code
    (let loop ((v value))
      (p:match v
        (() `((Ldnull)))
        (($hd . $tl)
         `(,@(loop hd)
           ,@(loop tl)
           (Newobj ,mtd_Cons))) ; mtd_ConsTrap for debugging
        (else (atom lenv v))
        ))
;= Emitting it, substituting the whole constant expression with a reference to
;= a static field.
    (let* ((nm (gensym)))
      (cc:localenv:addinit lenv 'zero `(,@res
                                        (Stsfld (field ,nm))))
      (cc:localenv:addfield lenv nm 'object)
      (env:collect: lenv quotelists `(,valstr ,nm))
      (env:set: lenv ((quotecache valstr) `(field ,nm)))
      `((Ldsfld (field ,nm))))))

;-
;- Try to cache quoted constants
;-

(function cc:emit-list-cache (lenv atom value)
  (let* ((valstr (to-string value))
         (env (cc:localenv:env lenv))
         (cch2 (env:get: lenv (quotecache valstr)))
         (cch1 (env:get: env (quotecache valstr)))
         (cch (if cch1 cch1 cch2))
         )
    (if cch
        (begin
          (env:inc: env ctr-qcache)
          `((Ldsfld ,cch)))
        (cc:emit-list lenv atom value valstr))))

;-
;- Emit a vector constant (byte array, TODO: int)
;-

(function cc:emit-vconst (lenv value)
  (with-syms (nm)
    (env:collect: lenv quotevecs `(,nm ,value))
    (cc:localenv:addfield lenv nm 'object)
    (cc:localenv:addinit lenv 'zero
      `(
        ,(_ldc_i4 (alength value))
        (Newarr ,t_Byte)
        (Dup)
        (Ldtoken (field ,(Sm<< nm "_init")))
        (Call ,mtd_InitializeArray)
        (Castclass ,t_object)
        (Stsfld (field ,nm))
        ))
    `((Ldsfld (field ,nm)))
    ))

;-
;- Emit a quoted value generator code
;-

(recfunction cc:emit-quote (lenv value)
  (cond
    ((null? value) `((Ldnull)))
    ((string? value) `((Ldstr ,value)))
    ((number? value) `((Ldc_I4 ,value)
                       (Box ,t_Int32)))
    ((char? value) `((Ldc_I4 ,(ascii value))
                     (Box ,t_Char)))

    ((boolean? value)
     `(,(if (r_debool value)
            `(Ldsfld ,fld_True)
            `(Ldsfld ,fld_False))))
    ((symbol? value)
     (cc:emit-symbol  lenv value))
    ((vector? value)
     (cc:emit-vconst lenv value))
    ((list? value)
     (cc:emit-list-cache lenv cc:emit-quote value))))

;-
;- Emit a delegate initialisation code
;-

(function cc:emit-delegate (env lenv name mtdi)
  (alet dgtry (env:get: env (globaldelegate name))
    (if dgtry `((Ldsfld ,dgtry))
        (let* ((newnm (gensym))
               (delgate (cc:env:delegate lenv mtdi)))
          (cc:localenv:addfield lenv newnm 'object)
          (cc:localenv:addinit lenv 'zero
                               `((Ldnull)
                                 (Ldftn ,mtdi)
                                 (Newobj ,delgate)
                                 (Stsfld (field ,newnm))
                                 ))
;= Delay a compiler environment delegate registration until the
;= current type is finalised.
;= This should not go into compiled code, each module starts with a
;= clean delegates cache.
          (cc:localenv:defdelegate lenv name newnm)
          `((Ldsfld (field ,newnm)))))))

;-
;- Emit an in place atom generator code (not a static field constant)
;-
(recfunction cc:atom->il (env lenv value)
  (p:match value
    ((Var $nm)
     `((Ldloc (var ,nm))))
    ((Arg $nm)
     `(,(_ldarg (cc:localenv:localarg lenv nm))))
    ((GlobRec $nm)
     (if (cc:localenv:isclosure? lenv)
         `(,(_ldarg 0))
         (cc:atom->il env lenv `(Glob ,nm))))
    ((Glob $nm0)
     (let* ((nm (cc:localenv:getname lenv nm0))
            (lc (cc:localenv:globalvalue lenv nm)))
       (p:match lc
         ((field $rf) `((Ldsfld ,(if (symbol? rf) `(field ,rf) rf))))
         ((method $mtd)
          (cc:emit-delegate env lenv nm (if (symbol? mtd) `(method ,mtd) mtd)))
         ((interp $nm)
          (alet at1 (env:get: lenv (intersymbol nm))
             (if at1 `((Ldsfld (field ,at1)))
                (alet ss (gensym)
                      (cc:localenv:addfield lenv ss 'object)
                      (cc:localenv:addinit lenv 'zero
                           `((Ldsfld ,f_Pc_symbols)
                             ,@(cc:emit-symbol lenv nm)
                             (Call ,m_getSItem)
                             (Stsfld (field ,ss))
                             ))
                      (env:set: lenv ((intersymbol nm) ss))
                      `((Ldsfld (field ,ss)))
                      ))))
         (else (cc:comperror `(CC03:WRONGGLOBAL ,nm ,lc))))))
    ((Meta (Glob $nm0)) ;; local refs only
     (let* ((nm (cc:localenv:getname lenv nm0))
            (lc (cc:localenv:localvalue lenv nm)))
       (p:match lc
         ((method $rf) `((Ldtoken (method ,rf))))
         ((field  $rf) `((Ldtoken (field ,rf))))
         ((cached $other)
          `((Ldtoken ,other)))
         (else (cc:comperror `(META ,nm ,lc)))
         )))
    ((IntPtr (Glob $nm0))
     (let* ((nm (cc:localenv:getname lenv nm0))
            (lc (cc:localenv:globalvalue lenv nm)))
        (p:match lc
          ((field $rf) `((Ldsfld ,rf)))
          ((method $mtd)
           (cc:emit-delegate env lenv nm (if (symbol? mtd) `(method ,mtd) mtd))
           )))
     )
    ((Meta $other) (cc:comperror `(CC03:WRONGMETA ,other)))
    ((IntPtr $other) (cc:comperror `(CC03:WRONGPTR ,other)))
    ((Clenv $nm) `((Ldarg_0) ; 'this' pointer - we're in a closure runner
                   (Ldfld (field ,(Sm<< "f" (cc:localenv:clarg lenv nm))))))
    ((Str $str) `((Ldstr ,str)))
    ((Num $n) `((Ldc_I4 ,n)
                (Box ,t_Int32)))
    ((NBNum $n) `((Ldc_I4 ,n)))
    ((Chr $c) `((Ldc_I4 ,(ascii c))
                (Box ,t_Char)))
    ((Bool $b)
     `(,(if (r_debool b) `(Ldsfld ,fld_True)
              `(Ldsfld ,fld_False))))

    ((Nil) `((Ldnull)))

    ((Quote $v)
     (cc:emit-quote lenv v))
    ((else)    (cc:comperror `(CC03:WRONGMODE)))
    ))

;- Emit an application statement

(function cc:app->il ( tail? env lenv fn args )
  (alet res
   (p:match fn
      ((GlobRec $nm0)
       (if (cc:localenv:isclosure? lenv)
           `(,@(cc:tail tail?)
             (Call ,(cc:localenv:closurerunner lenv nm0)))
           (cc:env:function-call env lenv (cc:localenv:getname lenv nm0)
                                 (length args) tail?)))
      ((Glob $nm0)
       (alet nm (cc:localenv:getname lenv nm0)
             (cc:env:function-call env lenv nm
                                   (length args) tail?)))
      ((Gencall $num)
       (alet xfn (cc:env:generic-call env (length args))
         ;TODO: add length check here!
         `(,@(cc:atom->il env lenv fn)
           ,@(cc:tail tail?)
           ,@xfn)))
      (else
       (alet xfn (cc:env:generic-call env (length args))
         ;TODO: add length check here!
         `(,@(cc:atom->il env lenv fn)
           ,@(cc:tail tail?)
           ,@xfn))))
   res
   ))

;- Emit a global definition
(function cc:emit-setglobal (env lenv name)
  (let* ((fnm (cc:localenv:fieldname lenv name name)))
    `((Stsfld (field ,fnm)))))

;- Flat body to IL transformation loop
(force-class-flush)

(function cc:flat-body-inner (env lenv ret? c)
  (cc:flatast:visit code c
    (code _
      ((Local `((local ,name ,t_object)))
       (FixLocal `((fixlocal ,name ,oldname)))
       (DebugPoint `((debugpoint ,@d)))
       (Label `((label ,lbl)))
       (Iflabel `((label ,lbl)))
       (Goto `((Br (label ,lbl))))
       (Setlocal `(,@(cc:atom->il env lenv value)
                   (Stloc (var ,id))))
       (Setnewglobal
        (cc:emit-setglobal env lenv nm))
       (Push (cc:atom->il env lenv value))
       (Pushapp (cc:app->il nil env lenv fn args))
       (Setlocapp
        `(,@(cc:app->il nil env lenv fn args)
          (Stloc (var ,nm))))
       (Retapp
        `(,@(cc:app->il ret? env lenv fn args)
          ,@(if ret? `((Ret)) `((Pop)))))
       (Dropapp
        `(,@(cc:app->il nil env lenv fn args)
          (Pop)))
       (Return
        `(,@(cc:atom->il env lenv value)
          ,@(if ret? `((Ret)) `((Pop)))))
       (Pop '((Pop)))
       (Switch `((Switch ,labels)))
       (Gotoif `((Brtrue (label ,lbl))))
       (GotoNull `((Brfalse (label ,lbl))))
       (GotoPairP
         `((Isinst ,t_Pair)
           (Brtrue (label ,lbl))))
       (GotoEqv `((Beq (label ,lbl))))
       (Gotoifnot
         `((Brfalse (label ,lbl))))
       (Patchclosure
        (alet clname (cc:env:innerclassname lenv liftid liftid)
          `(
            (Ldloc (var ,id))
            (Castclass (class ,clname))
            ,@(foreach-mappend (i args)
                `((Dup)
                  (Ldloc (var ,(cadr i)))
                  (Stfld (field ,(Sm<< clname "/f" (car i)))))
                )
            (Pop)
            )))

       (ClosureDummy
        (if (cc:localenv:isclosure? lenv)
            `(,(_ldarg 0))
            nil))

       (TryBegin `((try-block)))
       (CatchBegin `((catch-block ,(read-int-eval ex))))
       (TryEnd `((end-try-block)))

       (InitGlobalVar `((Call ,mtd_register_field)))
       (InitGlobalFun `((Call ,mtd_register_method)))
       (InitGlobalMac `((Call ,mtd_register_macro)))

       (Car
        `((Castclass ,t_Pair)
          (Ldfld ,_car_fld)))

       (Cdr
        `((Castclass ,t_Pair)
          (Ldfld ,_cdr_fld)))

       (SetCar
        `((Stfld ,_car_fld)))
       (SetCdr
        `((Stfld ,_cdr_fld)))
       
       (CastPair
        `((Castclass ,t_Pair)))

       (Cons `((Newobj ,mtd_Cons)))
       (Cons1 `((Newobj ,mtd_Cons1)))
       (Cons0 `((Newobj ,mtd_Cons0)))
       
       (BinOp `((,op)))
       (IntBox `((Box ,t_Int32)))
       (IntUnbox `((Unbox ,t_Int32) (Ldind_I4)))

       (Asm
        (with-hash (h1)
          (foreach (v use)
            (h1! (car v) (cadr v)))
          `(
            ,@(foreach-mappend (b body)
                (p:match b
                  ((expr $nm)
                   (cc:atom->il env lenv (hashget h1 nm)))
                  (else (list b))))
            ,@(p:match tgt
                ((local $nu)
                 `((Stloc (var ,nu))))
                (push nil)
                (tail `((Ret)))
                (drop `((Pop)))
                (else (cc:comperror `(CC03:ASM-TARGET ,tgt))))
            )))
       (else nil))))
  )
(force-class-flush)

(function cc:flat-body->il ( env lenv code ret? )
  (foreach-mappend (c code)
     (cc:flat-body-inner env lenv ret? c)))

;- Emit a closure inner class. Constructor is a closure maker part, and Run
;- method is a closure body.

(function cc:emit-closure ( env clsenv name args fargs code )
  (let* ((iclassname (cc:env:innerclassname clsenv name name))
         (nargs (length args))
         (partype (cc:env:genericclosuretype env (length fargs)))
         (parconstr (cdr partype))
         )
    `(class (,(S<< iclassname) NestedPublic)
            (extends ,t_AltClosure)
            (implements ,(car partype))
            ,@(formap (i 0 nargs)
                      `(field ,(S<< "f" i) ,t_object (Public)))
            (constructor ("make" (Public HideBySig) (Standard)
                          ,(map (fun (_) t_object) args))
                (Ldarg_0)
                (Call ,parconstr)
                ,@(foreach-mappend
                    (l (formap (i 1 (+ 1 nargs))
                        `((Ldarg_0)
                          ,(_ldarg i)
                          (Stfld (field ,(Sm<< "f" (- i 1))))))) l)
                (Ret)
                )
            (method ("run" (Public) (Standard) ,t_object
                     ,(map (fun (_) t_object) fargs))
                ,@(cc:flat-body->il env (cc:localenv:closure clsenv
                                                             name args fargs)
                                    code #t))
            )))

;- Register a closure signature in a class local environment

(function cc:emit-regclosure (lenv name usename args fargs)
  (let* ((iclassname (cc:env:innerclassname lenv name usename)))
    (cc:localenv:defmethod lenv name `(constr ,(Sm<< iclassname "/make")))))

;- Emit a simple method using the class local environment

(function cc:emit-simple (env clsenv name args code)
  (let* ((mtdname (cc:localenv:methodname clsenv name name))
         (lenv (cc:localenv:simple clsenv name args)))
    `(method (,(S<< mtdname) (Public Static) (Standard) ,t_object
              ,(map (fun (_) t_object) args))
         ,@(cc:flat-body->il env lenv code #t))))

;- Register a simple method signature in a class local environment

(function cc:emit-regsimple (lenv name usename fargs)
  (let* ((mtdname (cc:localenv:methodname lenv name usename)))
    (env:set: lenv ((localmethod-nargs mtdname) (length fargs)))
    (cc:localenv:defmethod lenv name `(method ,mtdname))))

;- Emit an expression --- no arguments, no captured closure fields are
;- available.

(function cc:emit-expr (env clsenv code ret?)
  (cc:flat-body->il env (cc:localenv:dummy clsenv) code ret?))

;- Prepare a field for a global variable
(function cc:emit-global (clsenv name usename)
  (let* ((rename (cc:localenv:fieldname clsenv name usename)))
    (cc:localenv:defglobal clsenv name `(field ,rename))))

;-
;- Helper function: injecting toplevel definitions into environment.
;-
(function cc:emit-flat-inject (env clsenv code add)
  (cc:flatast:iter lifted code
    (liftop _
     ((Init
       (env:set: env ((errorc 'last) ".NET init expression"))
       (cc:localenv:addinit clsenv mode
                            (cc:emit-expr env clsenv e
                                          (eqv? mode 'retexpression)
                                          )))
      (Simple
       (env:set: env ((errorc 'last) (S<< ".NET simple form " usename)))
       (add (cc:emit-simple env clsenv name fargs e)))
      (Funref nil)
      (Global
       (begin
         (add `(field ,(S<< (cc:localenv:fieldname clsenv name name))
                      ,t_object (Public Static)))
         (env:set: env ((errorc 'last) (S<< ".NET value " usename)))
         (cc:localenv:addinit clsenv 'expression
                              (cc:emit-expr env clsenv e nil))
         ))
      (Closure
       (env:set: env ((errorc 'last) (S<< ".NET closure " usename)))
       (add (cc:emit-closure env clsenv name args fargs e)))
      (else nil)
      )))
  )

;-
;- [[cc:emit-flat]] generates a class out of Flat code, using the given global
;- environment.
;-
(function cc:emit-flat (env code)
  (let* ((clsname (cc:env:newclassname env))
         (clsenv (cc:make:localenv env clsname)))
;= Pass 1.: register local methods and inner classes
    (cc:flatast:iter lifted code
        (liftop _
          ((Simple
            (begin
              (env:inc: env ctr-methods)
              (cc:emit-regsimple clsenv name usename fargs))
            )
           (Closure
            (begin
              (env:inc: env ctr-closures)
              (cc:emit-regclosure clsenv name usename args fargs)
              ))
           (Funref
            (cc:localenv:rename clsenv gloname iname))
           (Global
            (
             (env:inc: env ctr-globals)
             (cc:emit-global clsenv name usename)))
           (else nil))))
;= Pass 2.: emit the code
    (collector (add get)
      (cc:emit-flat-inject env clsenv code add)
;= Emit fields
      (foreach (flds (cc:localenv:getfields clsenv))
        (p:match flds
          (($nm object)
           (add `(field ,(S<< nm) ,t_object (Public Static))))
          (($nm symbol)
           (add `(field ,(S<< nm) ,t_Symbol (Public Static))))))
      (foreach (ifl (env:get: clsenv quotevecs))
        (format ifl (nm data)
        (add `(initfield ,(S<< nm "_init") (Public Static)
                         ,@(a->l data)))))
;= Emit init methods
      (let ((init1 (cc:localenv:getinit clsenv 'expression))
            (init1e (cc:localenv:getinit clsenv 'retexpression))
            (initi (cc:localenv:getinit clsenv 'zero))
            (initm (cc:localenv:getinit clsenv 'ctime))
            (init2 (cc:localenv:getinit clsenv 'funref)))
        (when initi
           (env:set: clsenv (init0 #t))
           (add
            `(method ("init0" (Public Static) (Standard) ,t_void ())
                     ,@(foreach-mappend (i initi) i)
                     (Ret))))
        (when (or init1 init1e)
           (env:set: clsenv (inite #t))
           (add `(method ("init_expression" (Public Static) (Standard) ,t_object ())
                   ,@(foreach-mappend (i init1) i)
                   ,@(if init1e nil '((Ldnull) (Ret)))
                   ,@(foreach-mappend (i init1e) i)
                   )))
        (when (or init2 initm)
           (env:set: clsenv (initc #t))
           (add `(method ("init_ctime" (Public Static) (Standard) ,t_void ())
                   ,@(foreach-mappend (i init2) i)
                   ,@(foreach-mappend (i initm) i)
                   (Ret)
                   )))
        (when (or initi (or init1 init1e) (or init2 initm))
          (env:set: clsenv (initrun #t))
          (add `(method ("run" (Public Static) (Standard) ,t_object ())
                  (local v ,t_object)
                  ,@(if initi `((Call (method "init0"))) nil)
                  ,@(if (or init1 init1e)
                        `((Call (method "init_expression")) (Stloc (var v)))
                        `((Ldnull) (Stloc (var v))))
                  ,@(if (or init2 initm)
                        `((Ldsfld ,fld_CompMode)
                          (Brfalse (label next))
                          (Call (method "init_ctime"))
                          (label next)))
                  (Ldloc (var v))
                  (Ret))))
        )

;= Form a class statement
      (cons clsenv
       `(class (,(S<< clsname) Public BeforeFieldInit Sealed)
               (extends "System.ValueType")
               (field "Dummy" ,t_object (Public)) ; keep PEverify happy
               ,@(get) ; embed all the collected statements here
               )))))