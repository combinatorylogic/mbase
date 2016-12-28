;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;-
;-
;-
;; Compilation-mode macros are here:

(define *cmhash* (mkshash))

(define *comp?* (mkref))
(function cc:is-compiling? () (cdr *comp?*))

(function enter-compilation-environment ()
   (set-cdr! *comp?* #t)
   (shashput (getfuncenv) 'compiled-environment #t)
   (set-macro-env (list *cmhash* (getmacroenv))))

(function store-compilation-environment (env)
  (shashput (getfuncenv) '_____cc:compilation-environment env)
  )

(function cc:compilation-environment-from-macro ()
  (let* ((e (shashget (getfuncenv) '_____cc:compilation-environment)))
    (if e e
        (ccerror `(CC03:FLUSH-FAILED)))))

(function exit-compilation-environment ()
   (set-cdr! *comp?* nil)
   (shashput (getfuncenv) 'compiled-environment nil)
   (set-macro-env (list (getmacroenv))))

(function new:read-int-eval (expr)
  (if (cc:is-compiling?)
      (begin
        (exit-compilation-environment)
        (alet res (core:read-int-eval expr)
              (enter-compilation-environment)
              res))
      (core:read-int-eval expr)))

(force-class-flush)

(begin
  (set-macro-env (getmacroenv))
  (set-car! core:read-int-eval-hook new:read-int-eval)
  )

(force-class-flush)
;-
;-
;-

(include "./cc-expand.al");-I

;-
;- An actual code emitting/environment handling fronted.
;-

(function cc:cachepost (env lenv cls cname)
  (let* ((ch (hashget env '-fun-cache-))
         (temp (hashget ch " temporary pool ")))
    (hashput ch " temporary pool " nil)
    (foreach (t temp)
      (format t (ha nm)
         (let ((a1 (env:get: lenv (methodname nm)))
               (a2 (env:get: lenv (inner-class nm)))
               (newnm (Sm<< "cached: " cname "." nm)))
           (hashput ch ha newnm)
           (env:inc: env ctr-fcached)
           (cond
            (a2 (hashput ch (S<< newnm " ref")
                         `(class
                           ,(r_iclass cls (S<< a2)))))
            (a1 (hashput ch (S<< newnm " ref")
                         `(method
                           ,(r_metod cls (S<< a1)))))
            (else (cc:comperror `(CC04:CACHE ,nm)))
            ))))
    nil))

(function cc:driver:postprocess (env lenv cls cname)
  (let* ((syms (env:get: lenv symbols))
         (dgats (cc:localenv:getdelegates lenv))
         (qcac (env:get: lenv quotelists))
         )
    (ctime (if (shashget (getfuncenv) 'compiler-final)
               '(cc:cachepost env lenv cls cname)
               'nil))
    (foreach (s syms)
      (let* ((f (r_getField cls (S<< (cadr s)))))
        (env:inc: env ctr-symbols)
        (env:set: env ((symbol (car s)) f))))
    (foreach (d dgats)
      (format d (nm fld)
         (let* ((f (r_getField cls (S<< fld))))
           (env:inc: env ctr-delegats)
           (env:set: env ((globaldelegate nm) f)))))
    (foreach (q qcac)
      (format q (str fld)
         (let* ((f (r_getField cls (S<< fld))))
           (env:set: env ((quotecache str) f))
           )))
    ))

(function cc:driver (env st1code)
  (when (shashget (getfuncenv) 'debug-compiler-driver)
        (println '-DRIVER-ENTRY-))
  (let* (
         (invoq (shashget (getfuncenv) 'debug-compiler-invoke))
         (fl0 (cc:emit-flat env st1code))
         (flpre (cdr fl0)) (lenv (car fl0))
         (fl (cc:dotnet-plugins flpre))
         (_ (when (shashget (getfuncenv) 'debug-compiler-dotnet)
                  (println '-DOTNET-)
                  (iter writeline fl)
                  (println '-------------------DOTNET)))
         (mdl (cc:env:getmodule env))
         (cls (clr:emit.class mdl fl))
         (_ (when invoq (println (S<< "--TO-INVOKE-ON: " (->s cls)))))

         (res (if (env:get: lenv initrun)
                  (alet mtd (r_mtdf cls "run" nil)
                        (cc:env:addrun env mtd)
                        (InvokeMember cls "run" '(InvokeMethod Public Static)
                                      nil nil nil))))
         )
    (env:inc: env ctr-classes)
    (when invoq
          (println (S<< "--INVOKED " (to-string res))))
    (cc:driver:postprocess env lenv cls (->s cls))

    res))

;-
;- Interface
;-

(function cc:filterout (expr)
  (let loop ((e expr))
    (p:match e
      ((def $nm . $_)
       `(def ,nm ...))
      ((function $nm $args . $_)
       `(function ,nm ,args ...))
      ((inner.lambda $args . $_)
       `(inner.lambda ,args ...))
      ((inner.reclambda $name $args . $_)
       `(inner.reclambda ,name ,args ...))
      ((inner.defmacro $nm . $_)
       `(inner.defmacro ,nm ...))
      ((inner.defrec $nm . $_)
       `(inner.defrec ,nm ...))
      ((inner.define $nm . $_)
       `(inner.define ,nm ...))
      ((lambda $args . $_)
       `(inner.lambda ,args ...))
      ((begin . $rest) `(begin ,@(map loop rest)))
      ((inner.begin . $rest) `(inner.begin ,@(map loop rest)))
      (($hd . $tl) `(,hd ...))
      (else '...)
      )))

(function cc:errorcontext (env)
  (alet tst (env:get: env (errorc 'last))
     (when tst
        (writeline tst))))

(function cc:do-all-the-things (env expr)
 (try
 (try
  (begin
    (env:set: env ((errorc 'last) nil))
    (when (shashget (getfuncenv) 'debug-compiler-drivertop)
          (println '-SOURCE-)
          (writeline expr)
          (println '-------------------SOURCE))
    (let* ((e1 (cc:core->ast expr))
           (e1prim (cc:core-plugins e1))
           (_ (cc:core-prepare-topdefs e1prim))
           (e2 (cc:compile-stage1 env e1prim))
           (res (cc:driver env e2)))
      (when (shashget (getfuncenv) 'debug-compiler-driver)
            (println '-DRIVER-DONE-))
      res))
  t_MBaseException
  (fun (e)
    (println "COMPILER Exception:")
    (println (S<< "MBase code: " (to-string (mbaseerror e))))
    (cc:errorcontext env)
    (println (S<< "In: " (to-string (cc:filterout expr))))
    (println (->s e))
    (exit -1)
    ))
 t_Exception
 (fun (e)
   (println "COMPILER Exception:")
   (cc:errorcontext env)
   (println (S<< "In: " (to-string (cc:filterout expr))))
   (println (->s e))
   (exit -1)
   )))


;-
;- Processing input expressions
;-

(function cc:add-top (env texpr)
  (if texpr (env:collect: env TopExprs (cc:defexpand texpr))))

(function cc:flush-top (env)
  (alet exs (reverse (env:get: env TopExprs))
    (env:set: env (TopExprs nil))
    (when exs (cc:do-all-the-things env `(inner.begin ,@exs)))))

(function cc:expand-once (l)
  (p:match l
    (($$M:ss . $rest)
     (let ((sh (hashget-seq (list *cmhash* (getmacroenv)) ss)))
       (if sh
           (sh l)
           l)))
    (else l)))

(recfunction cc:toplevel-expand (env blk cnd texpr)
 (store-compilation-environment env)
 (cc:add-top env
  (if (not (list? texpr))
   (cc:toplevel-expand env blk cnd `(inner.return ,texpr))
   (fccase texpr
     ((quote) rest texpr)
     ((inner.nop) _ nil)
     ((inner.debugpoint) _ nil)
     ((top-begin inner.begin begin) rest
      (begin (foreach (r rest)
               (cc:toplevel-expand env #t cnd r))
             nil))
     ((force-class-flush inner.force-class-flush) _
      (begin (cc:flush-top env) nil))
     ((inner.topblock topblock) rest
      (begin
        (foreach (r rest) (cc:toplevel-expand env #t nil r))
        (when cnd (cc:flush-top env))
        nil))
     ((inner.late-ctime) ((_ rest))
      (begin
        (cc:flush-top env)
        (alet cod (read-int-eval `(begin ,@rest))
              (cc:toplevel-expand env #t cnd cod)
              )
        nil
        ))
     ((inner.late-ctimex.nf) ((_ rest))
      (begin (read-int-eval (cons 'begin rest))
             nil))
     ((core.define inner.define function recfunction def
       core.function core.recfunction) _
      texpr)
     ((inner.defmacro core.macro macro cmacro) _
      (begin
        (when cnd (cc:flush-top env))
        (cc:add-top env texpr)
        (when cnd (cc:flush-top env))
        nil))
     (else ;; perform one level of macro expansion & continue
      (if (and (symbol? (car texpr))
               (not (eqv? blk (car texpr))))
          (alet e1 (cc:expand-once texpr)
                (if e1 (cc:toplevel-expand env (car texpr) cnd e1) nil)
                nil)
          (begin
            (when cnd (cc:flush-top env))
            texpr)))
      ))))


(function cc:toplevel-devour (env texpr)
  (
   "A main interface to the compiler. Takes a source expression and feeds it"
   "to the compilation pipeline within a given environment."
   )
  (when (shashget (getfuncenv) 'debug-compiler-devour)
        (println '-RAW-SOURCE-)
        (println texpr)
        )
  (try
   (try
    (begin
     (enter-compilation-environment)
     (store-compilation-environment env)
     (cc:toplevel-expand env #t #t (cc:expand-once texpr))
     (alet res (cc:flush-top env)
           (exit-compilation-environment)
           res))
    t_MBaseException
    (fun (x)
      (exit-compilation-environment)
      (println (buildstring "Exception " (to-string (mbaseerror x))))
      (println (buildstring "While expanding " (to-string texpr)))
      ;(r_raise x)
      ))
   t_Exception
   (fun (x)
     (exit-compilation-environment)
     (println (buildstring "SysException " (to-string x)))
     (println (buildstring "While expanding " (to-string texpr)))
     ;(r_raise x)
     )))

(function cc:toplevel-devour-transparent (env texpr)
  ("Same as [cc:toplevel-devour], but it does not handle any exceptions.")
    (begin
      (enter-compilation-environment)
      (store-compilation-environment env)
      (cc:toplevel-expand env #t #t (cc:expand-once texpr))
      (alet res (cc:flush-top env)
            (exit-compilation-environment)
            res)))

(function cc:flush-bypass-from-macro (code)
  (let* ((env (cc:compilation-environment-from-macro)))
;;; there was a problem with PFront, looks like it is solved now.
;    (writeline `(ENV-PASSED-IS: ,env ,(cc:env:getmodule env)
;                                ,(env:get: env dotnet-aname)
;                                ,(env:get: env dotnet-assembly)))
    (cc:toplevel-expand env #t #t (cc:expand-once code))
    (cc:flush-top env)
    ))

(function defcmacro (name val)
   (shashput *cmhash* name val)
   )

(macro cmacro (name args . body0)
  "Defines a compilation mode specific macro. Same syntax as for [(macro ...)]."
  (let* ((d (doc.isdoc? body0))
         (body (if d (cdr body0) body0)))
    (if d (doc.add 'def `(macro ,name ,args ,@d)))
    `(defcmacro (quote ,name)
       (fun (macroarg)
         (let ((res (format (cdr macroarg) ,args ,@body)))
           res)))))

(include "./cc-specific.al")

(function cc:extract-method (env nm)
  (alet res (hashget (env:get: env _funhash) nm)
        (if res `(minfo ,res ,(hashget (env:get: env _nargshash) nm))
            (alet r2 (hashget (env:get: env _fldhash) nm)
                  (if r2 `(fldinfo ,r2)
                      nil)))))

(function cc:execlass (tp)
  (case tp
    ((exe) 'ConsoleApplication)
    ((winexe) 'WindowApplication)
    (else 'ConsoleApplication)))

(function cc:emit-runner (env an0 asm mdl rnl deps dlltyp)
  (let* ((c `(class (,(S<< an0 ".InitDLL")  Public BeforeFieldInit Sealed)
                    (extends "System.ValueType")
                    (field "Dummy" ,t_object (Public))
                    (field "initp" ,t_Int32 (Public Static))
                    ,@(if (not (eqv? dlltyp 'dll)) ;; exe
                         `((method ("Main" (Static Public HideBySig) (Standard) ,t_int (,t_string_array))
                                   (Call ,m_Runtime_init)
                                   (Ldarg_0)
                                   (Call ,m_Runtime_setargs)
                                   (Call (method "init"))
                                   ,@(if (shashget (getfuncenv) 'main)
                                      (alet tst (cc:extract-method env "main")
                                       (p:match tst
                                         ((minfo $mi $na)
                                          `((Call ,mi) (Pop)))
                                         ((fldinfo $fi)
                                          `((Ldsfld ,fi)
                                            (Call ,(caar ms_Call_RevGenerics))
                                            (Pop)
                                            ))
                                         (else nil)))
                                      nil)
                                   (Ldc_I4_0)
                                   (Ret)
                                   ))
                         nil)
                    (method ("initdeps" (Public Static) (Standard) ,t_void ())
                            ,@(foreach-map (d deps)
                                `(Call ,d))
                            (Ret)
                            )
                    (method ("init" (Public Static) (Standard) ,t_void ())
                            (local thisasm ,t_object)

                            (Ldsfld (field "initp"))
                            (Ldc_I4 142284)
                            (Beq (label nay))
                            (Ldc_I4 142284)
                            (Stsfld (field "initp"))
                            (Call (method "initdeps"))

;                            (Ldsfld ,f_Assmblys)
;                            ;; load this assembly name + value:
;                            (Ldtoken (this))
;                            (Call ,m_typefromhandle)
;                            (Callvirt ,m_getassembly)
;                            (Dup)
;                            (Castclass ,t_object)
;                            (Stloc (var thisasm))
;                            (Callvirt ,m_getassemblyname)
;                            (Callvirt ,m_getassemblyshortname)
;                            (Castclass ,t_object)
;                            (Ldloc (var thisasm))
;                            (Callvirt ,m_hashAdd)
;                            ;;;;;;;;;;;;;;;;;;;;;;;;;;;

                            ,@(foreach-mappend (r rnl)
                                 `((Call ,r)
                                   (Pop)))
                            (Ldsfld ,f_Pc_symbols)
                            (Ldstr "*gensym-counter-storage*")
                            (Call ,mtd_MkSymbol)
                            (Call ,m_getSItem)
                            (Castclass ,t_Pair)
                            (Ldc_I4
                             ,(+ 10000
                                 (cdr
                                  (shashget (getfuncenv)
                                           '*gensym-counter-storage*))))
                            (Box ,t_Int32)
                            (Stfld ,f_Cdr)
                            ,@(if (eqv? dlltyp 'dll)
                                  `(
                                    ;; Register into the list of dependencies
                                    (Ldtoken (method "init"))
                                    (Call ,mtd_Add_Dependency)))
                            (label nay)
                            (Ret)))))
    (let* ((cls
            (clr:emit.class mdl c))
           (mainp (when (not (eqv? dlltyp 'dll)) (r_mtd cls "Main" t_string_array))))
      (when mainp
            (_set_exe_entry_point asm mainp (cc:execlass dlltyp))
            ))
    nil))

(function cc:save-dependencies (asmname lst)
  (if lst
    (let* ((fname (S<< asmname ".d")))
      (call-with-output-file fname
        (fun (fo)
          (fprintln fo
            (S<< asmname ": "
                 (strinterleave lst " "))))))))

(function cc:dump-module (env)
  (let* ((get (env:get: env modcollector-get))
         (asm (env:get: env dotnet-assembly))
         (mdl (cc:env:getmodule env))
         (nn0 (env:get: env dotnet-a0name))
         (rnl ((env:get: env modcollector-get)))
         (dlltyp (env:get: env dotnet-type))
         (deps (_Get_Deps))
         (srcdeps (flush-target-dependencies))
         )
    (cc:emit-runner env nn0 asm mdl rnl deps dlltyp)
    (cc:save-dependencies (env:get: env dotnet-aname)
                          srcdeps)
    (asm-save (env:get: env dotnet-assembly)
              (env:get: env dotnet-aname))
    nil))

