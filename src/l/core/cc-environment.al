;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{Handling backend environment}
;-

;-
;- A common name mangling definition:
;-
(define escape.net
 (let ((p0 (<r> (p.alpha | p.digit | (((! (p.alpha | p.digit)) +*) -> (fun (_) (wrap #\_)))) *)))
  (fun (sym)
    (let* ((n1 (any->string sym))
           (n2 (string->list n1))
           (n3 (list->string (p-result (p0 n2)))))
       n3))))

;{{
(recfunction dotnet:basic-constructor (type)
  (alet r (_gt_constr type (anew t_type 0))
     (if r r
         (alet pr (get_BaseType type)
            (dotnet:basic-constructor pr)))))

(function cc:check-local-method-args (chk1 nm nargs)
  nil)

(function cc:check-method-args (chk2 nm nargs)
;  (if (not (= (car chk2) nargs))
;      (cc:comperror `(CC02:N_NARGS ,nargs ,nm)))
  nil)
;}}

;- Create a new environment.
(function cc:newenv ()
  (format (cc:boot:getenv) (funhash fldhash nargshash nativs)
    (env:new:
     (_funhash funhash)
     (_fldhash fldhash)
     (_nargshash nargshash)
     (_nativshash nativs)
     (-fun-cache- (mkhash))
     )))

;- Get a core library function definition and pass it to the current environment.
(function _cc:checknative (env nm nargs)
  (alet c (ohashget (env:get: env _nativshash) nm)
     (when c
      (alet m (r_mtdf t_Runtime (S<< c)
                      (formap (i 0 nargs) t_object))
         (if (not m) (cc:comperror `(CC02:N_NARGS ,nargs ,nm ,c)))
         (alet v (cons nargs m)
           (env:set: env
                     ((global nm) v))
           v)))))

;- Generate a name for a new class for a current module.
(function cc:env:newclassname (env)
  (Sm<< (env:get: env dotnet-a0name) "." (gensym))
  )

;- Generate a name for inner class to represent a closure.
(function cc:env:innerclassname (lenv name usename)
  (env:check: (lenv (inner-class name))
     (Sm<< "c" (escape.net usename))))

;- Get a pair of a closure type and parent constructor, for a given number of arguments.
(function cc:env:genericclosuretype (env nargs)
  (alet tp (nth nargs AltClosures)
     (cons tp (dotnet:basic-constructor t_AltClosure))))

;- Generate a mangled name for a method
(function cc:localenv:methodname (lenv name usename)
  (env:check: (lenv (methodname name))
    (alet v (Sm<< "m" (escape.net usename))
          (env:set: lenv ((method name) v))
          v)))

;- Generate a mangled name for a field
(function cc:localenv:fieldname (lenv name usename)
  (env:check: (lenv (field name))
              (Sm<< "f" (escape.net usename))))

;- Create a local environment to keep things relevant to a current output class
(function cc:make:localenv (env clsname)
  (env:new: (parent-env env)
            (class-name clsname)))

;- Drop a local environment when the class is finished
(function cc:localenv:dummy (lenv)
  (env:set: lenv (local nil)))

;- Add a renaming schema
(function cc:localenv:rename (lenv oname nname)
  (env:set: lenv ((rename oname) nname))
  )

;- Check the name, using a renaming schema if necessary
(function cc:localenv:getname (lenv name)
  (alet tt (env:get: lenv (rename name))
    (if tt tt name)))

;- Prepare an environment entry for a simple function
(function cc:localenv:simple (lenv name args)
  (env:set: lenv
            (local
             (env:new: (:gen (mapi (fun (i a)
                                     `((argument ,a) ,i)) args)))))
  lenv
  )

;- Prepare an environment entry for a closure
(function cc:localenv:closure (lenv name args fargs)
  (env:set: lenv
            (local
             (env:new:
              (type 'closure)
              (:gen (mapi (fun (i a)
                            `((argument ,a) ,(+ 1 i))) fargs))
              (:gen (mapi (fun (i a)
                            `((closureargument ,a) ,i)) args)))))
  lenv
  )

;- Check if we're in a closure context
(function cc:localenv:isclosure? (lenv)
  (alet tt (env:get: (lenv local) type)
        (if tt (eqv? tt 'closure) nil)))

;- Define a method
(function cc:localenv:defmethod (lenv name value)
  (env:set: lenv  ((method name) value))
  )

;- Get current method or closure arguments
(function cc:localenv:localarg (lenv nm)
  (env:get: (lenv local) (argument nm))
  )

;- Get current closure captured environment
(function cc:localenv:clarg (lenv nm)
  (env:get: (lenv local) (closureargument nm))
  )

(function cc:constr (cls)
  (let* ((ls (a->l ((r_tbind t_Type "GetMembers") cls)))
         (flt (filter (fun (mb)
                        (t_ass? t_ConstructorInfo (r_GetType mb)))
                      ls
                      )))
    (if flt
        (car flt)
        nil)))

;- Get a global variable definition, even if it's defined in a local environment
(function cc:localenv:globalvalue (lenv nm)
  (let* ((at1 (env:get: lenv (method nm)))
         (at2 (if at1 nil (env:get: lenv (field  nm))))
         (at3 (if (or at1 at2) nil (env:get: lenv (global nm))))
         (at4 (if (or at1 at2 at3) nil
                  (hashget
                   (env:get: (lenv parent-env) -fun-cache-) (S<< nm " ref"))))
         )
    (cond
     (at4 (p:match at4
            ((method $m) at4)
            ((class $cls)
             `(method ,(cc:constr cls)))
            (else (cc:comperror `(CC03:STRANGECACHE ,at4)))))
     (at1 at1)
     (at2 `(field ,at2))
     (at3 at3)
     (else
      (let* ((env (env:get: lenv parent-env))
             (at3
              (alet n0 (hashget (env:get: env _funhash) nm)
                    (if n0 `(method ,n0)
                        (alet n1
                          (hashget (env:get: env _fldhash) nm)
                          (if n1 `(field ,n1) nil)))))
             (at4 (shashget (getfuncenv) nm)))
        (cond
         (at3 at3)
         (at4 `(interp ,nm))
         (else (cc:comperror `(CC03:MISSING-GLOBAL ,nm)))
         ))))))

;- Get a local method or field
(function cc:localenv:localvalue (lenv nm)
  (let* ((at1 (env:get: lenv (method nm)))
         (at2 (env:get: lenv (field  nm))))
    (cond
     (at1 at1)
     (at2 `(field ,at2))
     (else
      (let ((tst (hashget
                   (env:get: (lenv parent-env) -fun-cache-) (S<< nm " ref"))))
        (p:match tst
          ((method $mm) `(cached ,mm))
          ((class $cl) `(cached ,(cc:constr cl)))
          (else
           (cc:comperror `(CC03:NONLOCAL ,nm)))))))))

;- Add a field to be emitted
(function cc:localenv:addfield (lenv nm type)
  (env:collect: lenv fields `(,nm ,type))
  )

;- Add some code to a given init method
(function cc:localenv:addinit (lenv mode code)
  (when code (env:collect: lenv (init mode) code)))

;- Get a collected initialisation code
(function cc:localenv:getinit (lenv mode)
  (let* ((res (reverse (env:get: lenv (init mode))))
         (chk0 (foreach-mappend (i res) i))
         (chk (filter (fun (x)
                        (p:match x
                          ((debugpoint . $_) nil)
                          (else #t))) chk0)))
    (if chk res nil)))

;- Get a list of fields to be emitted
(function cc:localenv:getfields (lenv)
  (reverse (env:get: lenv fields)))

;- Get a parent environment
(function cc:localenv:env (lenv)
  (env:get: lenv parent-env)
  )

;- Code generation helpers
(function cc:tail (tail?)
  (ctime (if ##prefix-tail
             '(if tail?  '((Tailcall)) nil)
             'nil)))

(function cc:methodinfo (mi) (cdr mi))

(function cc:env:fcall-helper-chk2 (env lenv nm nargs)
  (alet n0 (hashget (env:get: env _funhash) nm)
   (cond
    (n0 (cons
         (hashget (env:get: env _nargshash) nm)
         `(method ,n0)))
    (else
     (alet n1 (hashget (env:get: env _fldhash) nm)
       (cond
        (n1 (cons '_ `(gfield ,n1)))
        (else
         (alet nn (_cc:checknative env nm nargs)
           (cond
            (nn nn)
            (else
             (alet tt (shashget (getfuncenv) nm)
               (if tt
                   `(_ intrvalue ,nm)
                   nil)
               )))))))))))

(force-class-flush)

(define cc:emit-symbol-stub (mkref))

(function cc:env:fcall-helper-mi (env lenv mi nm nargs tail?)
  (p:match mi
    ((method $minf)
     `(,@(cc:tail tail?)
       (Call ,minf)))
    ((field $nm)
     `((Ldsfld (field ,nm))
       ,@(cc:tail tail?)
       (Call ,(nth nargs (car ms_Call_RevGenerics)))))
    ((gfield $finf)
     `((Ldsfld ,finf)
       ,@(cc:tail tail?)
       (Call ,(nth nargs (car ms_Call_RevGenerics)))))
    ((intrvalue $nm)
     (alet at1 (env:get: lenv (intersymbol nm))
       `(
         ,@(if at1 `((Ldsfld (field ,at1)))
             (alet ss (gensym)
                (cc:localenv:addfield lenv ss 'object)
                (cc:localenv:addinit lenv 'zero
                                     `((Ldsfld ,f_Pc_symbols)
                                       ,@((car cc:emit-symbol-stub) lenv nm)
                                       (Call ,m_getSItem)
                                       (Stsfld (field ,ss))
                                       ))
                (env:set: lenv ((intersymbol nm) ss))
                `((Ldsfld (field ,ss)))
                ))
         ,@(cc:tail tail?)
         (Call ,(nth nargs (car ms_Call_RevGenerics))))))
    (else `(,@(cc:tail tail?) (Call ,mi)))
    ))

(function cc:env:function-call (env lenv nm nargs tail?)

  (let* ((chk1  (env:get: lenv (method nm)))
         (chk11 (env:get: lenv (global nm)))
         (chk2 (if chk11 (cons '_ chk11)
                   (cc:env:fcall-helper-chk2 env lenv nm nargs)
                   ))
         (at4
          (hashget
           (env:get: (lenv parent-env) -fun-cache-) (S<< nm " ref")))
         )
    (alet res
     (cond
      (at4
       (p:match at4
           ((method $minf)
            `(,@(cc:tail tail?) (Call ,minf)))
           ((class $cinf)
            `((Newobj ,(cc:constr cinf))))
           (else (ccerror `(IMPOSSIBLE ,at4)))
           ))
      (chk1
       (begin
         (cc:check-local-method-args chk1 nm nargs)
         (p:match chk1
           ((method $mnm . $_)
            `(,@(cc:tail tail?) (Call (method ,mnm))))
           ((constr $mnm . $_)
            `((Newobj (method ,mnm))))
           (else (ccerror `(IMPOSSIBLE ,chk1)))
           )))
      (chk2
       (alet mi (cc:methodinfo chk2)
             (when mi
                   (cc:check-method-args chk2 nm nargs))
             (cc:env:fcall-helper-mi env lenv mi nm nargs tail?)
             ))
      (else
       (cc:comperror `(CC03:UNKNOWN-METHOD ,nm))))

     res
     )))

(function cc:env:generic-call (env nargs)
  ;TODO:
  (when (>= nargs SMAXARGS) (cc:comperror `(CC03:TOO-MANY-ARGS ,nargs)))
  (alet mtd (nth (- nargs 1) (car ms_Call_Generics))
    `((Call ,mtd))))

(function cc:localenv:closurerunner (lenv nm0)
  (let* ((nm (cc:localenv:getname lenv nm0))
         (cls (cc:env:innerclassname lenv nm nm)))
    `(method ,(Sm<< cls "/run"))))

(function cc:dotnet:nofargs (mtdi)
  (length (a->l (__get_Parameters mtdi))))


(function cc:env:delegate (lenv mtdi)
  (p:match mtdi
    ((method $nm) ;; a local method
     (let* ((mnm (env:get: lenv
                           (localmethod-nargs nm))))
       (if mnm
           (_gt_constr (nth mnm AltFuns) (mkvector (list t_object t_IntPtr)))
           (cc:comperror `(CC03:INTERNAL DELEGATE ,mtdi)))))
    (else ;; .NET method info is provided
     (alet mnm (cc:dotnet:nofargs mtdi)
           (_gt_constr (nth mnm AltFuns) (mkvector (list t_object t_IntPtr)))))
     ))

(function cc:defmethod (env mtd code)
  (env:set: env ((global mtd) code)))

(function cc:localenv:defglobal (lenv name rename)
  (env:set: lenv ((global name) rename)))


;- Delayed definitions

(function cc:localenv:defdelegate (lenv name newnm)
  (env:collect: lenv delegates `(,name ,newnm))
  (env:set: lenv ((localdelegate name) newnm))
  )

(function cc:localenv:getdelegates (lenv)
  (env:get: lenv delegates)
  )

(function cc:localenv:symbol (lenv value)
  (let ((at1
         (env:get: lenv (symbol value)))
        (at2
         (env:get: (lenv parent-env) (symbol value))))
    (if at1 `(field ,at1) at2)))

(function cc:localenv:regsymbol (lenv value fnm)
  (env:collect: lenv symbols `(,value ,fnm))
  (env:set: lenv ((symbol value) fnm)))


;-

(function cc:env:getmodule (env)
  (env:get: env dotnet-module))

(function cc:env:defmodule:strong (env nm dll? version keyfile)
  (let* ((asm (if (and version keyfile)
                   (make-strong-assembly nm version keyfile)
                   (make-assembly nm)))
         (dbg (if (shashget (getfuncenv) 'compiler-debug-enabled) #t bool-false))
         (_ (if dbg (clr:mark-assembly-debuggable asm)))
         (aname (if (eqv? dll? 'dll)
                    (S<< nm ".dll")
                    (S<< nm ".exe")))
         (mdl (make-module-s asm (S<< aname) aname dbg)))
    (env:set: env (dotnet-module mdl))
    (env:set: env (dotnet-assembly asm))
    (env:set: env (dotnet-aname aname))
    (env:set: env (dotnet-a0name nm))
    (env:set: env (dotnet-type dll?))
    (collector (add get)
               (env:set: env
                        (modcollector-add add)
                        (modcollector-get get)))
    ))

(function cc:env:defmodule (env nm dll?)
  (cc:env:defmodule:strong env nm dll? nil nil))

(function cc:env:addrun (env mtd)
  ((env:get: env modcollector-add) mtd))


(function cc:env:report (env)
  `(
    (classes ,(env:get: env ctr-classes))
    (globals ,(env:get: env ctr-globals))
    (methods ,(env:get: env ctr-methods))
    (closures ,(env:get: env ctr-closures))
    (code-cache-hits ,(env:get: env ctr-cache))
    (const-cache-hits ,(env:get: env ctr-qcache))
    (symbols ,(env:get: env ctr-symbols))
    (delegates ,(env:get: env ctr-delegats))
    (code-cached ,(env:get: env ctr-fcached))
    ))

(function cc:env:printreport (report)
  (println "Compiler stats:")
  (foreach (r report)
    (format r (nm vl)
      (println (S<< "    " nm ": " (to-string vl)))))
  (println "---------------")
  )