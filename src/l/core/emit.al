;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; A highlevel bindings to the System.Reflection.Emit functionality.

(Section "System.Reflection.Emit frontend")

(using ("System.Reflection" "System.Reflection.Emit"
        "System.Diagnostics.SymbolStore")

;; .NET definitions block

  (net.types AppDomain AssemblyName AssemblyBuilder ModuleBuilder TypeBuilder Type
             FieldBuilder MethodBuilder ILGenerator OpCode OpCodes MethodAttributes
             CallingConventions AssemblyBuilderAccess Label TypeAttributes
             FieldAttributes PEFileKinds MethodInfo FieldInfo ConstructorBuilder
         ;;;
             BindingFlags Binder
             CustomAttributeBuilder
             ConstructorInfo
             ISymbolDocumentWriter
             Guid
             LocalBuilder
         ;;;
   )

  ;;; For some [unknown] reason this thing MUST be called via reflection for the
  ;;;  Dynamic module methods.
  (define tp_InvokeMember (r_bind t_TypeBuilder "InvokeMember" string t_BindingFlags
                                   t_Binder object "System.Object[]"))
  (define mi_Pars (r_tbind t_MethodInfo "GetParameters"))
  (function mi_arity (mi)
    (length (a->l (mi_Pars mi))))

  (function InvokeMember (tp name flglst bndr trgt args)
   (tp_InvokeMember tp name (enumOr t_BindingFlags flglst)
                    bndr trgt args
   ))

  (define clr:defaultInvokeFlags (enumOr t_BindingFlags '(InvokeMethod Public Static)))

  (function clr:invoke.method (tp name flags args)
   ("Invokes a dynamically created method."
    "If [flags] value is null, default ([InvokeMethod|Public|Static]) is used."
    )
    (tp_InvokeMember tp (any->string name)
                     (if (null? flags) clr:defaultInvokeFlags
                         (enumOr t_BindingFlags flags))
                     nil nil (mkovector args))
   )
  ;;;

  (define _pinvoke_maker_hook (mkref))

  (function _pinvoke_maker (cls dll name rtype artyps)
    ((car _pinvoke_maker_hook) cls dll name rtype artyps))

  (define t_void (r_typer "System.Void"))

  (define t_object_array (r_typer "System.Object[]"))

  (define _emit (r_tbind t_ILGenerator "Emit" t_OpCode))

  (define _begin_exception_block (r_tbind t_ILGenerator "BeginExceptionBlock"))
  (define _throw_exception (r_tbind t_ILGenerator "ThrowException" t_Type))
  (define _begin_catch_block (r_tbind t_ILGenerator "BeginCatchBlock" t_Type))
  (define _end_exception_block (r_tbind t_ILGenerator "EndExceptionBlock"))

  (define _empty_guid (s-> t_Guid "Empty"))

  (define clr:mark-assembly-debuggable
    (r_tsbind (dotnet "Meta.Scripting.DebugHelper") "setDebuggable"
              t_AssemblyBuilder))

  (define _define_document_0 (r_tbind t_ModuleBuilder "DefineDocument"
                                      string t_Guid t_Guid t_Guid))

  (function _define_document (mdl src)
    (_define_document_0 mdl src _empty_guid _empty_guid _empty_guid))

  (define _mark_sequence_point (r_tbind t_ILGenerator "MarkSequencePoint"
                                        t_ISymbolDocumentWriter
                                        t_int t_int t_int t_int))

  (define _emit_calli
   (r_tbind t_ILGenerator "EmitCalli"
       t_OpCode t_CallingConventions t_type "System.Type[]" "System.Type[]"))

  (define _calli_opcode (s-> t_OpCodes "Calli"))
  (define _define_label (r_tbind t_ILGenerator "DefineLabel"))
  (define _mark_label (r_tbind t_ILGenerator "MarkLabel" t_Label))
  (define _declare_local (r_tbind t_ILGenerator "DeclareLocal" t_type))

  (define _setlocalsyminfo (r_tbind t_LocalBuilder "SetLocalSymInfo" string))

  (define mb_gettype (r_tbind t_ModuleBuilder "GetType" string))
  (define create-type (r_tbind t_TypeBuilder "CreateType"))
  (define _define_method (r_tbind t_TypeBuilder "DefineMethod" string
           t_MethodAttributes t_CallingConventions t_type "System.Type[]"))
  (define _define_constructor (r_tbind t_TypeBuilder "DefineConstructor"
           t_MethodAttributes t_CallingConventions "System.Type[]"))

  (define _get_ilgenerator (r_tbind t_MethodBuilder "GetILGenerator"))
  (define _get_ilgeneratorc (r_tbind t_ConstructorBuilder "GetILGenerator"))

  (define tpb_getmethod (r_tbind t_TypeBuilder "GetMethod" string "System.Type[]"))
  (define tp_getmethod (r_tbind t_Type "GetMethod" string "System.Type[]"))
  (define tpb_override (r_tbind t_TypeBuilder "DefineMethodOverride" t_MethodInfo t_MethodInfo))
  (define tpb_addclass (r_tbind t_TypeBuilder "DefineNestedType"
                                string t_TypeAttributes t_Type "System.Type[]"))

  (define _define_dynamic_assembly
    (r_tbind t_AppDomain "DefineDynamicAssembly"
         t_AssemblyName t_AssemblyBuilderAccess))
  (define _set_entry_point
    (r_bind t_AssemblyBuilder "SetEntryPoint" t_MethodInfo t_PEFileKinds))
  (function _set_exe_entry_point (asm mtd kind)
    (_set_entry_point asm mtd (getEnum t_PEFileKinds (S<< kind))))

  (function asm-save (asm nm)
   ((r_tbind t_AssemblyBuilder "Save" string) asm nm))
  (define _define_dynamic_module (r_tbind t_AssemblyBuilder "DefineDynamicModule" string t_Boolean))
  (define _define_dynamic_module_s (r_tbind t_AssemblyBuilder "DefineDynamicModule" string string t_Boolean))

  (define _define_field
    (r_tbind t_TypeBuilder "DefineField"
            string t_type t_FieldAttributes))
  (define _define_init_field
    (r_tbind t_TypeBuilder "DefineInitializedData"
             string "System.Byte[]" t_FieldAttributes))

  (define _define_type  (r_tbind t_ModuleBuilder "DefineType" string t_TypeAttributes t_Type "System.Type[]"))

  (define __opchash (mkhash))

  (function thistype? (cls tp)
    (if (and (list? tp)
             (eqv? (car tp) 'this)) cls tp))

  (function errhashgetf (hn hs nm)
    (let ((tst (hashget hs nm)))
      (if tst tst
          (ccerror `(EMIT:NOTFOUND ,nm in ,hn WHERE CONTENTS IS ,@(hashmap (fun (a b) (Sm<< a)) hs))))))

  (macro errhashget (hs nm)
    `(errhashgetf (quote ,hs) ,hs ,nm))

  (function il_opcode (opc)
      (let ((v (hashget __opchash opc)))
        (if (null? v)
            (let ((oo (s-> t_OpCodes (symbol->string opc))))
              (hashput __opchash opc oo)
              oo)
            v)))

  (function il1 (i op)
     (_emit i (il_opcode op) ))

  (define __emithash (mkhash))

  (function il2 (i op arg)
    (let* ((nm (string-append  (->s t_OpCode) (->s (r_GetType arg))))
           (chk (ohashget __emithash nm))
           (fn (if chk chk
                   (let* ((f0
                           (r_bindx t_ILGenerator "Emit"
                                    t_OpCode (r_GetType arg))))
                     (ohashput __emithash nm f0)
                     f0))))
      (fn i (il_opcode op) arg)))

  (function il2-calli (i args)
      (format args (conv rtype atyps oatyps)
          (_emit_calli i _calli_opcode
            (if (or (not conv) (symbol? (car conv)))
                (enumOr t_CallingConventions conv)
                (car conv))
            rtype
            (if (null? atyps) (anew t_type 0) (mkvector atyps))
            (if (null? oatyps) nil (mkvector oatyps)))))

  (function il2-switch (i lhash labels)
      (il2 i "Switch" (mkvector (foreach-map (x labels)
                                             (errhashget lhash x)))))

  (function _shash (s)
    (hashmap (fun (a b) `(,a ,b)) s))

  (recfunction emit:findmethod (h p nm)
    (let ((a1 (hashget h nm)))
      (if a1 a1
          (if (null? p) (ccerror `(EMIT:METHOD ,nm))
              (emit:findmethod (caar p) (cdr p) nm)))))

  (recfunction emit:findfield (h p nm)
    (let ((a1 (hashget h nm)))
      (if a1 a1
          (if (null? p) (ccerror `(EMIT:FIELD ,nm))
              (emit:findfield (cadar p) (cdr p) nm)))))

  (define *clr:emit:docsymhash* (mkhash))

  (function _try_emit_sequencepoint (m mdl rest)
    (format rest (srcfile ln1 cf ln2 ct)
      (let ((doc
             (let ((trydoc (ohashget *clr:emit:docsymhash* srcfile)))
               (if trydoc trydoc
                   (let* ((newdoc (_define_document mdl srcfile)))
                     (ohashput *clr:emit:docsymhash* srcfile newdoc)
                     newdoc)))))
        (_mark_sequence_point m doc ln1 cf ln2 ct))))

  (function emit-instructions (m mtdhash fldhash clshash body cls path mdl)
    (let ((labels (mkhash)) (locals (mkhash)) (prevdbgline (noconst (cons 0 nil))))
      (foreach (inst body)
          (if (symbol? inst) (hashput labels inst (_define_label m))
            (if (eqv? (car inst) 'label)
               (hashput labels (cadr inst) (_define_label m))
               0)))
      (foreach (inst body)
         (if (symbol? inst) ;; it is a label
           (_mark_label m (errhashget labels inst))
           (let ((ci (car inst)))
            (cond
             ((eqv? 'lift ci) nil)
             ((eqv? 'Skip ci) nil)
             ((eqv? 'raise ci)
              (_throw_exception m (cadr inst)))
             ((eqv? 'try-block ci) (_begin_exception_block m))
             ((eqv? 'catch-block ci) (_begin_catch_block m (cadr inst)))
             ((eqv? 'end-try-block ci) (_end_exception_block m))
             ((eqv? 'local ci) ;; local variable declaration
              (hashput locals (cadr inst)
                       (_declare_local m (thistype? cls (caddr inst)))))
             ((eqv? 'debugpoint ci)
              (format (cdr inst) (_ l1 . _)
                 (if (not (= l1 (car prevdbgline)))
                     (begin
                       (set-car! prevdbgline l1)
                       (il1 m 'Nop)
                       (_try_emit_sequencepoint m mdl (cdr inst))
                       ))))
             ((eqv? 'fixlocal ci)
              (let ((lc (hashget locals (cadr inst))))
                (if lc
                    (_setlocalsyminfo lc (symbol->string (caddr inst))))
                ))
             ((null? (cdr inst)) ;; simple instruction
              (il1 m (car inst)))
             ((eqv? 'Calli ci) ;; specific emit method
              (il2-calli m (cdr inst)))
             ((eqv? 'Switch ci) ;; specific emit method
              (il2-switch m labels (cadr inst)))
             ((eqv? 'label ci)
              (_mark_label m (errhashget labels (cadr inst))))
             ((list? (cadr inst)) ;; local or label referenced
              (il2 m (car inst)
                   (let* ((a (cadr inst))
                          (b
                           (case (car a)
                             ((var) (errhashget locals (cadr a)))
                             ((class)
                              (format (errhashget clshash (cadr a))
                                      (nm flas basety ifaces body lbody cls
                                          mtds itypes flds)
                                      cls))
                             ((label) (errhashget labels (cadr a)))
                             ((this) cls)
                             ((method)
                              (let ((v (emit:findmethod mtdhash path (cadr a))))
                                (if (null? v) (ccerror `(UNKNOWN-METHOD ,a)))
                                (if (not (null? (cddr a)))
                                    (let ((l (begin
                                               (if (null? v) (ccerror `(EMIT-MTD ,(cadr a))))
                                               (cadr v))))
                                      (if (not (= (caddr a) l))
                                          (ccerror `(EMIT:INCORRECT-NUMBER-OF-ARGUMENTS ,(cadr a))))))
                                (car v)))
                             ((field) (emit:findfield fldhash path (cadr a)))
                             ((othertype) (r_typer (cadr a)))
                             ((othermethod)
                              (format a (_ rt mnm argts)
                                 (read-int-eval `(r_mtd ,rt ,mnm ,@(foreach-map (a argts) a)))))
                             ((otherfield)
                              (format a (_ rt fnm)
                                 (read-int-eval `(r_getField (r_typer ,rt) ,fnm))))
                             ((otherctor)
                              (format a (_ rt argts)
                                 (read-int-eval `(r_constr ,rt ,@(foreach-map (a argts) a)))))
                             (else nil))))
                     (if (null? b) (ccerror `(EMIT-IN ,inst (,(_shash mtdhash) ,(_shash fldhash)))) b))))
             (else
              (il2 m ci (cadr inst))))))
      )))

 (recfunction pre-emit (l)
  (if (null? l) l
   (if (null? (car l)) (pre-emit (cdr l))
     (if (symbol? (car l)) l
      (if (not (symbol? (caar l))) (append (pre-emit (car l)) (pre-emit (cdr l)))
          (cons (car l) (pre-emit (cdr l))))))))


  (function emit-method (tpb nm attrs conv ret args)
     (let ((iattr (enumOr t_MethodAttributes attrs))
           (iconv (enumOr t_CallingConventions conv)))
        (_define_method tpb nm iattr iconv ret args)))

  (function emit-constructor (tpb mats ccals patyps)
    (let ((iattr (enumOr t_MethodAttributes mats))
          (iconv (enumOr t_CallingConventions ccals)))
      (_define_constructor tpb iattr iconv patyps)))


  (function make-assembly (name)
     (let ((cd (sg-> t_AppDomain "CurrentDomain"))
           (an (new t_AssemblyName)))
         (s<-: an "Name" name)
         (_define_dynamic_assembly cd an (getEnum t_AssemblyBuilderAccess "RunAndSave"))))

 (function clr:make.assembly (name)
    "Makes a new assembly builder with a given name."
    (make-assembly name))

 (define t_FileStream (dotnet "System.IO.FileStream"))

 (function make-strong-assembly (name version key)
     (let ((cd (sg-> t_AppDomain "CurrentDomain"))
           (keyf ((r_tsbind "System.IO.File" "OpenRead" string) key))
           (an (new t_AssemblyName)))
         (s<-: an "Name" name)
         (s<-: an "Version" (new (dotnet "System.Version") (string version)))
         (s<-: an "KeyPair" (new (dotnet "System.Reflection.StrongNameKeyPair") (t_FileStream keyf)))
         (_define_dynamic_assembly cd an (getEnum t_AssemblyBuilderAccess "RunAndSave"))))

 (function clr:make.strong.assembly (name version key)
    ("Makes an assembly builder using the given strong name (built of a name,"
     " an explicit version and a public key fingerprint).")
    (make-strong-assembly name version key))

  (function make-module (asm name dbg?)
     (_define_dynamic_module asm name dbg?))
  (function make-module-s (asm name fname dbg?)
     (_define_dynamic_module_s asm name fname dbg?))

  (function make-class (mod name flas basetyp ifaces)
    (_define_type mod name (enumOr t_TypeAttributes flas)
            basetyp
            (if (null? ifaces)
                (anew t_Type 0)
                (mkvector ifaces))
    ))

  (function add-class (parent name flas basetyp ifaces)
    (tpb_addclass parent name (enumOr t_TypeAttributes flas)
                  basetyp
                  (if (null? ifaces)
                      (anew t_Type 0)
                      (mkvector ifaces))))

  (function add-field (class name type acc)
     (let ((iacc (enumOr t_FieldAttributes acc)))
      (_define_field class name (thistype? class type) iacc)
    ))

  (function add-init-field (class name data acc)
    (let ((iacc (enumOr t_FieldAttributes acc)))
      (_define_init_field class name
                          (mkvector data) ;; data must be a list of bytes!
                          iacc)))

  (function make-cattr (ca)
    (format ca (ctor . args)
      (new t_CustomAttributeBuilder
           (t_ConstructorInfo ctor)
           ("System.Object[]" args))))

  (function _set_custom_attribute (v catt)
    (let* ((t (r_GetType v))
           (fn (r_getmethod01
                t "SetCustomAttribute" (vector t_CustomAttributeBuilder))))
      (r_invoke fn v (ovector catt))))

  (function flush-attrs (cuattrs v)
    (let ((c (car cuattrs)))
      (set-car! cuattrs nil)
      (foreach (ca c)
        (let ((catt (make-cattr ca)))
          (_set_custom_attribute v catt)))
      v
      ))

(recfunction clr:emit:pass1:i (mod e parent itypes)
  (let* ((nm (if (string? (cadr e)) (cadr e) (car (cadr e))))
         (flas (if (string? (cadr e)) (if parent '(NestedPublic) '(Public)) (cdr (cadr e))))
         (basety (let ((ex (filter (fmt (x) (eqv? x 'extends)) (cddr e))))
                   (if (null? ex) (r_typerx "System.Object") (r_typerx (cadr (car ex))))))
         (ifaces (map (@ r_typerx cadr)
                   (filter (fmt (x) (eqv? x 'implements)) (cddr e))))
         (body (cddr e))
         (lbody (mkref))
         (cls (if (null? parent)
                  (make-class mod nm flas basety ifaces)
                  (add-class (caddr parent) nm flas basety ifaces)))
         (mtds (mkhash))
         (flds (mkhash))
         (env (list nm flas basety ifaces body
                    lbody cls mtds itypes flds)))
    (let pass1loop ((bo body))
      (let ((cuattrs (mkref)))
      (foreach (x bo)
        (try
         (fccase x
           ((attr) acode
            (set-car! cuattrs (cons acode (car cuattrs))))
           ((field) (name type opts)
            (begin
              (hashput flds name
                       (flush-attrs cuattrs
                          (add-field cls name type opts)))
              (if parent
                  (hashput (cadr parent) (S<< nm "/" name)
                           (errhashget flds name)))
              ))
           ((initfield) (name opts . data)
            (begin
              (hashput flds name
                       (flush-attrs cuattrs
                          (add-init-field cls name data opts)))
              (if parent
                  (hashput (cadr parent) (S<< nm "/" name)
                           (errhashget flds name)))
              ))
           ((pinvoke-method)
                     (dll name rtype artyps)
            (hashput mtds name
                     (flush-attrs cuattrs
                      (_pinvoke_maker cls dll name rtype artyps))))
           ((method) ((name0 atype ctype rtype . artyps) . body)
            (let* ((name (if (list? name0) (cadr name0) name0))
                   (mname (if (list? name0) (car name0) name0))
                   (args (if (or (null? artyps) (null? (car artyps)))
                             (anew t_type 0)
                             (mkvector (map (fun (z) (thistype? cls z))
                                         (car artyps)))))
                   (overr (filter
                           (fun (x) (not (null? x)))
                           (foreach-map (t (cons basety ifaces))
                             (tp_getmethod t  mname args))))
                   (mtdi (emit-method cls mname (if (null? overr) atype
                                                    (cons 'Virtual atype))
                                      ctype
                                      (thistype? cls rtype)
                                      args))
                   )
              (if (not (null? overr))
                  (tpb_override cls mtdi (car overr)))
              (flush-attrs cuattrs mtdi)
              (hashput mtds name
                       (list
                        mtdi
                        (if (null? artyps) 0 (length (car artyps)))
                        ))
              (if parent
                  (hashput (car parent) (S<< nm "/" name)
                           (errhashget mtds name)))
              (foreach (inst body)
                (if (and (list? inst)
                         (eqv? (car inst) 'lift))
                    (begin
                      (set-cdr! lbody (cons (cdr inst) (cdr lbody)))
                      (pass1loop (list (cdr inst))))))
              ))
           ((constructor) ((nickname atype ctype . artyps) . body)
            (begin
              (hashput mtds nickname
                       (list
                        (flush-attrs cuattrs
                          (emit-constructor cls atype ctype
                                          (if (or (null? artyps) (null? (car artyps)))
                                              (anew t_type 0)
                                              (mkvector (map (fun (z) (thistype? cls z))
                                                          (car artyps))))))
                        (if (null? artyps) 0 (length (car artyps)))))
              (if parent
                  (hashput (car parent) (S<< nm "/" nickname)
                           (errhashget mtds nickname)))
              (foreach (inst body)
                (if (and (list? inst)
                         (eqv? (car inst) 'lift))
                    (begin
                      (set-cdr! lbody (cons (cdr inst) (cdr lbody)))
                      (pass1loop (list (cdr inst))))))
              ))
           (else nil))
         t_Exception
         (fun (ex)
           (ccerror
            `(EMIT_ERR_P1 ,x ,(g-> ex "Message")
                          ,@(if (instanceof ex t_MBaseException)
                                (list 'MB: (mbaseerror ex))
                                (list (->s ex)))
                          ))
           )
         ))))
    (let pass11loop ((bo body))
      (let ((cuattrs (mkref)))
      (foreach (x bo)
        (try
         (fccase x
           ((attr) acode
            (set-car! cuattrs (cons acode (car cuattrs))))
           ((class) ((newnm . _) . _)
            (try
             (let ((iclass (clr:emit:pass1:i mod x (list mtds flds cls) itypes)))
               (flush-attrs cuattrs iclass)
               (hashput itypes newnm iclass)
               )
             t_MBaseException
             (fun (e)
               (ccerror `(INNER IN1 ,newnm ,(mbaseerror e))))
             ))
           (else nil))
         t_Exception
         (fun (ex)
           (ccerror
            `(EMIT_ERR_P1C ,x ,(g-> ex "Message")))
           )
         ))))
    env
    ))

(function clr:emit:pass1 (mod e itypes)
  (clr:emit:pass1:i mod e nil itypes))

(recfunction clr:emit:pass2 (pre mod env e path)
  (format env (nm flas basety ifaces body lbody cls mtds itypes flds)
    (foreach (x (append (cdr lbody) body))
       (fccase x
         ((class) ((newnm . _) . _)
          (try
           (try
            (let* ((newenv (errhashget itypes newnm)))
              (clr:emit:pass2 pre mod newenv x (cons (list mtds flds)
                                                     path)))
            t_MBaseException
            (fun (e)
              (ccerror `(INNER IN2 ,newnm ,(mbaseerror e))))
            )
           t_Exception
           (fun (ex)
                (ccerror
                 `(EMIT_ERR_P2 ,x ,(g-> ex "Message")
                               ,@(if (instanceof ex t_MBaseException)
                                     (list (mbaseerror ex)) nil)
                               ))
                )
           ))
         ((method) ((name0 _ _ _ . artyps) . body0)
          (if body0
          (let* ((name (if (list? name0) (cadr name0) name0))
                 (mtd (car (errhashget mtds name)))
                 (body (pre cls (cons mtd (if (null? artyps) nil
                                              (car artyps))) body0)))
            (try
             (emit-instructions (_get_ilgenerator mtd)
                                mtds flds itypes body cls path mod)
             t_Exception
             (fun (x)
               (if (instanceof x t_MBaseException)
                   (ccerror `(EMIT_ERR_MTD_I ,name0 ,(mbaseerror x) ,body))
                   (ccerror
                    `(EMIT_ERR_MTD ,name0 ,(g-> x "Message") ,body))))
             ))))
         ((constructor) ((nickname atype ctype . artyps) . body0)
          (if body0
          (let* ((mtd (car (errhashget mtds nickname)))
                 (body (pre cls (cons mtd (if (null? artyps) nil
                                              (car artyps))) body0)))
            (try
             (emit-instructions (_get_ilgeneratorc mtd)
                                mtds flds itypes body cls path mod)
             t_Exception
             (fun (x)
               (if (instanceof x t_MBaseException)
                   (ccerror `(EMIT_ERR_CTR_I ,artyps ,(mbaseerror x) ,body))
                   (ccerror
                    `(EMIT_ERR_CTR ,artyps ,(g-> x "Message") ,body))))
             )
            )))
         (else nil)))
    (create-type cls)))

(function -bind-em-pre (pre mod e)
  (let* ((itypes (mkhash))
         (env (clr:emit:pass1 mod e itypes)))
    (clr:emit:pass2 pre mod env e nil)))

(function -bind-em (mod e)
  (-bind-em-pre (fun (c m x) x) mod e))

  (function clr:emit.class (mod e) ;; "exported" function
     ("Emits a class definition [e] into a given dynamic module [mod]."
      "The class format is following:"
      "[["
      "<class>:"
      "  (class <name-string> [(extends <type>)] "
      "    [(implements <type>*)]"
      "    <elt>*)"
      "<elt>:"
      "  (field <name> <type> <attrs>)"
      "  (initfield <name> <attrs> . <bytedata>)"
      "  <class>"
      "  (method (<name> <attrs> <ret-type> (<arg-type>*))"
      "     <instr>*)"
      ""
      "<instr>:"
      "   (local <name> <type>) "
      "   (label <name>)"
      "   (lift . <elt>)"
      "   (<InstrName> [<arg>])"
      "<arg>:"
      "   (var <name>)"
      "   (label <name>)"
      "   (method <name>) - for another method of this class"
      "   <anything-else>"
      "]]"
      ""
      "[<InstrName>]'s are the same as fields of [System.Reflection.Emit.OpCodes] class."
      )
     (-bind-em mod e))

  (function _ldc_i4 (n)
    "A shortcut for creating a proper [ldc_i4] instruction."
    (cond
      ((and (>= n 0) (<= n 8))
       `(,(string->symbol (string-append "Ldc_I4_" (number->string n)))))
      ((= n (- 0 1))
       '(Ldc_I4_M1))
      (else `(Ldc_I4 ,n))))

  (function _ldarg (n)
    "A shortcut for creating a proper [ldarg] instruction"
    (cond
      ((and (>= n 0) (<= n 3))
       `(,(string->symbol (string-append "Ldarg_" (number->string n)))))
      (else `(Ldarg ,n))))

) ; using

