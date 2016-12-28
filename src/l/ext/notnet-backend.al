;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;

(function ll-type-class-int (tp)
  (p:match tp
    ((this) 'Ref)
    (else (il-type-class-int (ll-nettype tp)))))

(function ll-type-class-int-elem (tp)
  (p:match tp
    ((this) 'Ref)
    (else
     (let* ((tt (ll-nettype tp))
            (res
             (il-type-class-int tt)))
       (if (IsValueType tt)
           `(a ,tt)
           (Sm<< "_" res)
           )))))

(define gettypefromhandle (r_mtd "System.Type" "GetTypeFromHandle"
                                 (dotnet "System.RuntimeTypeHandle")))

(define typeeq? (r_mtd "System.Type" "Equals"
                       (dotnet "System.Type")))

(define getobjtype (r_mtd "System.Object" "GetType"))

(define ptrtostructure
  (r_mtd "System.Runtime.InteropServices.Marshal"
         "PtrToStructure"
         (dotnet "System.IntPtr")
         (dotnet "System.Type")))

(function lltnet-emit-enum (tp val)
  (let* ((tt (ll-nettype tp))
         (ut ((r_tsbind "System.Enum" "GetUnderlyingType" "System.Type") tt))
         (utc (il-type-class-int ut))
         (vnum (+ 0 (getEnum tt (any->string val)))))
    `((,(Sm<< 'Ldc_ utc) ,vnum))))

(function lltnet-ctorcall (t args)
  (let* ((tx (ll-nettype t))
         (ii (il-type-constructors tx
                                   (map ll-nettypeT args))))
    (if (not ii)
        (ccerror `(CTOR-LOOKUP ,t ,@args)))
    (if nil ; (IsValueType tx)
        `(Call ,(car ii))
        `(Newobj ,(car ii)))))

(function lltnet-thiscall (ret args name)
  (p:match name
    ((CTOR $cname)
     `(Newobj (method ,cname)))
    (else `(Call (method ,name)))))

; lltnet-methodcall (mtd)
(function lltnet-methodcall (tail? mtdi)
  (format mtdi ((t ret . args) name)
   (alet clcode
    (p:match t
      ((this)
       (lltnet-thiscall ret args name))
      (else
       (if (eqv? name nm_ctor)
           (lltnet-ctorcall t args)
           (let* ((ii (il-types-havemethod
                       (list (ll-nettype t))
                       name
                       (ll-nettype ret)
                       (map ll-nettype args))))
             (if (not ii)
                 (ccerror `(METHOD-LOOKUP ,mtdi)))
             (format ii ((_ minfo))
                     (cond
                      ((and (IsVirtual minfo)
                            (not (IsValueType (ll-nettype t))))
                       `(Callvirt ,minfo))
                      (else
                       `(Call ,minfo))))))))
    (if tail?
        `((Tailcall) ,clcode)
        `(,clcode))
    )))

(function lltnet-methodref (mtdi)
  (format mtdi ((t ret . args) name)
    (p:match t
      ((this)
       `(method ,name))
      (else
       (let* ((ii (il-types-havemethod
                   (list (ll-nettype t))
                   name
                   (ll-nettype ret)
                   (map ll-nettype args))))
         (if (not ii)
             (ccerror `(METHOD-LOOKUP ,mtdi)))
         (format ii ((_ minfo))
                 minfo))))))

(function tppboxed? (p)
  (p:match p
    ((boxed $t) #t)
    (else nil)))

; lltnet-ldfield (fld)
(function lltnet-ldstfield (st? flt tpp)
  (format flt ((t vt) name)
    (p:match t
      ((this)
       (if st?
           (if (car name)
               `((Stsfld (field ,(cadr name))))
               `((Stfld (field ,(cadr name)))))
           (if (car name)
               `((Ldsfld (field ,(cadr name))))
               `((Ldfld (field ,(cadr name)))))
           ))
      (else
       (let* ((ii (il-types-havefield
                   (list (ll-nettype t))
                   (symbol->string name)
                   (ll-nettype vt)
                   )))
         (if (not ii)
             (ccerror `(FIELD-LOOKUP ,flt)))
         (list
          (format ii ((_ fldinfo))
           (if (tppboxed? tpp)
               (if st? `(Stfld ,fldinfo)
                   `(Ldflda ,fldinfo))
               (if (_isfldstatic fldinfo)
                   `(,(if st? 'Stsfld 'Ldsfld) ,fldinfo)
                   `(,(if st? 'Stfld 'Ldfld) ,fldinfo))))))))))

(function lltnet-ldfield (tpp flt)
  (lltnet-ldstfield nil flt tpp))

(function lltnet-stfield (tpp flt)
  (lltnet-ldstfield #t flt tpp))

; lltnet-ldelem (type)
(function lltnet-ldelem (tpp type)
  (p:match type
    ((array $of)
     (let* ((ct (ll-type-class-int of)))
       (if (tppboxed? tpp)
           `(Ldelema ,(ll-nettype of))
           `(,(Sm<< 'Ldelem_ ct)))))
    (else (ccerror `(NOT-AN-ARRAY ,type)))))

; lltnet-cast (from to)
(function lltnet-cast (f t)
  (let* ((tf (ll-nettypeT f))
         (tt (ll-nettypeT t))
         (cf (ll-type-class-int f))
         (ct (ll-type-class-int t)))
    (p:match (list cf ct)
      ((Ref Ref)
       `((Castclass ,tt)))
      ((Obj Ref)
       `((Box ,tf)
         (Castclass ,tt)
         ))
      ((Ref Obj)
       `((Unbox ,tt)
         (Ldobj ,tt)))
      ((Obj Obj)
       `())
      ((Ref $int)
       `((Castclass ,tt)
         (Unbox ,tt)
         (,(Sm<< 'Ldind_ int))))
      (($frm Ref)
       `((Box ,tf)))
      (($frm $tto)
       `((,(Sm<< 'Conv_ tto)))))))

;

(function lltnet-laddr (e)
  (p:match (car e)
    ((this)
     (cdr e)
     )
    (else
     (let ((nt (ll-nettype (car e))))
       (if (isBoxed nt)
           `(,@(cdr e)
             (Box ,(ll-nettype (car e))))
           (cdr e)
           )))))

; Last finalising pass before emitting the code - propagate tailness from tail-return to its inner call
(function lltnet-detail ( bdy )
  (lltnett:visit mtdbody bdy
    (mtdbody DEEP
       ((tailreturn
          `(return
            ,(lltnett:visit expression e
               (iexpression _
                 ((am (ast:mknode (tailp #t)))
                  (asm (ast:mknode (tailp #t)))
                  (avm (ast:mknode (tailp #t)))
                  (csm (ast:mknode (tailp #t)))
                  (else node))))))
        (else node)))))

;
(function lltnet-emitbody ( bdy )
  (when (shashget (getfuncenv) 'debug-compiler-lltnet)
        (println 'LLTNET:--------------------)
        (iter writeline bdy)
        (println '-------------------LLTNET---))
  (<> (lltnet-detail bdy)
   (ast:visit lltnett mtdbody
     (mtdbody DEEP
      ((tvar
        `((local ,name ,(ll-nettypeT tp))
          ,@(cdr value)
          (Stloc (var ,name))
          ,@(foreach-mappend (i in) i)))
       (tvardef
        `((local ,name ,(ll-nettypeT tp))
          ,@(foreach-mappend (i in) i)))
       (vvar
        `((local ,name ,(ll-nettypeT (caar mtd)))
          (Ldloca (var ,name))
          ,@(if (null? args)
                `((Initobj ,(ll-nettypeT (caar mtd))))
                `(
                  ,@(foreach-mappend (x args) (cdr x))
                  (Call ,(car (il-type-constructors
                               (ll-nettypeT (caar mtd))
                               (map (M@ ll-nettypeT car) args))))
                  ))
          ,@(foreach-mappend (i in) i)
          ))
       (begin (foreach-mappend (x b) x))
       (debugpoint `((debugpoint ,@d)))
       (return
        `(,@(cdr e)
          (Ret)))
       (leave ;; DANGEROUS!!!
        `(,@(cdr e)
          ,@(p:match (car e)
              ((void) `((Ldnull)))
              (else (lltnet-cast (car e) '(object))))))
       (vreturn
        '((Ret)))

       (nop
        '())

       (if
        (let ((l1 (gensymp "lbl"))
              (l2 (gensymp "lbl")))
          `(,@(cdr value)
            (Brfalse (label ,l2))
            ,@iftr
            ,@(if iffls
                 `((Br (label ,l1))
                   (label ,l2)
                   ,@(car iffls)
                   (label ,l1))
                 `((label ,l2))))))
       (tryblock
        `((local ,nm ,(ll-nettype mask))
          (try-block)
          ,@body
          (catch-block ,(ll-nettype mask))
          (Stloc (var ,nm))
          ,@bcatch
          (end-try-block)
          ))

       (embedd-asm code)

       (throw
        `(,@(cdr exc)
          (Throw)))

       (label
        `((label ,nm)))
       (goto
        `((Br (label ,nm))))
       (gotoif
        `(,@(cdr value)
          (Brtrue (label ,nm))))
       (gotoiff
        `(,@(cdr value)
          (Brfalse (label ,nm))))
       (e
        `(,@(cdr e)
          ,@(p:match (car e)
              ((void) nil)
              (else `((Pop))))))
       (subst (cdr e))
       (xset
        (p:match (lvalue.> v)
          ((vl $nam)
           `(,@(cdr e)
             (Stloc (var ,nam))))
          ((vf $nm $fi)
           `((Ldloca (var ,nm))
             ,@(cdr e)
             ,@(lltnet-stfield (caar stack) fi)))
          ((vaf $tp $e1 $e2 $fi)
           `(,@(cdr e1)
             ,@(cdr e2)
             (Ldelema ,(ll-nettypeT tp))
             ,@(cdr e)
             ,@(lltnet-stfield tp fi)))
          ((f $ex $fi)
           `(,@(cdr ex)
             ,@(cdr e)
             ,@(lltnet-stfield (caar stack) fi)))
          ((sf $fi)
           (let ((res (lltnet-stfield (caar stack) fi)))
             (append (cdr e) res)))
          ((ar $ex $idx)
            `(,@(cdr ex)
              ,@(cdr idx)
              ,@(cdr e)
              ,(let ((x (ll-type-class-int (car e))))
                 `(,(Sm<< 'Stelem_  x)))))
            ))
       (else
        (ccerror `(WRONG-EMIT-TIME-NODE ,node)))
       ))
     (vvalue DEEP
      ((vl
        `((Ldloca (var ,nm))))
       (ar
        `(,@(cdr e)
          ,@(cdr idx)
          (Ldelema ,(ll-nettypeT (caar stack)))))
       (lbox
        (let ((x (gensym)))
          `((local ,x ,(ll-nettypeT (car e)))
            ,@(cdr e)
            (Stloc (var ,x))
            (Ldloca (var ,x)))))))
     (iexpression DEEP
      ((am
        `(
          ,@(lltnet-laddr e)
          ,@(foreach-mappend (i args) (cdr i))
          ,@(lltnet-methodcall tailp mtd)))
       (avm
        `(
          ,@(cdr e)
          ,@(foreach-mappend (i args) (cdr i))
          ,@(lltnet-methodcall tailp mtd)))
       (asm
        `(,@(foreach-mappend (i args) (cdr i))
          ,@(lltnet-methodcall tailp mtd)))
       (newarr
        (ccerror `(NEWARR: must not be here: ,node)))
       (newtarr
        (let* ((len (length args))
               (tt (ll-nettypeT tp))
               (tc (ll-type-class-int tp))
               (stelem
                (Sm<< 'Stelem_ tc)))
          `(,(_ldc_i4 len)
            (Newarr ,tt)
            ,@(foreach-mappend-count (a args ix)
                      `((Dup)
                        ,(_ldc_i4 ix)
                        ,@(cdr a)
                        (,stelem))))))
       (allocarr
        `(,@(cdr size)
          (Newarr ,(ll-nettypeT tp))))

       (funref
        `((Ldftn ,(lltnet-methodref mtd))))

       (vl
        `((Ldloc (var ,nm))))
       (va
        `(,(_ldarg nm)))

       (ref
        `((Ldloca (var ,nm))))

       (f
        `(,@(cdr e)
          ,@(lltnet-ldfield (caar stack) fld)))
       (sf
        (lltnet-ldfield (caar stack) fld))
       (ar
        `(,@(cdr e)
          ,@(cdr idx)
          ,(lltnet-ldelem (caar stack) (car e))))
       (ic
        `(,(_ldc_i4 v)))
       (fc
        `((Ldc_R4 ,(flt1:parse v))))
       (dfc
        `((Ldc_R8 ,(flt:parse v))))
       (i64c
        `((Ldc_I8 ,((r_tsbind "System.Int64" "Parse" string) v))))
       (sc
        `((Ldstr ,v)))
       (enumc
        (lltnet-emit-enum tp val))
       (null
        `((Ldnull)))
       (true
        `(,(_ldc_i4 1)))
       (false
        `(,(_ldc_i4 0)))
       (bin
        `(,@(cdr l)
          ,@(cdr r)
          ,@(case op
             ((+) '((Add)))
             ((-) '((Sub)))
             ((*) '((Mul)))
             ((/) '((Div)))
             ((mod) '((Rem)))
             ((>) '((Cgt)))
             ((>=) '((Clt) (Ldc_I4_0) (Ceq)))
             ((<) '((Clt)))
             ((<=) '((Cgt) (Ldc_I4_0) (Ceq)))
             ((==) '((Ceq)))
             ((!=) '((Ceq) (Ldc_I4_0) (Ceq)))
             ((or) '((Or)))
             ((and) '((And)))
             ((xor) '((Xor)))
             ((binand) '((And)))
             ((binor) '((Or)))
             ((lshift) '((Shl)))
             ((rshift) '((Shr)))
             (else (ccerror `(BINOP: ,op)))
             )))

       (un
        (case op
          ((-) `(,@(cdr v)
                 (Neg)))
          ((not) `(,@(cdr v)
                   (,(Sm<< 'Ldc_ (il-type-class-int (ll-nettype (car v))))
                      0)
                   (Ceq)
                   ))
          ((isnull)
           (let ((t1 (gensymp "lblisnull"))
                 (t2 (gensymp "lblisnull")))
             `(,@(cdr v)
               (Brfalse (label ,t1))
               (Ldc_I4_0)
               (Br (label ,t2))
               (label ,t1)
               (Ldc_I4_1)
               (label ,t2))))
          ((bitnot) `(,@(cdr v)
                      (Not)))
          (else (ccerror `(UNOP: ,op)))))

       (typecast
        `(,@(cdr e)
          ,@(lltnet-cast (car e) tp)))

       (typetoken
        `((Ldtoken ,(ll-nettypeT tp))
          (Call ,gettypefromhandle)
          ))

       (fieldtoken
        (let* ((fldi
                (p:match (cadr e)
                 ((Ldsfld $fi) fi)
                 ((Ldfld $fi) fi)
                 (else (ccerror `(FTOKEN ,e)))
                 )))
          `((Ldtoken ,fldi))))

       (typeof
        `(,@(cdr e)
          (Call ,getobjtype)))

       (istype
        `(,@(cdr e)
          (Isinst ,(ll-nettype tp))
          ))

       (marshal
        (let* ((tt (ll-nettypeT tp)))
          `(,@(cdr e)
            (Ldtoken ,tt)
            (Call ,gettypefromhandle)
            (Call ,ptrtostructure)
            (Unbox ,tt)
            (Ldobj ,tt))))
       (embedded-begin-1 bdy)
       (ldthis
        `(,(_ldarg 0)))

       (else (ccerror `(STINKS ,node)))
       )))))

(function accs (acc)
  (map-over acc
    (fmt (a)
         (case a
           ((public) 'Public)
           ((static) 'Static)
           ((private) 'Private)
           ((protected) 'Protected)
           ((virtual) 'Virtual)
           (else a)
           ; (else (ccerror `(T: ,a)))
           ))))

(define lltnet-module nil)
(define lltnet-module-name nil)

(function not.ctor-name (accs)
  (let loop ((a accs))
    (p:match a
      (((Static) . $_) nm_cctor)
      (((static) . $_) nm_cctor)
      (() nm_ctor)
      (($hd . $tl) (loop tl)))))

(function lltnet-emit0 (src atts0)
  (let ((basector (noconst (cons nil nil)))
        (atts (if atts0 atts0 '(Public)))
        )
    (<> src
      (ast:visit lltnett class
        (class _
          `(:classwrap ,name ,atts
              (extends ,(let* ((tx (ll-nettypeT `(T ,@parent)))
                               (bc (il-type-constructors
                                                   tx nil)))
                          (if (not (null? bc))
                              (set-cdr! basector (car bc)))
                          tx))

              ,@(let ((notmodulenm (read-int-eval `(car (list lltnet-module-name)))))
                  (if notmodulenm
                      `((target-module ,notmodulenm)) nil))

              ,@(if (null? interfaces) nil
                    `((implements ,@(map (M@ ll-nettypeT
                                     (fun (x) `(T ,@x)))
                                 interfaces))))
              ,@(foreach-map (m members)
                  (<> m
                    (ast:visit lltnett classmember
                       (classmember DEEP
                         ((method
                           (if (list? name)
                               `(xconstr ,(S<< (cadr name)) ,(accs acc)
                                         (Standard)
                                         ,(map (@ ll-nettypeT car) args)
                                         ;TODO: add field initialisers here
                                         ,@(if (eqv? nm_ctor (not.ctor-name acc))
                                               `(
                                                 ,(_ldarg 0)
                                                 (Call ,(cdr basector))
                                                 ) nil)
                                         ,@body
                                         (Ret)
                                         )
                               `(xmethod (,(S<< name) ,(accs acc)
                                          (Standard)
                                          ,(ll-nettypeT ret)
                                          ,(map (@ ll-nettypeT car) args))
;                                         ,@(begin
;                                             (println '------------)
;                                             (writeline `(method ,name))
;                                             (iter writeline body)
;                                             (println '------------)
;                                             nil)
                                         ,@body
                                         )))
                          (field
                           `(field ,(S<< name) ,(ll-nettypeT tp)
                                   ,(accs acc)
                                   )
                           )
                          (initfield
                           `(initfield ,(S<< name)
                                       ,(accs acc)
                                       ,@mess))
                          (custom thing)
                          (else nil)
                          ))
                       (mtdbody _ (forall
                                   (lltnet-emitbody node))))))
              ))))))

(function lltnet-emit-single (m)
  (<> m
      (ast:visit lltnett classmember
         (classmember DEEP
            ((method
              (if (list? name)
                  (ccerror `(CONSTRUCTOR-LIFTED?!?))
                  `(method (,(mtdnames (S<< name)) ,(accs acc)
                            (Standard)
                            ,(ll-nettypeT ret)
                            ,(map (@ ll-nettypeT car) args))
                           ,@body
                           )))
             (field
              `(field ,(S<< name) ,(ll-nettypeT tp)
                      ,(accs acc)
                      )
              )
             (initfield
              `(initfield ,(S<< name)
                          ,(accs acc)
                          ,@mess))
             (else nil)
             ))
         (mtdbody _ (forall
                     (lltnet-emitbody node))))))

(function lltnet-emit (src atts)
  (lltnet-emit0 src atts))

(function notnet-new-module (nm tp)
  (let* ((s (S<< nm))
         (asm (make-assembly s))
         (mdl (make-module-s asm s (S<< s "." tp) #f))
         )
    mdl))

(macro lltnet-target (nm tp)
  (let* ((modul (notnet-new-module nm tp))
         (amodul (_aasm modul)))
  `(top-begin
     (ctimex (begin
               (define lltnet-module-name 'lltnet-module)
               (define lltnet-module-file ,(S<< nm "." tp))
               (define lltnet-module (quote ,modul))
               (add-assembly (quote ,amodul)))))))

(macro not.module (nm tp)
  `(lltnet-target ,nm ,tp))


(macro lltnet-default-target ()
  `(ctimex (begin
             (define lltnet-module-name nil)
             (define lltnet-module nil))))

(function lltnet-current-module ()
  (let ((tst (read-int-eval '(car (list lltnet-module-name)))))
    (if tst (read-int-eval `(car (list ,tst)))
        (! _global_module))))

(macro lltnet-savemodule ()
  (let ((tst (read-int-eval '(car (list lltnet-module-name)))))
    (if tst
        (let* ((modl (lltnet-current-module))
               (asm (_aasm modl)))
          (asm-save asm (read-int-eval '(car (list lltnet-module-file))))
          `(Nop))
        `(Nop))))


(macro not.savemodule ()
  `(lltnet-savemodule))



(function lltnet-embed ( args body )
  (try
   (let ((res
          (lltnet-emitbody (lltnet-prepare-embedded args body))))
     res)
   t_MBaseException
   (fun (x)
     (println "LLtNET error:")
     (writeline (mbaseerror x))
     (println "-----------")
     (println x)
     (r_raise x)
     )))

(function lltnet-embed-lifts ( args lifts body )
  (try
   (let* ((prep (lltnet-prepare-embedded-with-top  args body lifts))
          (bodes (foreach-map (p (car prep))
                              (lltnet-emit-single p)))
          (res
           (lltnet-emitbody (cdr prep))))
     (append
      (foreach-map (b bodes)
       `(lift ,@b))
       res))
   t_MBaseException
   (fun (x)
     (println "LLtNET error:")
     (writeline (mbaseerror x))
     (println "-----------")
     (println x)
     (r_raise x)
     )))

(define not.Nil nil)

(function f.not.net (args body )
  (let* ((has (mkhash))
         (a2s (map-over args
                 (fmt (tp nm)
                      `(,nm ,(gensym) ,tp)))))

  `(n.asm ,(foreach-map (a a2s) (car a))
          ,@(foreach-mappend (a a2s)
                 (format a (nm nn tp)
                   (let ((tt (ll-nettype tp)))
                     `((local ,nn ,tt)
                       (expr ,nm)
                       ,@(if (isBoxed tt)
                             `((Unbox ,tt)
                               ,(_ldind tt))
                             `((Castclass ,tt)))
                       (Stloc (var ,nn))))))
          ,@(lltnet-embed a2s body)
          )))

(macro not.net (args body)
  ("Compiles and substitutes a Not.Net AST body")
  (f.not.net args body))

(function f.not.net.lift (pure? xtp blk? args lifts body)
  (let* ((has (mkhash))
         (a2s (map-over args
                 (fmt (tp nm)
                      `(,nm ,(gensym) ,tp))))
         (cbody
          `(,@(foreach-mappend (a a2s)
                 (format a (nm nn tp)
                   (let ((tt (ll-nettype tp)))
                     `((local ,nn ,tt)
                       (expr ,nm)
                       ,@(if (isBoxed tt)
                             `((Unbox ,tt)
                               ,(_ldind tt))
                             `((Castclass ,tt)))
                       (Stloc (var ,nn))))))
            ,@(lltnet-embed-lifts a2s lifts body))))
    (if pure? cbody
        `(,@(if xtp `(let ((not.Nil nil))) '(begin))
           (,(if blk? 'n.asm.block 'n.asm)  ,(map car a2s)
                  ,@cbody
                  )
           ,@(if xtp nil `(not.Nil))
           ))))

(macro not.net.lift (xtp blk? args lifts body)
  ("Compiles and substitutes a Not.Net AST body")
  (f.not.net.lift  nil xtp blk? args lifts body))

