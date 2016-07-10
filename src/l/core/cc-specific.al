;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cmacro late-ctime rest
   `(inner.late-ctime (quote ,rest)))

(cmacro late-ctimex rest
   `(inner.late-ctimex.nf (quote ,rest))
)

(cmacro let rest
  (if (list? (car rest))
     `(inner.let ,(car rest) (begin ,@(cdr rest)))
     (format rest (nm args . body)
       `((inner.reclambda ,nm ,(map car args) (begin ,@body))
          ,@(map cadr args)))))

(cmacro let* (args . body)
   (if (null? args)
     `(begin ,@body)
      (if (null? (cdr args))
         `(inner.let (,(car args)) (begin ,@body))
         `(inner.let (,(car args)) (let* ,(cdr args) (begin ,@body))))))

(cmacro if rest
   `(inner.if ,@rest))

(cmacro defrec rest
   `(inner.defrec ,@rest))

(cmacro lambda rest
   `(inner.lambda ,@rest))

(cmacro begin rest
   `(inner.begin ,@rest))

(cmacro topblock rest
   `(inner.topblock ,@rest))

(cmacro defmacro (cnnm . rest)
   `(inner.defmacro ,cnnm ,@rest))

(cmacro try rest
  (with-syms (tr)
   `(alet ,tr (inner.try ,@rest)
          ,tr)))

(cmacro n.asm (args . body)
  `(inner.asm normal ,args ,@body))

(cmacro n.asm.block (args . body)
  `(inner.asm block ,args ,@body))

(cmacro n.label (lbl)
  `(inner.label ,lbl))

(cmacro n.null ()
  nil)

(cmacro n.goto (lbl)
  `(inner.goto ,lbl))

(expand-if (shashget (getfuncenv) 'compiler-final)

  (cmacro not (a)
    `(inner.not ,a))

  (cmacro eqv? (a b)
     `(inner.eqv? ,a ,b))

  (cmacro null? (a)
         `(inner.null? ,a))

  (cmacro list? (a)
         `(inner.pair? ,a))

  (cmacro pair? (a)
         `(inner.pair? ,a))

  (cmacro usdotnet (pth t)
    (let* ((tp (udotnet pth t)))
      `(n.asm ()
              (Ldtoken ,tp)
              (Call ,m_typefromhandle)
              (Castclass ,t_object))))

)

(cmacro n.stloc! (lc val)
        `(inner.setloc ,lc ,val))

(macro ctime (expr)
  (read-int-eval expr))

(macro ctimeq (expr)
  (list 'quote (read-int-eval expr)))

(macro ctimex (expr)
  (begin (read-int-eval expr) `(begin)))

(macro letrec (defns . body)
  `(inner.letrec ,defns (begin ,@body)))

(cmacro core.recfunction (name args . body)
  `(inner.defrec ,name (fun ,args ,@body)))

(cmacro straise (code)
  (with-syms (sn chk mk)
    `(begin
     (n.asm ()
       (lift field ,(S<< sn) ,t_object (Private Static))
       (Ldnull))
     (let ((,chk (n.asm () (Ldsfld (field ,sn)))))
       (if ,chk ,chk
           (let ((,mk ,code))
             (n.asm (,mk)
               (expr ,mk)
               (Stsfld (field ,sn))
               (Ldnull))
             ,mk))))))

(unit-test 3 (let ((abc (fun (x) (list (straise (cons x x)) x))))
               (list (abc 10) (abc 20) (abc 30)))
           ( ((10 . 10) 10)
             ((10 . 10) 20)
             ((10 . 10) 30)))

(define _force_assembly_load
  (let ((GetTypes (r_tbind t_assembly "GetExportedTypes"))
        (tfh
         (r_mtd "System.Type" "GetTypeFromHandle"
                (dotnet "System.RuntimeTypeHandle")))
        (gtas
         (r_mtd "System.Type" "get_Assembly"))
        )
    (fun (as)
      (let* ((ts (GetTypes as))
             (t (car (a->l ts))))
        `(n.asm ()
            (Ldtoken ,t)
            (Call ,tfh)
            (Callvirt ,gtas))))))

(cmacro add-assembly (nm)
  (let* ((rnm (read-int-eval nm))
         (as (lookup-assembly-inner rnm)))
    `(try-context-assembly-wrapper ,(_force_assembly_load as) ,((r_tbind t_assembly "get_FullName") as))))

(expand-if (shashget (getfuncenv) 'compiler-final)

(cmacro for ((idx st . end) . body)
  (with-syms (lbl tp res)
    `(let ((,idx ,st) (,tp ,(car end)))
       (n.label ,lbl)
       (let ((,res (begin
                    ,@body)))
         (n.stloc! ,idx (+ ,idx ,(if (null? (cdr end)) 1 (cadr end))))
         (if (< ,idx ,tp) (n.goto ,lbl))
         ,res))))

(cmacro map (ffun lst)
  (with-syms (lx ly lz lf lbl)
    `(let ((,lx ,lst) (,ly nil) (,lz nil))
        (n.label ,lbl)
        (if (null? ,lx) ,ly
            (begin
               (let ((,lf (cons (,ffun (car ,lx)) nil)))
                  (if (null? ,ly) (begin (n.stloc! ,ly ,lf) (n.stloc! ,lz ,lf))
                                  (begin (set-cdr! ,lz ,lf) (n.stloc! ,lz ,lf)))
                  )
               (n.stloc! ,lx (cdr ,lx))
               (n.goto ,lbl)
               nil
               )))))

(cmacro mkhash ()
  `(n.asm ()
      (Newobj ,ctr_Hashtable)
      (Castclass ,t_object)
      ))

(cmacro hashgetODD (a b)
  (with-syms (aa bb)
    `(inner.let ((,aa ,a) (,bb ,b))
       (n.asm (,aa ,bb)
         (expr ,aa)
         (Castclass ,t_Hashtable)
         (expr ,bb)
         (Call ,m_getItem)
         ))))

(cmacro hashputODD (a b c)
  (with-syms (aa bb cc)
    `(inner.let ((,aa ,a) (,bb ,b) (,cc ,c))
       (n.asm (,aa ,bb ,cc)
         (expr ,aa)
         (Castclass ,t_Hashtable)
         (expr ,bb)
         (expr ,cc)
         (Call ,m_hashSetItem)
         (Ldnull)))))

(cmacro println (a)
  (with-syms (aa)
    `(inner.let ((,aa ,a))
       (n.asm (,aa)
          (expr ,aa)
          (Call ,m_WriteLine)
          (Ldnull)))))

(cmacro foreach-map ((id lst) . body)
  (with-syms (lx ly lz lf lbl)
    `(with-macros ((foreach-break (fun ()
                                    (list 'begin
                                          (list 'n.stloc! (quote ,lx) 'nil)
                                          (list 'n.goto (quote ,lbl))))))
     (let ((,lx ,lst) (,ly nil) (,lz nil))
        (n.label ,lbl)
        (if (null? ,lx) ,ly
            (begin
               (let ((,lf (cons (let ((,id (car ,lx))) ,@body) nil)))
                  (if (null? ,ly) (begin (n.stloc! ,ly ,lf) (n.stloc! ,lz ,lf))
                                  (begin (set-cdr! ,lz ,lf) (n.stloc! ,lz ,lf)))
                  )
               (n.stloc! ,lx (cdr ,lx))
               (n.goto ,lbl)
               nil
               ))))))

(cmacro foreach ((id lst) . body)
   (with-syms (lx lbl)
    `(with-macros ((foreach-break (fun ()
                                    (list 'begin
                                          (list 'n.stloc! (quote ,lx) 'nil)
                                          (list 'n.goto (quote ,lbl))))))
      (let ((,lx ,lst))
        (n.label ,lbl)
        (if (null? ,lx) nil
            (begin
               (let ((,id (car ,lx)))
                  ,@body)
               (n.stloc! ,lx (cdr ,lx))
               (n.goto ,lbl)
               nil
               ))))))

(cmacro collector ((add get) . body)
 (with-syms (clec cren v v1)
   `(let* ((,cren (cons nil nil))
           (,clec (mkref ,cren))
           (,add (fun (,v) (alet ,v1 (cons ,v nil)
                            (set-cdr! (deref ,clec) ,v1)
                            (r! ,clec ,v1)
                            ,v)))
           (,get (fun () (cdr ,cren))))
      ,@body)))

(cmacro foreach-mappend ((id lst) . body)
 (with-syms (add get id2)
  `(collector (,add ,get)
     (foreach (,id ,lst)
       (foreach (,id2 (begin ,@body)) (,add ,id2)))
     (,get))))

(cmacro map-over (lst ffun) `(map ,ffun ,lst))

)

(cmacro set-car! (xp xv)
  `(inner.set-car! ,xp ,xv))

(cmacro set-cdr! (xp xv)
  `(inner.set-cdr! ,xp ,xv))

(cmacro ageto (ar id)
  (with-syms (a i)
    `(let ((,a ,ar) (,i ,id))
       (n.asm (,a ,i)
         (expr ,a)
         (Castclass ,t_object_array)
         (expr ,i)
         (Unbox ,t_int)
         (Ldind_I4)
         (Ldelem_Ref)))))

(macro aseto (ar idx vl)
  `(aset ,ar ,idx ,vl))

(cmacro aseto (ar idx vl)
  (with-syms (v aar iid)
    `(let ((,v ,vl) (,aar ,ar) (,iid ,idx))
      (n.asm (,v ,aar ,iid)
        (expr ,aar)
        (Castclass ,t_object_array)
        (expr ,iid)
        (Unbox ,t_int)
        (Ldind_I4)
        (expr ,v)
        (Stelem_Ref)
        (Ldnull)
        )
      )))

(cmacro asetx (ar const-id vl)
  (with-syms (v aar)
    `(let ((,v ,vl) (,aar ,ar))
      (n.asm (,v ,aar)
        (expr ,aar)
        (Castclass ,t_object_array)
        ,(_ldc_i4 const-id)
        (expr ,v)
        (Stelem_Ref)
        (Ldnull)
        )
    )))

(cmacro cons (a b)
   `(inner.cons ,a ,b))
(cmacro car (a)
   `(inner.car ,a))
(cmacro cdr (a)
   `(inner.cdr ,a))
(cmacro + (a b)
   `(inner.arithop Add ,a ,b))
(cmacro - (a b)
   `(inner.arithop Sub ,a ,b))
(cmacro * (a b)
   `(inner.arithop Mul ,a ,b))
(cmacro / (a b)
   `(inner.arithop Div ,a ,b))

(include "./ccase.al")

(include "./fccase.al")

(expand-if (shashget (getfuncenv) 'compiler-final)
 (expand-if ##optimise-case
  (cmacro case rest
          `(cc-newgencase ,@rest))
  (cmacro gencase rest
          `(cc-newgencase ,@rest))
  )
)

(define t_Stringbuilder (dotnet "System.Text.StringBuilder"))
(define ctr_Stringbuilder (r_getconstructorf t_Stringbuilder nil))
(define _Stringbuilder_AppendObject (r_mtd t_Stringbuilder "Append" t_object))

(cmacro string-builder args
  (with-syms (sbild)
    `(let* ((,sbild (n.asm () (Newobj ,ctr_Stringbuilder) (Castclass ,t_object))))
       ,@(foreach-map (a args)
           (with-syms (na)
             `(let ((,na ,a))
                (n.asm (,sbild ,na)
                       (expr ,sbild)
                       (Castclass ,t_Stringbuilder)
                       (expr ,na)
                       (Call ,_Stringbuilder_AppendObject)
                       (Pop) (Ldnull)))))
       (->s ,sbild))))


(expand-if (shashget (getfuncenv) 'compiler-final)

(function modulo (a b)
  (n.asm (a b)
     (expr a)
     (Unbox_Any (othertype "System.Int32"))
     (expr b)
     (Unbox_Any (othertype "System.Int32"))
     (Rem)
     (Box (othertype "System.Int32"))
     ))
)

(include "./pmatchcomp.al")




