;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(recfunction cc:strip-meta (node)
  (p:match node
    ((Meta $nn) (cc:strip-meta nn))
    ((IntPtr $nn) (cc:strip-meta nn))
    (else node)))

(function cc:atom (node target)
  (p:match target
    ((local $nm)
     `((Setlocal ,nm ,node)))
    (push
     `((Push ,node)))
    (pushmeta
     `((Push (Meta ,(cc:strip-meta node)))))
    (tail
     `((Return ,node)))
    (drop ; no need to reference this atom
     '())))

(function cc:pop (target)
  (p:match target
    ((local $nm)
     `((Setlocal ,nm (Pop))))
    (push nil)
    (pushmeta
     (cc:comperror `(I03:IMPOSSIBLE ,target)))
    (tail
     `((Return (Pop))))
    (drop ; no need to reference this atom
     '((Pop (Pop))))))

(function cc:expr->flat ( expr )
  (<> expr
   (ast:revisit tailloop ; revisitor name
                ((target 'tail) (escs nil)) ; additional args
                cc:mbcoreast expr ; AST and top node
;= For a block of code, only the last expression can be a tail call.
      ((Begin
        (let* ((pr (map 
                    (cut tailloop <> 'drop escs) 
                    (cuttail es)))
               (pst (list 
                     (tailloop (car (lasttail es))
                               target escs))))
          `(,@(foreach-mappend (x pr) x)
            ,@(foreach-mappend (x pst) x))))
;= Use a target value to set locals
       (SLet 
        (let* ((locls (map car defs))
               (cmpl (map (fmt (nm vl)
                            (tailloop vl 
                                      `(local ,nm) escs))
                          defs))
               (bdy (tailloop body target escs)))
          `(,@(foreach-map (l locls)
                  `(Local ,l))
            ,@(foreach-mappend (c cmpl) c)
            ,@bdy)))
;= Same for recursive definitions
       (SLetRec
        (let* ((locls (map car defs))
               (cmpl (map (fmt (nm vl)
                            (tailloop vl 
                                      `(local ,nm) escs)) 
                          defs))
               (bdy (tailloop body target escs)))
          `(,@(foreach-map (l locls)
                  `(Local ,l))
            ,@(foreach-mappend (c cmpl) c)
            ,@bdy)))
;= Closure patcher is to be unrolled later, treat it is a high level instruction.
       (PatchClosure
        `((Patchclosure ,nm ,liftname ,@args)))
       (Label `((Label ,id)))
       (GotoLabel `((Goto ,id)
                    ,@(cc:atom '(Nil) target)
                    ))
;= [[If]] form flattening
       (If
        (let* ((lbl1 (gensym))
               (lbl2 (gensym)))
          `(,@(tailloop e 'push escs)
            (Gotoifnot ,lbl1 (Pop))
            ,@(tailloop iftr target escs)
            ,@(p:match target
                (tail nil)
                (else `((Goto ,lbl2))))
            (Iflabel ,lbl1)
            ,@(tailloop iffl target escs)
            ,@(p:match target
                (tail nil)
                (else `((Label ,lbl2))))
            )))
;= [[While]] form flattening, showing some respect to escapes
       (While
        (let* ((lbll (gensym))
               (lble (gensym)))
          `((Label ,lbll)
            ,@(tailloop cnd 'push escs) ; Escape does not work for condition.
            (Gotoifnot ,lble (Pop))
            ,@(tailloop body 'drop 
                        (cons `(,escapename ,lble) escs))
            (Goto ,lbll)
            (Iflabel ,lble)
            ,@(tailloop '(Nil) target nil) ; Always return nil
            )))
;= [[Escape]] is a companion to [[While]], it provides a way of premature loop termination.
       (Escape
        (let* ((f (find (fmt (w) 
                             (eqv? w escapename)) escs)))
          (if (null? f) 
              (cc:comperror `(CC02-ESC ,escapename)))
          (format (car f) (_ lbl)
            `((Goto ,lbl)))))
;= Try-catch
       (TryBlock
        (with-syms (exv inr)
         `((Local ,exv)
           (Local ,inr)
           (TryBegin)
           ,@(tailloop body 'push nil)
           (Setlocal ,inr (Pop))
           (CatchBegin ,tcatch)
           (Setlocal ,exv (Pop))
           ,@(tailloop `(App ,actionfun (Var ,exv)) 'push nil)
           (Setlocal ,inr (Pop))
           (TryEnd)
           ,@(cc:atom `(Var ,inr) target)
           )))
       (Dummy nil)

       (FixLocal `((FixLocal ,id ,oldid)))
       (DebugPoint `(,node))

;= [[XSet]] destructive operation, compilation depends on a target type
       (XSet
        (case tp
          ((Glob)
           `(,@(tailloop vl 'push nil)
             (Setglobal ,nm (Pop))
             ,@(tailloop '(Nil) target nil)))
          ((Var)
           `(,@(tailloop vl `(local ,nm) nil)
             ,@(tailloop '(Nil) target nil)))
          )
        )
;= [[GSet]] toplevel control construction
       (GSet
        `(,@(tailloop vl 'push nil)
          (Setnewglobal ,nm (Pop))
          ,@(cc:atom `(Quote ,nm) 'push)
          (Push (Meta (Glob ,nm)))
          (Push (Glob ,nm))
          (InitGlobalVar (Pop) (Pop) (Pop))
          ))

;= [[GSetFn]] toplevel control construction
       (GSetFn
        `(,@(cc:atom `(Quote ,nm) 'push)
          ,@(tailloop vl 'pushmeta nil)
          (Push (NBNum ,nargs))
          (InitGlobalFun (Pop) (Pop) (Pop))
          ))

;= [[GSetM]] toplevel control
       (GSetM
        `((Push (Str ,(S<< nm)))
          ,@(tailloop vl 'push nil)
          (InitGlobalMac (Pop) (Pop))
          ))

;= [[Switch]] construction
       (Switch
        (let* ((lbls (map (fun (_) (gensym)) opts))
               (tg (p:match target
                     (tail nil)
                     (else (gensym)))))
          `(,@(tailloop body 'push nil)
            ,@(if (not n) `((IntUnbox (Pop))) nil)
            (Switch (Pop) ,@lbls)
            ,@(foreach-mappend (o (zip lbls opts))
                 `((Label ,(car o))
                   ,@(tailloop (cadr o) target nil)
                   ,@(if tg `((Goto ,tg)) nil)))
            ,@(if tg `((Label ,tg)) nil))
          ))

;= [[Labels]] construction
       (Labels
         (let* ((tg (p:match target
                      (tail nil)
                      (else (gensym)))))
         `(
          ,@(foreach-mappend (ex es)
             (format ex (lbl e)
              `((Label ,lbl)
                ,@(tailloop e target nil)
                ,@(if tg `((Goto ,tg)) nil))))
          ,@(if tg `((Label ,tg)) nil))))

;= [[App]] is a function application, a lengthy chunk of code.
       (App
        (let* ((aas0 (map (fun (_) '(Pop)) args))
               (prepost ; list of (args pre-code function-expression)
                (p:match fn
;= A global function is referenced, so it is a simple static application case.
                  ((Glob $glb)
                   (list 
                    aas0
                    (foreach-mappend (a args)
                     (tailloop a 'push nil)
                     )
                    `(Glob ,glb)))
;= Recursive reference is a special case                  
                  ((Recref $rc)
                   (list
                    aas0
                    `((ClosureDummy)
                      ,@(foreach-mappend (a args)
                           (tailloop a 'push nil)
                           ))
                    `(GlobRec ,rc)))
;= Function reference - simple one
                  ((Funref $glb)
                   (list 
                    aas0
                    (foreach-mappend (a args)
                     (tailloop a 'push nil)
                     )
                    `(Glob ,glb)))
;= Function itself should be computed.
                  (else
                   (with-syms (nm)
                     (list 
                      (cons '(Pop) aas0)
                      `(,@(tailloop fn 'push nil)
                        ,@(foreach-mappend (a args)
                           (tailloop a 'push nil)
                           ))
                      `(Gencall ,(length aas0))
                      ))))))
;= Now we must deal with the variety of value return methods, including
;= a tail call and safe disposal of unwanted return value.
          (format prepost (aas pre fnn)
          `(,@pre
            ,(p:match target
               ((local $nm)
                `(Setlocapp ,nm ,fnn ,@aas))
               (push
                `(Pushapp ,fnn ,@aas))
               (tail
                `(Retapp ,fnn ,@aas))
               (drop
                `(Dropapp ,fnn ,@aas))
               (else
                (cc:comperror `(CC01:IMPOSSIBLE ,target)))
               )))))
;= Hacking thing is just passed down with no changes
       (BackendAsm
        `((Asm ,(if (eqv? atp 'block)
                    (if (eqv? target 'tail) 'push target)
                    target)
               ,use ,@body)))
;= Performance enforcment
       (Car
        `(,@(tailloop e 'push nil)
          (Car (Pop))
          ,@(cc:pop target)
          ))
       (Cdr
        `(,@(tailloop e 'push nil)
          (Cdr (Pop))
          ,@(cc:pop target)
          ))
       (Cons
        `(
          ,@(tailloop a 'push nil)
          ,@(tailloop b 'push nil)
          (Cons (Pop) (Pop))
          ,@(cc:pop target)
          ))
       (BinOp
        `(,@(tailloop left 'push nil)
          (IntUnbox (Pop))
          ,@(tailloop right 'push nil)
          (IntUnbox (Pop))
          (BinOp ,op (Pop) (Pop))
          (IntBox (Pop))
          ,@(cc:pop target)
          ))
          
       (Eqv
        (with-syms (l1 l2)
          `(,@(tailloop a 'push nil)
            ,@(tailloop b 'push nil)
            (GotoEqv (Pop) (Pop) ,l1)
            (Push (Nil)) (Goto ,l2)
            (Label ,l1) (Push (Bool #t))
            (Label ,l2)
            ,@(cc:pop target))))

       (NullP
        (with-syms (l1 l2)
          `(,@(tailloop a 'push nil)
            (GotoNull (Pop) ,l1)
            (Push (Nil)) (Goto ,l2)
            (Label ,l1) (Push (Bool #t)) (Label ,l2)
            ,@(cc:pop target))))

       (PairP
        (with-syms (l1 l2)
          `(,@(tailloop a 'push nil)
            (GotoPairP (Pop) ,l1)
            (Push (Nil)) (Goto ,l2)
            (Label ,l1) (Push (Bool #t)) (Label ,l2)
            ,@(cc:pop target))))
                   

       (Not
        (let* ((lbl1 (gensym))
               (lbl2 (gensym)))
          `(,@(tailloop a 'push escs)
            (Gotoifnot ,lbl1 (Pop))
            ,@(cc:atom '(Nil) target)
            ,@(p:match target
                (tail nil)
                (else `((Goto ,lbl2))))
            (Iflabel ,lbl1)
            ,@(cc:atom '(Bool #t) target)
            ,@(p:match target
                (tail nil)
                (else `((Label ,lbl2))))
            ))
        )

       (IfNull
        (with-syms (lbl1 lbl2)
          `(,@(tailloop e 'push escs)
            (GotoNull (Pop) ,lbl1)
            ,@(tailloop iffl target escs)
            ,@(p:match target
                (tail nil)
                (else `((Goto ,lbl2))))
            (Label ,lbl1)
            ,@(tailloop iftr target escs)
            ,@(p:match target
                (tail nil)
                (else `((Label ,lbl2))))
            )))

       (IfPair
        (with-syms (lbl1 lbl2)
          `(,@(tailloop e 'push escs)
            (GotoPairP (Pop) ,lbl1)
            ,@(tailloop iffl target escs)
            ,@(p:match target
                (tail nil)
                (else `((Goto ,lbl2))))
            (Label ,lbl1)
            ,@(tailloop iftr target escs)
            ,@(p:match target
                (tail nil)
                (else `((Label ,lbl2))))
            )))

       (IfEqv
        (with-syms (lbl1 lbl2)
          `(,@(tailloop a 'push escs)
            ,@(tailloop b 'push escs)
            (GotoEqv (Pop) (Pop) ,lbl1)
            ,@(tailloop iffl target escs)
            ,@(p:match target
                (tail nil)
                (else `((Goto ,lbl2))))
            (Label ,lbl1)
            ,@(tailloop iftr target escs)
            ,@(p:match target
                (tail nil)
                (else `((Label ,lbl2))))
            )))

;= And the rest are special cases:
       (Funref
        (cc:atom `(IntPtr (Glob ,id)) target))
       (Recref
        (cc:atom `(GlobRec ,id) target))
;= And the same pattern as above applies to all the atomic value expressions
;= (Str, Num, Chr, Bool, Quote, Var, Nil, Arg, Clenv, Glob):
       (else (cc:atom node target))
       )
      ())))

(function cc:flat-peephole ( code )
  (collector (caddx cget)
   (let loop ((c code) (cadd caddx))
     (alet nxt
      (p:match c
        (((IntBox (Pop))
          (IntUnbox (Pop)) . $rest) rest)
        (((IntUnbox (Pop))
          (IntBox (Pop)) . $rest) rest)
        (((Push (Num $n))
          (IntUnbox (Pop)) . $rest)
         (begin
           (cadd `(Push (NBNum ,n)))
           rest))
        (((Goto $lbl) (Label =lbl) . $rest)
         (begin (cadd `(Label ,lbl))
                rest))
        (((Goto $lbl) . $rest)
         (alet nr
          (let iloop ((r rest))
            (if (p:match r
                  (() #t)
                  (((Label $_) . $_) #t)
                  (((Iflabel $_) . $_) #t)
                  (else nil))
                r (iloop (cdr r))))
          (p:match nr
            (((Label =lbl) . $rest1) nr)
            (else (begin
                    (cadd `(Goto ,lbl))
                    nr)))))
        (((Nop) . $rest) rest)
        (($hd) (begin (cadd hd) nil))
        (($hd . $rest)
         (begin (cadd hd)
                rest))
        (else (begin
                (iter cadd c) nil)))
      (if nxt (loop nxt cadd))))
   (cget)))

(function cc:expr->flat2 (e)
  (let ((tmp (cc:expr->flat e)))
    (ctime (if (shashget (getfuncenv) 'compiler-final)
               '(cc:flat-peephole tmp)
               'tmp))))

(function cc:compile-lifted ( src )
  (cc:mbcoreast:visit lifted src
    (liftop _
       (
        (Init `(Init ,mode ,(cc:expr->flat2 e)))
        (Global `(Global ,name ,usename ,(cc:expr->flat2 vl)))
        (Funref node)
        (Simple
         (format e ("Fun" _ fargs cd)
            `(Simple ,name ,usename ,fargs 
                     ,(cc:expr->flat2 cd))))
        (Closure
         (format e ("Fun" _ fargs cd)
                 `(Closure  
                   ,name ,usename ,args ,fargs 
                   ,(cc:expr->flat2 cd))))))
    (expr _
          (forall 
           (cc:expr->flat2 node)))))

