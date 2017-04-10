;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(function peg-collval (t v)
  (if v `(*val* ,@(cons t (cdr v))) `(*val* ,@t)))

(function __peg_xhashget (ht nm)
  (alet res (ohashget ht nm)
        (if res res
            (begin
              (writeline `(FAIL-ON ,nm))
              (ccerror `(O))))))


(macro peg-fail ()
  `(quote ,__peg-epic-fail__))

(macro peg-success? (arg)
  `(not (eqv? (quote ,__peg-epic-fail__) ,arg)))

(function peg-collect (t1 t2)
  (if (peg-success? t2) nil (peg-fail)))

(include "./backend-atoms.al")
(force-class-flush)
(include "./backend-recform.al")

(macro peg-inner-ignore terms
  `(begin
    ,@(foreach-map (t terms)
       `(peg-call-nr-terminal ,t))))

(macro peg-call-nr-terminal (name)
  `(__peg:sapply_ Env _lrstk ;;; may be: (mkref nil)
                  source
                  (straise (__peg_xhashget (PegContext) (quote ,(Sm<< name))))))

(macro peg-call (name)
  (with-syms (tmp)
      `(let ((,tmp (peg-call-terminal ,name)))
         (if (peg-success? ,tmp)
             (peg-dummy)
             ,tmp))))

(macro peg-call-nr (name)
  (with-syms (tmp)
      `(let ((,tmp (peg-call-nr-terminal ,name)))
         (if (peg-success? ,tmp)
             (peg-dummy)
             ,tmp))))

(macro peg-fast-collect (t1 t2)
  `(begin ,t2 ,t1 nil))

(macro const.eq? (v c)
  (with-syms (nv l1 l2)
    `(let ((,nv ,v))
       (n.asm (,nv)
          (expr ,nv)
          (Unbox_Any ,t_Int32)
          (Ldc_I4 ,c)
          (Bne_Un (label ,l1))
          (Ldsfld ,fld_True)
          (Br (label ,l2))
         (label ,l1)
          (Ldnull)
         (label ,l2)))))

(macro const.>= (v c)
  (with-syms (nv l1 l2)
    `(let ((,nv ,v))
       (n.asm (,nv)
          (expr ,nv)
          (Unbox_Any ,t_Int32)
          (Ldc_I4 ,c)
          (Bge (label ,l1))
          (Ldnull)
          (Br (label ,l2))
         (label ,l1)
          (Ldsfld ,fld_True)
         (label ,l2)))))

(macro const.<= (v c)
  (with-syms (nv l1 l2)
    `(let ((,nv ,v))
       (n.asm (,nv)
          (expr ,nv)
          (Unbox_Any ,t_Int32)
          (Ldc_I4 ,c)
          (Ble (label ,l1))
          (Ldnull)
          (Br (label ,l2))
         (label ,l1)
          (Ldsfld ,fld_True)
         (label ,l2)))))

(macro peg-char-in-ranges (ch . rcode)
  `(or ,@(foreach-map (r rcode)
           (p:match r
             (() 'nil)
             (($f $t)
              `(and (const.>= ,ch ,f)
                    (const.<= ,ch ,t)))
             (else
              `(const.eq? ,ch ,r))))))

(macro peg-check-ranges rngcode
  `(alet ch (__peg:get-check source)
     (if (and ch (peg-char-in-ranges ch ,@rngcode))
         (peg-dummy)
         (peg-fail))))

(macro peg-trivial-or ps
 (with-syms (tsaved)
  `(alet ,tsaved (__peg:get-position_ source)
     ,(let loop ((p ps))
        (p:match p
          (($a . $b)
           `(alet tmp ,a
                  (if (eqv? (quote ,__peg-epic-fail__) tmp)
                      (begin
                        (__peg:set-position_ source ,tsaved)
                        ,(loop b)))))
          (() `(begin
                 (quote ,__peg-epic-fail__))))))))

(macro peg-trivial-string str
  (let loop ((s str))
    (p:match s
      (($ch . $rest)
       `(let* ((chr (__peg:get-advance source)))
          (if (const.eq? chr ,ch)
              ,(loop rest)
              (quote ,__peg-epic-fail__))))
      (()
       `(quote ,str))
      )))

(macro peg-call-self-precedence (name prec)
  `(,name Env (list 'PRATT_ROCKS ,prec _lrstk) source))

(function __peg:get-cur-precedence (_lrstk_hack)
  (p:match _lrstk_hack
    ((PRATT_ROCKS $prec $lrstk) prec)
    (else 0)))

(function __peg:get-hack-lrstk (_lrstk_hack)
  (p:match _lrstk_hack
    ((PRATT_ROCKS $prec $lrstk) lrstk)
    (else _lrstk_hack)))

(macro peg-term-function-pratt (ranges name ttype lr vs dtype)
  `(inner.reclambda
      ,name (Env _lrstk_hack source)
      ,@(p:match lr
          ((peg-ignore $igcode $xcode)
           `((let ((_lrstk (__peg:get-hack-lrstk _lrstk_hack)))
               (peg-inner-ignore
                ,@igcode))))
          (else nil))
      ;; TODO: range check, dynamic extension nodes, reporting, signals.
      ;;
      ;;   Pratt parsing:
      ;;
      ;;   1) Try parsing lr
      ;;   2) If successful, store as Left
      ;;      Otherwise return the failure
      ;;   3) Orderly, nicely try the ops from the list
      ;;      TODO: have a range check for ops here
      ;;   4) If all failed, return Left
      ;;   5) If lucky, check precedence
      ;;      5.1) while less than the current, apply corresponding infix
      ;;           parser
      ;;      5.1) Otherwise return Left
      ;;
      ;;   An infix parser:
      ;;   1) Try parsing right side with a current precedence,
      ;;      if right assoc, use precedence-1
      ;;   2) If successful, apply a constructor
      ;;   3) Otherwise, backtrack to the beginning of the infix expr
      ;;      entry
      (let* ((saved (__peg:get-position_ source))
             (peg-node-name (quote ,name))
             (_lrstk (__peg:get-hack-lrstk _lrstk_hack))
             (L ,lr))
        (if (peg-success? L)
            (begin
              ,@(p:match lr
                  ((peg-ignore $igcode $xcode)
                   `((peg-inner-ignore
                      ,@igcode)))
                  (else nil))
            (let iloop ((L L))
              (let ((opsaved (__peg:get-position_ source)))
                ,(let loop ((v vs))
                   (p:match v
                     (() 'L)
                     (( (binary $precsion $assoc
                                $opcode
                                $dcode) . $res)
                      `(let ((op ,opcode))
                         (if (peg-success? op)
                             (let ((curprec (__peg:get-cur-precedence _lrstk_hack)))
                               (if (< curprec ,precsion)
                                   (let ((R (peg-call-self-precedence
                                             ,name
                                             ,(if (eqv? assoc 'right)
                                                  (- precsion 1)
                                                  precsion)
                                             )))
                                     (if (peg-success? R)
                                         (iloop (peg-dcode ,name ,dcode (list L op R) ,dtype))
                                         (begin
                                           (__peg:set-position_ source saved)
                                           (peg-fail))))
                                   (begin
                                     (__peg:set-position_ source opsaved)
                                     (return L))))
                             (begin
                               (__peg:set-position_ source opsaved)
                               ,(loop res))))
                      )))
                )))
            ;; L was not successful
            (begin
              (__peg:set-position_ source saved)
              (return L))
            ))))

(macro peg-term-function (ranges name ttype dcode icode report dtype)
  (let ((prefx
         (p:match icode
           ((peg-ignore $igcode $xcode)
            `((peg-inner-ignore
               ,@igcode)))
           (else nil)))
        (struc (foreach-map (i (car dcode))
                 (format i (nm ml tn)
                         `(,nm nil))))
        (mkstruc (foreach-map (i (car dcode))
                   (format i (nm ml tn)
                         nm)))
        (rchk  (if ranges
                   (fun (ic)
                     `(if (peg-success? (peg-check-ranges ,@ranges))
                          ,ic
                          (peg-fail)))
                   (fun (ic) ic)))

        )
    `(inner.reclambda ,name (Env _lrstk source)
       ,@prefx
       ,@(if (shashget (getfuncenv) 'debug-compiler-peg)
               `((writeline (list 'TRYING-PEG-NODE (quote ,name)
                                  'AT (__peg:displaypos (__peg:get-position_ source)) 'WILL-DO (quote ,icode)
                                  ))))
       ,(rchk
          `(let* ((saved (__peg:get-position_ source))
                  (peg-node-name (quote ,name))
              ,@struc
              (finalresult nil)
              (result
               ,icode
               ))
         ,@(if (shashget (getfuncenv) 'debug-compiler-peg)
               `((writeline (list 'ATTEMPTED-PEG-NODE (quote ,name)
                                  'AT (__peg:displaypos saved)
                                  'RESULT-IS (peg-success? result)
                                  'POSITION-NOW (__peg:displaypos (__peg:get-position_ source))))))
         (alet __result
          (if (peg-success? result)
             (peg-dcode ,name ,dcode
                        ,(case ttype
                           ((normal)
                            'nil)
                           ((term)
                            (if mkstruc `(list ,@mkstruc) 'finalresult))
                           ((token)
                            `(__peg:get-delta saved
                                              (__peg:get-position_ source))))
                        ,dtype
                        )
             (begin
               ,@(if report
                     `((__peg:env:rightmostfailure Env source (quote ,name)
                                                   ,report))
                     `((__peg:env:rightmostfailure Env source (quote ,name)
                                                   ,(S<< name)))
                     )
               result))
          __result
          ))))))

(function __peg-compile-acode (name acode)
  (if acode
      `(if Env
           (let* ((last (__peg:get-position_ source)))
             ,@(map-over acode
                (fmt (n l)
                     (if (eqv? l 'LOCTOKEN)
                         `(peg:env-signal Env (quote ,name) saved last (list (quote ,n)
                                                                             (gensym)))
                         `(peg:env-signal Env (quote ,name) saved last
                                          (quote (,n ,l)))))
                )))
      nil
      ))

(macro peg-dcode (name adcode rcode dtype)
  (format adcode (istruct annot constr)
    (let*
        ((ac (__peg-compile-acode name annot))
         (cc
          `(if (peg:construct? Env)
               ,(p:match constr
                  ((nop)
                   `(peg-return (quote ,name) ,rcode))
                  (else ;; not possible for token terms!
                   `(peg-constr-compile ,constr ,istruct ,dtype)
                   )))))
      (if ac
          `(begin
             ,ac
             ,cc)
          cc))))

(function peg-constr-fmtvars (istruct)
  (with-ohash (ht)
     (foreach (i istruct) (ht! (car i) #t))
     (return ht)))

(macro peg-constr-compile-listform (constr srcfmt dtype)
 (alet varshash (peg-constr-fmtvars srcfmt)
  (packrat:visit code constr
    (code DEEP
       ((var (if (ohashget varshash name) name
                 (if (eqv? name 'pegenv)
                     'Env
                     (ccerror `(PEG:UNKNOWN-CONSTRUCTOR-VARIABLE ,name IN ,constr)))))
        (const (list 'quote s))
        (fcall `(,(Sm<< "peg-function-" fname) ,@ars))
        (constr `(cons (quote ,cname) (quasiquote ,ars)))
        (action code)
        (auto  (ccerror `(PEG:AUTO-NOT-ALLOWED)))
        (dauto   (ccerror `(PEG:RECFORM)))
        (dconstr (ccerror `(PEG:RECFORM)))
        ))
    (carg DEEP
       ((set `(unquote ,val)) ;; var name is for AST building purposes
        (append `(unquote-splicing ,val))))
    ))

(macro peg-constr-compile-recform (constr srcfmt dtype)
   ;; TODO:
   ;; Identify the target node and variant,
   ;; fetch the format, match this format against provided args
   ;; Matching rules are following:
   ;;  - if all the provided names are matching the format names,
   ;;    assume the user meant it. TODO: generate nils instead of gensyms,
   ;;    then add a pass to lift trivial variable names into cargs.
   ;;  - if there is no name match, assume sequential substitution
   ;;  - if there is an append node in the format, match accordingly
   `(inner-expand-first
     peg-constr-compile-recform-inner (quote ,constr)
     (quote ,srcfmt)
     (quote ,dtype)
     (packrat-target-ast)
     (packrat-target-node)
     ))

(macro peg-constr-compile (constr srcfmt dtype)
  `(inner-expand-first
    peg-constr-compile-select (quote (,constr ,srcfmt ,dtype))
    (packrat-target-ast)
    ))

(macro peg-constr-compile-select (qargs ast)
  ;; (writeline `(CONSTR: ,qargs ,ast))
  (p:match ast
    ((packrat-target-ast) `(peg-constr-compile-listform ,@(cadr qargs)))
    (else `(peg-constr-compile-recform ,@(cadr qargs)))))

(function peg-return (name rv)
  (p:match rv
    (
     (*val* . $x) ;; single value substituted
     x)
    (else
     (cons name rv))))

(function peg-list-exports (defs)
  (map car defs))

(function peg-list-ranges (defs)
  (foreach-mappend (d defs)
    (p:match d
      (($nm (peg-term-function $rng . $_))
       (if rng `((,nm ,rng)))))))

(function __peg-parser_makeinitfunctions (entry defs targetast)
  (collector (fadd fget)
  (collector (fnadd fnget)
   (let* (
          (with-ast (fun (c)
                      (if targetast
                          `(ast2:with-ast ,targetast ,c)
                          c)))
          (continue
            (fun (dfs)
             (let* ((fn (gensym))
                    (fb
                     `(top-begin
                        (function ,fn (Context)
                        ,(with-ast
                        `(with-macros
                         (
                          ,@(if targetast `((packrat-target-ast (fun (_) (quote ,targetast)))))
                          (PegContext (fun (_)
                                        (quote
                                         ,(Sm<< "peg_" entry "_Context")))))
                         ,@(foreach-map (d dfs)
                             (format d (n v)
                                     `(ohashput Context (quote ,(Sm<< n))
                                               (cons ,v (quote ,(Sm<< n)))))))))
                        (force-class-flush))
                     ))
               (fadd fb)
               (fnadd fn)))))
     (let loop ((i 0) (d defs) (rd nil))
       (cond
        ((null? d)
         (continue rd))
        ((> i 50)
         (begin
           (continue rd)
           (loop 0 d nil)))
        (else
         (loop (+ i 1) (cdr d) (cons (car d) rd)))))
     (list (fget)
           (fnget))))))

(macro peg-parser (export? entry borrow defs exports dhooks targetast nodetypes)
 (let* ((initnm (if (not export?) (gensym) (Sm<< "peg_" entry "_init")))
        (initfns (__peg-parser_makeinitfunctions entry defs targetast))
        (ainits (car initfns))
        (binits (cadr initfns)))
  `(top-begin
     ,@(if export?
           `((define ,(Sm<< "peg_" entry "_Context") (mkhash))))
     ,@(foreach-map (d dhooks)
         `(define ,(Sm<< "peg_" entry "_point_" (cadr d)) (mkref nil)))
     ,@ainits
     (define ,initnm
        (fun (Context)
          ,@(foreach-map (b borrow)
              (if (not (eqv? b entry)) ;; it is possible in dynamic extensions
                `(alet cnt ,(Sm<< "peg_" b "_contents")
                   (if (list? cnt)
                       (foreach (i ,(Sm<< "peg_" b "_contents"))
                         (ohashput Context i
                                  (ohashget ,(Sm<< "peg_" b "_Context") i)))
                       (hashiter (fun (k v)
                                   (ohashput Context k v))
                                 ,(Sm<< "peg_" b "_Context"))))))
          ,@(foreach-map (d dhooks)
              (format d (dh name)
                `(ohashput Context (quote ,(Sm<< name))
                  (cons
                   (inner.reclambda ,(Sm<< "entry_" name) (Env _lrstk source)
                     (alet saved (__peg:get-position_ source)
                     (let loop ((lst (deref ,(Sm<< "peg_" entry "_point_" name))))
                       (if (null? lst)
                           (peg-fail)
                           (alet  a1 (__peg:apply_ Env _lrstk source (car lst))
                             (if (peg-success? a1)
                                 a1
                                 (begin
                                   (__peg:set-position_ source saved)
                                   (loop (cdr lst)))
                                 )))
                       )))
                   (quote ,(Sm<< name))

                   ))))
          ,@(foreach-map (d binits)
              `(,d Context))
          ))
     ,@(if export?
           (let ((ctn (peg-list-exports defs)))
             (if (> (length ctn) 200)
                 `((define ,(Sm<< "peg_" entry "_contents") 'fromhash))
                 `((define ,(Sm<< "peg_" entry "_contents")
                     (quote ,ctn))))))
     ,@(if export?
           (let ((ctn (peg-list-ranges defs)))
             `((define ,(Sm<< "peg_" entry "_ranges") (quote ,ctn)))))
     (,initnm
      ,(Sm<< "peg_" entry "_Context"))
     ,@(if export?
        `((define ,(Sm<< "peg_" entry)
            (fun (Env stream)
              (with-macros
               (
                ,@(if targetast `((packrat-target-ast (fun (_) (quote ,targetast)))))
                (PegContext (fun (_)
                              (quote ,(Sm<< "peg_" entry "_Context")))))
               (let* (( source (mkref stream) )
                      ( _lrstk (mkref nil) ))
                 ;(try
                  (alet  res (peg-call-terminal ,entry)
                         (cons res (__peg:get-position_ source)))
                 ; t_Exception
                 ; (fun (e)
                 ;   (println (->s e))
                 ;   (peg-fail)
                 ;   ))
                 ))
              ))
          (define ,(Sm<< "peg-exportmacros-" entry "-src")
            (quote ,exports))
          (define ,(Sm<< "peg-exportnodes-" entry "-src")
            ,(S<< nodetypes))
          (define ,(Sm<< "peg-exportdepends-" entry)
            (quote ,borrow))
          (force-class-flush) ; make sure macros are available in the hash
          )))))

;------------------
(macro peg-extend-dynhook (tgt tnode src snode)
  `(let* ((t ,(Sm<< "peg_" tgt "_point_" tnode))
          (s ,(Sm<< "peg_" src "_Context"))
          (sn (ohashget s (quote ,(Sm<< snode)))))
     (r! t (cons sn (deref t)))))

(macro peg-minigrammar (tgt tnode abor clause . adden)
  (let* ((nm (gensym))
         (inm (gensym))
         (code `(packrat-ast ,inm (,tgt ,@abor)
                  ,@adden
                  (terminal () term ,nm ,@clause))))
    `(top-begin
       ,code
       (peg-extend-dynhook ,tgt ,tnode ,inm ,nm))))

;-----------------

(function peg-alldead? (strm)
  (or (null? strm)
      (null? (StreamEntry.chknext strm))
      (<= (StreamEntry.char strm) 0)
      ))

;---
(function peg:easyparse (pars stream)
  (let* ((env (peg:makeenv))
         (res (pars env stream)))
    (if (and (peg-success? (car res))
             (peg-alldead? (cdr res)))
        (car res)
        (let* ((rf (peg:reportfailure env))
               (f1 (if (null? rf)
                       (__peg:displaypos (cdr res))
                       rf)))
          (cons 'FAIL:
                (cons
                 res
                 f1))
        ))))

(function peg:easyparse2 (pars stream)
  (let* ((env (peg:makeenv))
         (res (pars env stream)))
    (if (peg-success? (car res))
        res
        (cons
         (cons 'FAIL:
               (peg:reportfailure env))
         (cdr res)
         ))))

(function peg:easyparse3 (env pars stream)
  (let* ((res (pars env stream)))
    (if (peg-success? (car res))
        res
        (cons
         (cons 'FAIL:
               (peg:reportfailure env))
         (cdr res)
         ))))

;---

(function peg-function-sval (v)
  (string->symbol (cdr v)))

(function peg-function-nval (v)
  (S->N (cdr v)))

(function peg-function-val (v)
  (cdr v))

(function peg-function-stripval (v)
  (alet v0 (cdr v)
   (list->string (cdr (cuttail (string->list v0))))))

(function peg-function-symbol (s)
  (string->symbol s))

(function peg-function-cons (a b)
  (cons a b))


(function peg-function-list (a b)
  (list a b))

(function peg-function-wrap (a)
  (list a))

(function peg-function-nil ()
  nil)

(function peg-function-true ()
  #t)

(function peg-function-nullp (a)
  (null? a))

(function peg-function-not (a)
  (not a))

(function peg-function-charcode (s)
  (ascii (car (string->list s))))

(function peg-function-sstripval (v)
  (alet v0 (cdr v)
   (string->symbol
    (list->string (cdr (cuttail (string->list v0)))))))

(function peg-function-hexval (v)
  (alet v0 (cdr v)
   (HX->N
    (list->string (cddr (string->list v0))))))

(function peg-function-gensym ()
  (gensym))

(function peg-function-cadr (x) (cadr x))
(function peg-function-caddr (x) (caddr x))

(function peg-function-cutlast (s) (list->string
                                    (cuttail (string->list s))))
(function peg-function-stringtolist (s) (string->list s))
(function peg-function-chr (n) (n2s n))

(function peg-function-charval(n)
  (n2s (S->N (list->string (cuttail (string->list (cdr n)))))))

(define scr-replacers (strreplacers* ("\\\\" "\\")
                                     ("\\n" "\n")
                                     ("\\t" "\t")
                                     ("\\\"" "\"")
                                     ))

(function peg-function-stringdescreen (str)
  (strapply* scr-replacers str)
  )


(function pf:makenewentry (pos c)
  (StreamEntry.new nil nil pos c nil))

