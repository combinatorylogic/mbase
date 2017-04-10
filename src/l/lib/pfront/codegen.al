;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "./ast.al")

; For: a-b-c-d-e
; (leftass (- a (leftass (- b (leftass (- c (leftass (- d e))))))))
;   =>
; (- (- (- (- a b) c) d) e)
(function fixleft (leftass n)
  (hlevel:visit expr n
    (expr _
      ((leftass (leftass node))
       (unleft (leftass node))
       (stopfix a)
       (else-deep ((else node)))))))

(recfunction leftass (node)
  (p:match node
    ((unleft (leftass $x)) x)
    ((leftass (binop $op $a (leftass (binop $opr $b $c))))
     (leftass `(leftass (binop ,opr (stopfix (binop ,op ,(fixleft leftass a) ,(fixleft leftass b)))
                               ,c))))
    ((leftass $x) x)
    (else node)))

(function fixleft_outer (n)
  (let* ((res (fixleft leftass n))
         (res1 (hlevel:visit expr res
                  (expr DEEP
                    ((leftass a)
                     (unleft a)
                     (stopfix a)
                     (else node))))))
    (return res1)))


(recfunction vtp (node tp)
  (p:match tp
    ((id once) `(,node _))
    ((id deep) `(,node DEEP))
    ((as $i) `((,node ,i) DEREF))
    ((asonce $i) `((,node ,i) DEREFT))
    ((withdst $t $d) (let* ((t0 (vtp node t)))
                       `(,(car t0) -> ,d ,@(cdr t0))))
    ))

(function normal-or-append (consfun appendfun rnil args)
  (let loop ((a args))
    (p:match a
      (() rnil)
      (((normal $x) . $rest)
       (consfun x (loop rest)))
      (((append $x)) x)
      (((append $x) . $rest)
       (appendfun x (loop rest)))
      (else (ccerror `(WHAT? ,a))))))


 (function hlevel-gen-visit-ops (ops)
   (foreach-mappend (o ops)
     (hlevel:visit visitopt o
        (visitopt DEEP
           ((recform nil)
            (dst `((dst ,id)))
            (listformsrc `((listform_src)))
            (listformdst `((listform_dst))))))))


 (function hlevel-gen-visit (ops ast top e ps)
   `(ast:visit:new ,ast ,(hlevel-gen-visit-ops ops) ,top ,e ,@ps))

 (function hlevel-compile-expr (c)
  (hlevel:visit expr c
    (expr DEEP
      ((cons `(cons ,a ,b))
       (easyconstr `(cons (quote ,tag)
                      ,(normal-or-append (fun (a b) `(cons ,a ,b))
                                         (fun (a b) `(append ,a ,b))
                                         'nil
                                         args)))
       (append `(append ,a ,b))
       (leftass a)
       (unleft a)
       (stopfix a)
       (binop (case o
                ((+.) `(f+ ,a ,b))
                ((-.) `(f- ,a ,b))
                ((*.) `(f* ,a ,b))
                ((/.) `(f/ ,a ,b))
                ((mod) `(modulo ,a ,b))
                ((===) `(eqv? ,a ,b))
                ((==) `(eq? ,a ,b))
                ((!==) `(not (eqv? ,a ,b)))
                ((!=) `(not (eq? ,a ,b)))
                (($==) `(string=? ,a ,b))
                ((&&) `(and ,a ,b))
                ((||) `(or ,a ,b))
                (else `(,o ,a ,b))))
       (range `(fromto ,a ,b))
       (accelement `(,(Sm<< a ".>") ,b))
       (list (normal-or-append (fun (a b) `(cons ,a ,b))
                               (fun (a b) `(append ,a ,b))
                               'nil
                               es))
       (begin `(begin-with-defs ,@es))

       (flatbegin `(flatbegin-inside-begin-with-defs ,@es))

       (loc (format L (Src F T)
               (letf (((fl fc) (peg:decode-pos F))
                      ((tl0 tc0) (peg:decode-pos T)))
                 (let* ((tl fl)
                        (tc (if (> tl0 fl)
                                (+ fc 1)
                                tc0)))
                   `(pfront.debugpoint ,Src ,(+ fl 1)
                                       ,(+ fc 1) ,(+ tl 1) ,(+ tc 1))
                   ))))

       (call `(,fn ,@args))
       (match `(p:match ,e ,@ps))
       (case `(case ,e ,@es ,@(if els `((else ,@els)))))
       (cond `(cond ,@es ,@(if els `((else ,@els)))))
       (visit
        (p:match os
          ((none)
           `(,(Sm<< ast ":visit") ,top ,e ,@ps))
          ((some . $ops)
           (hlevel-gen-visit ops ast top e ps))))
       (viter
        `(,(Sm<< ast ":iter") ,top ,e ,@ps))
       (if3 `(if ,e ,tr ,fl))
       (if2 `(if ,e ,tr))
       (lambda `(fun ,args ,body))
       (letloop `(let ,(_get_md_id nm) ,args
                      (with-added-metadata (,(cdr nm)) ,b)))
       (withast `(ast2:with-ast ,nm ,e))
       (mknode `(ast:mknode (@md (,md)) ,@args))
       (mkvnode `(ast2:vctr ,nd ,tag ,@args))
       (mkxnode `(ast2:xctr ,tag ,@args))
       (mksnode `(ast2:ctr  ,nd ,@args))
       (filter
          (p:match n
            ((var $vnm . $_)
             `(foreach-map-filter (,vnm ,e) ,b ,vnm))
            ((ptn $ptn)
             (alet vnm (gensym)
             `(foreach-map-filter (,vnm ,e) (pfront.with-format ,vnm ,ptn ,b) ,vnm)))))
       (mappend
          (p:match n
            ((var $vnm . $_)
             (if cn
                    `(foreach-mappend-count (,n ,e ,(car cn)) ,b)
                    `(foreach-mappend (,n ,e) ,b)))
            ((ptn $ptn)
             (alet vnm (gensym)
               (if cn
                    `(foreach-mappend-count (,vnm ,e ,(car cn))
                                            (pfront.with-format ,vnm ,ptn ,b))
                    `(foreach-mappend (,vnm ,e)
                                      (pfront.with-format ,vnm ,ptn ,b)))))))

       (map
           (p:match n
             ((var $vnm . $md)
              (if cn `(foreach-map-count (,n ,e ,(car cn)) ,b)
                     `(foreach-map (,n ,e) ,b)))
             ((ptn $ptn)
              (alet vnm (gensym)
              (if cn `(foreach-map-count (,vnm ,e ,(car cn))
                                         (pfront.with-format ,vnm ,ptn ,b))
                     `(foreach-map (,vnm ,e) (pfront.with-format ,vnm ,ptn ,b)))))
             ))
       (iter
        (p:match n
          ((var $vnm . $md)
           (if cn `(foreach-count (,n ,e ,(car cn)) ,b)
               `(foreach (,n ,e) ,b)))
          ((ptn $ptn)
           (alet vnm (gensym)
                 (if cn `(foreach-count (,vnm ,e ,(car cn))
                                        (pfront.with-format ,vnm ,ptn ,b))
                     `(foreach (,vnm ,e) (pfront.with-format ,vnm ,ptn ,b)))))))

       (collector `(pfront-collector (,addname ,getname) ,body))
       (withmacros `(with-macros ,ds ,e))
       (withmetadata `(ast-with-metadata ,m ,e))
       (vquote (list 'quote e))
       (qquote `(quasiquote ,e))
       (unquote (list 'unquote v))
       (def `(inblock-def ,v ,e))
       (defformat `(inblock-def-format ,f ,e))
       (let `(let ((,v ,e)) ,@rest))
       (var (if md `(inner.identmetadata ,v ,@md) v))
       (number v)
       (char ch)
       (string v)
       (true #t) (false #f)
       (symbol `(quote ,v))
       (lisp code)
       (macroext `(,id ,code))

       (pftry
        `(try ,e ,tp (fun (,nm) ,proc)))
       ))

    (xllpair DEEP
       ((p `(,nm ,v))
        (e `(*NEXT* ,v))))
    
    (lmacrodef DEEP
       ((def `(,nm ,v))))

    (mpattern DEEP
       ((cons (cons a b))
        (bindas `(,(Sm<< "$$AS:" (cadr id)) ,p ,@(cddr id)))
        (guard `(,(Sm<< "$$FFF") ,a (fun (,(cadr id)) ,b)))
        (list (normal-or-append
               (fun (a b) (cons a b))
               (fun (a b)
                 (ccerror `(APPEND-PATTERN-NOT-ALLOWED ,a ,b)))
               nil
               ps))
        (nil nil)
        (quote s)
        (alist `(,hd ,@(normal-or-append
                        (fun (a b) (cons a b))
                        (fun (a b)
                          (ccerror `(APPEND-PATTERN-NOT-ALLOWED ,a ,b)))
                        nil
                        args)))
        (binding (Sm<< "$" id))
        (mdbinding `(,(Sm<< "$$AAS:" (cadr id)) ,@(cddr id)))
        (eq (Sm<< "=" id))
        (any '$_)
        (number n)
        (string s)))

    (visitp DEEP ;; TODO
      (list v e))
    
    (visitptn DEEP
       ((many `(,@(vtp node t) (,@ps ,@el)))
        (manyfun (alet addargs (fun (ps)
                                 (foreach-map (p ps)
                                   (format p (nm ex)
                                           `(,nm (fun ,args ,ex)))))
                  `(,@(vtp node t)
                   (,@(addargs ps)
                    ,@(foreach-map (e el)
                        (p:match e
                          ((else $code) `(else (fun ,args ,code)))
                          ((else-deep $ps) `(else-deep ,(addargs ps)))))))))
        (forall `(,@(vtp node t) (forall ,e)))
        (single `(,@(vtp node t) ,e))))
    (visitelse DEEP
       ((velse `(else ,ee))
        (elsedeep `(else-deep (,@ps ,@es)))))
    ))

(function hlevel-check-entry (nm)
  (format nm (a b)
  (alet n (Sm<< a "_" b)
    (case n
      ((expr_bintop expr_bin1 expr_bin2 expr_start expr_preatom
        expr_end top_start top_middle) n)
      (else

       (ccerror `(WRONG-SYNTAX-ENTRY ,nm)))))))

(define hlevel-default-entries
  '((with-ignore Spaces)
    (rule keyword
          (seq (terminal keyword) (notp (terminal IdentRest)))
          (((ctoken keyword)) ()))
    (rule lexical (terminal lexical) (((ctoken lexic)) ()))))

(function hlevel-ast-pattern (args)
  (let loop ((a args))
    (p:match a
      (((dot $x) . $_) x)
      (((list . $ls) . $rest)
       `(,(loop ls) ,@(loop rest)))
      (($hd . $tl) `(,hd ,@(loop tl)))
      (() nil))))

(function _split_module_exports (es)
  (collector (exadd exget)
  (collector (imadd imget)
   (foreach (e es)
     (p:match e
       ((export $nm) (exadd nm))
       ((import $nm) (imadd nm))))
   (cons (exget) (imget)))))

(function _mkpattern (x)
  (p:match x
    ((var $id) (Sm<< "$" id))
    ((var $id . $md) `(,(Sm<< "$$AAS:" id) ,@md))
    (else (ccerror `(PATTERN ,x)))))

(function hlevel-compile (c)
  (hlevel:visit topexpr c
    (topexpr DEEP
      (
       (begin `(top-begin ,@es))

       (using `(using ,lst ,@es))
       (module
        `(n.module ,nm))
       (exemodule
        `(n.module ,nm exe))
       (defmodule
         (let* ((exs0 (_split_module_exports exs))
                (exports (car exs0))
                (imports (cdr exs0)))
           `(inner.module-using-export-push ,nm ,exports ,imports)))
       (endmodule
        `(inner.module-using-export-pop))

       (usemodule
        `(ctimex (begin (bootlib:push-module-env)
                        (bootlib:add-module-env (quote ,nms)))))
       (endusing
        `(top-begin
           (force-class-flush)
           (ctimex (bootlib:pop-module-env))))

       (topfunction
        `(top-begin
           (definition-metadata ,nm ,loc)
           (recfunction ,nm ,args ,body)
           ))
       (lispmacro
        (alet argslist
          (let loop ((a args))
            (p:match a
              (() nil)
              (((a $x) . $_) (_mkpattern x))
              (((i $x) . $rest) (cons (_mkpattern x) (loop rest)))))
          (with-syms (rnam)
           `(macro ,nm ,rnam
              (p:match ,rnam
                (,argslist ,body)
                (else (ccerror (list 'MACRO-FORMAT-ERROR))))))))
       (topdefine
        `(define ,nm ,val))
       (ast3
        (p:match opt
          ((recform)
           `(def:ast:new ,nm ,ps ,@defs))
          (else
           `(def:ast ,nm ,ps ,@defs))))
       (ast2
        (p:match opt
          ((recform)
           `(def:ast:new ,nm () ,@defs))
          (else
           `(def:ast ,nm () ,@defs))))
       (topsyntax
        (p:match c
          ((mpeg $cde $rst)
           `(top-begin
              (peg-minigrammar pfront ,(hlevel-check-entry nm)
                               ,bor
                               (,cde (() (action ,body)))
                               ,@hlevel-default-entries
                               ,@rst
                               )))))
       (othersyntax
        (p:match c
          ((mpeg $cde $rst)
           `(top-begin
              (peg-minigrammar ,synt
                               ,(Sm<< (car nm) "_" (cadr nm))
                               ,bor
                               (,cde (() (action ,body)))
                               ,@hlevel-default-entries
                               ,@rst
                               )))))
       (hlinclude `(hlevl-file ,s))
       (litinclude `(hlevl-lfile ,texnm ,s))
       (htmlinclude `(hlevl-lfile-html ,texnm ,s))
       (lispinclude `(include ,s))
       (topparser
        `(packrat-ast ,name ,bor ,@nodes))
       (expr e)
       (else
        (begin
          (writeline `(UNIMPLEMENTED: ,node))
          `(top-begin

             )
          ))))
    (astbody DEEP
      ((alt `(,nm (| ,@alts)))
       (addalt `(,(Sm<< "::" nm) (| ,@alts)))
       (struct `(,nm ,(hlevel-ast-pattern s)))
       (pphintvar `(varhint ,nd ,var ,hints))
       (pphintnd `(hint ,nd ,hints))
       ))
    (astalt DEEP
      ((withargs `(,nm ,@(hlevel-ast-pattern args)))
       (remove `(- ,nm))
       (single `(,nm))))
    (astbind DEEP
      ((bindmany (Sm<< "<*" tp ":" nm ">"))
       (bindopt (Sm<< "<*" tp ":" nm ">"))
       (bindone (Sm<< "<" tp ":" nm ">"))
       (dot node)
       (list node)
       ))
    (aststruct DEEP
      ((many args)
       (single b)))

    (expr _ (forall (hlevel-compile-expr (fixleft_outer node))))
    ))

(ctime (if (shashget (getfuncenv) 'pfront-macros-defined)
           `(top-begin )
           `(include "./backport.al")))

(function  pfront_expr (e) (hlevel-compile-expr e))
(function  pfront_top (e) (hlevel-compile e))
