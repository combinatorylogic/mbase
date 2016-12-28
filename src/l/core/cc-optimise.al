;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(function cc:simple-thing? ( mt defexpr )
  (use-hash (mt)
    (if (mt> (car defexpr)) nil
        (case (car (cadr defexpr))
          ((Str Num FNum Chr Bool Symbol Var Nil Arg Glob Recref Funref Clenv) #t)
          (else nil)))))

(function cc:mutables ( expr )
  (with-hash (mb)
    (cc:mbcoreast:iter expr expr
        (expr DEEP
           ((XSet (mb! nm #t))
            (else nil))))
    mb))

(function cc:replace-things (things expr)
  (with-hash (th)
     (iter (fmt (nm vl) (th! nm vl)) things)
     (alet fix (fun (v)
                 (alet tst (th> (cadr v))
                   (if tst tst v)))
       (cc:mbcoreast:visit expr expr
           (expr DEEP
             ((Arg (fix node))
              (Var (fix node))
              (Funref (fix node))
              (Recref (fix node))
              (Clenv (fix node))
              (else node)))))))

(function cc:estimate ( expr )
  (let ((res (noconst (cons 0 nil))))
    (cc:mbcoreast:iter expr expr
       (expr DEEP (forall (set-car! res (+ (car res) 1)))))
    (car res)))

(function cc:notreferenced ( expr varn )
  (let ((res (mkref)))
    (cc:mbcoreast:iter expr expr
       (expr DEEP
          ((Recref (if (eqv? varn id) (set-car! res #t)))
           (Var (if (eqv? varn id) (set-car! res #t)))
           (else nil))))
    (not (car res))))

(function cc:arg->var (args expr)
  (with-hash (ah)
    (foreach (a args) (ah! a a))
    (cc:mbcoreast:visit expr expr
       (expr DEEP
         ((Arg (if (ah> id) `(Var ,id) node))
          (else node))))))

(recfunction cc:optimise ( expr )
  (cc:mbcoreast:visit expr expr
    (expr DEEP
      ((Begin
        (p:match es
          (($one) one)
          (else node)))
       (App
        (p:match fn
          ((Fun $rn $fnargs $body)
           (if (and
                (< (length fnargs) 12)
                (< (cc:estimate node) 2000)
                (or (not rn) (cc:notreferenced body rn)))
               (cc:optimise
                `(SLet ,(zip fnargs args)
                       ,(cc:arg->var fnargs body)))
               node))
          (else node)))
       (Cons
        (p:match (list a b)
          (((Nil) (Nil))
           `(Cons0))
          (($a (Nil))
           `(Cons1 ,a))
          (else node)))
       (If
        (p:match e
          ((NullP $a) `(IfNull ,a ,iftr ,iffl))
          ((PairP $a) `(IfPair ,a ,iftr ,iffl))
          ((Not (NullP $a)) `(IfNull ,a ,iffl ,iftr))
          ((Not (PairP $a)) `(IfPair ,a ,iffl ,iftr))
          ((Not (Eqv $a $b))
                    `(IfEqv ,a ,b ,iffl ,iftr))
          ((Eqv $a $b) `(IfEqv ,a ,b ,iftr ,iffl))
          ((Not $a) `(If ,a ,iffl ,iftr))
          (else node)))
       (SLet
        (p:match (list defs body)
          (((($name $value))
            (Var =name)) value)
          (else
           (let* ((mutabs (cc:mutables body))
                  (sthings (filter (cut cc:simple-thing? mutabs <>) defs)))
             (if sthings ;; there are some
                 (let ((nw (filter (fun (x) (not (cc:simple-thing? mutabs x))) defs))
                       (bdy (cc:replace-things sthings body)))
                   (cc:optimise
                    (if nw
                        `(SLet ,nw ,bdy)
                        bdy)))
                 node)))))
       (else node)))))

