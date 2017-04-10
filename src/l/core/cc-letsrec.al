;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(function cc:capture-recursion (expr)
  (<> expr
   (ast:revisit recloop () cc:mbcoreast expr
     ((SLetRec node)
      (Recref  `(Var ,id)))
     ())))

(function cc:make-patchers (defs lifted)
  (with-hash (ht ht1)
  (awith (ls (foreach-mappend (d defs)
               (p:match d
                 (($nm CCAPTURE . $_) (list nm))
                 (nil))))
    (foreach (l ls) (ht1! l l))
    (foreach (l lifted)
      (p:match l
        (($tp $nm . $rest) (ht! nm l))))
    (foreach-mappend (d defs)
       (p:match d
         (($nm CCAPTURE $vl $env)
          (alet dfn (hashget ht nm)
            (p:match dfn
              ((Closure $cloname $clorename $clenv $rest)
               (let* ((iclenv (mapi (fun (i v) (list i v)) clenv))
                      (captch (filter
                               (fmt (i v)
                                    (hashget ht1 v))
                               iclenv)))
                 (if captch `((PatchClosure ,nm ,cloname ,@captch)) nil)))
              (else (cc:comperror `(CC0:IMPOSSIBLE! ,env ,dfn))))))
         (else nil))))))

(function cc:fix-closuremaker (ht expr)
  (use-hash (ht)
    (cc:mbcoreast:visit expr expr
       (expr DEEP ((Var (if (ht> id) '(Nil) node))
                   (else node))))))

;= [[SLetRec]] is the most complex thing here. Recursive definitions are
;= checked against the tweaked list of bound variables, and that ones which are
;= definitely going to be compiled as closures are backpatched to reflect each other
;= exact values. The trick here is that a simple recursive function should not capture
;= itself in a closure environment, but will rather reference to self in a normal way,
;= but if a function is a closure, it will capture all the environment and a reference
;= to self, added later with a closure patcher.

(function cc:doletsrec (liftloop bound rrec stickyname get defs body)
  (let*
            ((alst (map car defs))
             (nbound (append alst bound))
;= [[ndefs0]] is a list of annotated definitions --- simple references and
;= captured environment closures.
             (ndefs0
              (map-over defs
                (fmt (nm vl)
                  (let* ((vars (cc:count-refs vl))
                         (nbound0 (append
                                   (filter
                                    (fun (x)
                                      (not (eqv? x nm)))
                                    alst)
                                   bound))
                         (env (cc:intersection nbound0 vars)))
                    (if (null? env) ; simple recursion
                        `(,nm SIMPLE ,vl)
                                    ; otherwise: closure
                        `(,nm CCAPTURE
                              ,(cc:capture-recursion vl)
                              ,env
                              ))))))
             (ndefs01 (with-hash (ht)
                       (foreach (i ndefs0) (ht! (car i) #t)) ht))
;= [[ndefs1]] is a compiled list of definitions, taking a correct list of
;= bound variables into account to make sure they are going into a closure environment.
             (ndefs1
              (map-over ndefs0
                 (fmt (nm tp vl)
                   (alet nbound0
                          (case tp
                            ((SIMPLE)
                             (append
                              (filter
                               (fun (x)
                                 (not (eqv? x nm)))
                               alst)
                              bound))
                            ((CCAPTURE)
                             (append alst bound))
                            )
                     `(,nm ,(cc:fix-closuremaker ndefs01
                                (liftloop vl nbound0 nm stickyname)))))))
             )
;= And finally a patched [[SLetRec]] code is emitted:
            `(SLetRec ,ndefs1
                      (Begin
                       ,@(cc:make-patchers ndefs0 (get))
                       ,(liftloop body nbound nil stickyname)))
            ))
