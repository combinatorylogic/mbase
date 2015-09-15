;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{Simple tail recursion handling}
;-
;- This stage should be applied after the lambda lifting pass.
;-

;= Helper function: create names for argument shadowing variables
(function cc:rename-args ( arglist expr )
  (with-hash (ht)
    (foreach (a arglist)
      (ht! a (Sm<< a "_arg")))
    (cc:mbcoreast:visit expr expr
      (expr DEEP
        ((Arg (alet tst (ht> id)
                (if tst `(Var ,tst) node)))
         (else node))))))

;= Convert a simple tail recursive function into a loop, if it is possible.
(function cc:convert-tail ( recname arglist expr )
  (let* ((yesitis (cons nil nil))
         (res
          (let loop ((e0 expr))
            (cc:mbcoreast:visit expr e0
              (expr _
                ((If (ast:mknode (iftr (loop iftr))
                                 (iffl (loop iffl))))
                 (TryBlock
                  (ast:mknode (body (loop body))))
                 (Begin
                  (ast:mknode (es (append
                                   (cuttail es)
                                   (list (loop (car (lasttail es))))))))
                 (SLet
                  (ast:mknode (body (loop body))))
                 (SLetRec
                  (ast:mknode (body (loop body))))
                 (App ;; The check itself
                  (p:match fn
                    (($$M:xx =recname)
                     (case xx
                       ((Glob Var Recref)
                        (begin
                          (set-car! yesitis #t)
                          `(SLet ,(foreach-map (a (czip arglist args))
                                    `(,(Sm<< (car a) "_arg_new") ,(cdr a)))
                             (Begin
                              ,@(foreach-map (a arglist args)
                                  `(XSet Var ,(Sm<< a "_arg")
                                         (Var ,(Sm<< a "_arg_new"))))
                              (GotoLabel ,recname)))))
                       (else node)))
                    (else node)))
                 (else node)))))))
    (if (car yesitis)
        `(SLet ,(foreach-map (a arglist)
                  `(,(Sm<< a "_arg") (Arg ,a)))
               (Begin
                (Label ,recname)
                ,(cc:rename-args arglist res)))
        expr)))

;= Interface function: convert only functions, ignore other expression types.
(function cc:convert-tail-helper ( name expr )
  (p:match expr
    ((Fun $rn $args $body)
     `(Fun ,rn ,args ,(cc:convert-tail name args body)))
    (else expr)))
