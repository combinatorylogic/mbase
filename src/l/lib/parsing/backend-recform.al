
;; ASTs are not persistently stored in the binaries
;(include "../../core/ast2-ast.al")

;; Matching rules:
;;   @x : @y => x = y
;;   @x : y,z, ... => x = [y,z,...]
;;   x : y => x = y
;;   x : F(...) => x = F(...)


;; This function only deals with one level of node constructur,
;;  nested constructors must be dealt with externally.

;; constr is a cargs list; astfmt is an ast pattern
(function peg-constr-match-args (constr astfmt)
  ;; Build a picker
  (collector (add-match get-matches)
  (let* ((match-entry
          (fun (ref name l)
            (p:match l
              ((list set $v $x) (add-match `(,ref ,name ,x))) ;; TODO: ?!?
              ((set $v $x) (add-match `(,ref ,name ,x)))
              ((list var $x) (add-match `(,ref ,name (var ,x)))) ;; TODO: ?!?
              ((var $x) (add-match `(,ref ,name (var ,x))))
              ((list $x) ;; trailing nil
               (add-match `(,ref ,name (nil))))
              (((append $v $x))
               (add-match `(,ref ,name ,x)))
              (else (ccerror `(PEG-CONSTR-MATCH ,ref ,name ,l))))))
          (match-lists
           (fun (vs0 l0)
             (let loop ((vs vs0)
                        (l  l0))
               (p:match (list vs l)
                 (( ((@ $f) . $r) ;; vs
                    ((append $_ $v) . $lr)) ;; l
                  (begin (f v)
                         (loop r lr)))
                 (( ((@ $f) . $r) ;; vs
                    $lr) ;; l
                  (begin (f `(list ,@lr))
                         (if r (ccerror `(PEG-CONSTR-MATCH-REST ,r)))))
                 (( ($f . $r)
                    ((append $_ $v) . $lr))
                  (f v))

                 (( ($f . $r) ;; vs
                    ($lf . $lr)) ;; l
                  (begin (f lf)
                         (loop r lr)))
                 (else nil))))))

    (let* ((picker
            (astlang:visit pattern astfmt
              (pattern DEEP
                ((tuple (fun (l)
                          (match-lists vs l)))
                 (append `(@ ,p))
                 (entry
                  (if (eqv? tp 'mul)
                      (fun (l)
                        (match-lists `((@ ,(fun (l)
                                             (match-entry ref name l))))
                                     l))
                      (fun (l)
                        (match-entry ref name l))))
                 (nil (fun (l) nil)))))))
      (picker constr)
      (get-matches)
      ))))


(function peg-constr-compile-recform-loop ( constr target-node target-ast )
  (let* ((asrc (ast2:default-ifun target-ast))
         (asth (ast-make-cache asrc)))
    (let loop ((c constr) (nd target-node))
      (let* ((dovarnode
              (fun (nd cname ars)
                (let* ((frmt (ast-get-variant-pattern asth nd cname))
                       (ms (peg-constr-match-args ars frmt))
                       (args2
                        (let iloop ((is ms))
                          (foreach-map (m is)
                            (format m (tp nm v)
                              (p:match v
                                ((list . $ls)
                                 `(,nm (list ,@(foreach-map (r (iloop ls))
                                                 (cadr r)))))
                                ((nil)
                                 `(,nm nil))
                                (else
                                 `(,nm ,(loop v tp)))))))))
                  `(ast2:vctr ,nd ,cname ,@args2)))))
        (packrat:visit code c
          (code _
            ((constr (dovarnode nd cname ars))
             (dconstr (dovarnode nname cname ars))
             (var name)
             (const `(quote ,s))

             (fcall `(,(Sm<< "peg-function-" fname)
                      ,@(foreach-map (a ars)
                          (loop a target-node))))

             (else (ccerror `(PEG-CONSTR-RECFORM-UNSUPPORTED ,node)))
             )))
        ))))

(function peg-constr-get-target (a b)
  (p:match a
    (((target $nm)) nm) ;; TODO!
    (else b)))

(function __peg-stream-delta (s1 s2)
    `(,(StreamEntry.idx s1) ,(StreamEntry.idx s2))
    )

(macro peg-constr-get-loc ()
  `(cons (cons 'LOC (__peg-stream-delta saved (deref source)))
         nil))

(macro peg-constr-compile-recform-inner (qconstr qsrcfmt qdtype target-ast
                                                 target-node)
  (p:match target-node
    (else
     (let* ((ret (peg-constr-compile-recform-loop (cadr qconstr)
                   (peg-constr-get-target (cadr qdtype) target-node)
                   target-ast )))
       (with-syms (loc)
         `(with-macros ((ast-current-metadata (fun (_) (quote ,loc))))
            (let ((,loc (peg-constr-get-loc)))
              ,ret)))))))


