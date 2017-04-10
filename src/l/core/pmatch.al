;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; An alternative pattern matching (to be used in a peephole optimizer).

(Section "Advanced pattern matching")


;; 1. Parsing the symbolic patterns.

(notaruntime
(define pm:ptn-parse-p
   (<r> (
         (((_ "[") ((! "]") +*) (_ "]")) -> (fun (x) `(S ,(list->symbol x))))
       |
         (((_ "$$") ((p.ucalpha +*)  :-> list->symbol) (_ ":") ((p. *) :-> list->symbol))
          -> (fmt (a b) `($$ ,a ,b)))
       |
         (((_ "$$") ((p.ucalpha +*) :-> list->symbol)) -> (fmt (a) `($$1 ,a)))
       |
         (((_ "$") ((p. *) :-> list->symbol)) -> (fmt (a) `($ ,a)))
       |
         (((_ "=") ((p. *) :-> list->symbol)) -> (fmt (a) `(= ,a)))
       |
         ((p. *) -> (fun (x) `(S ,(list->symbol x))))
       )))
)

(notaruntime
(function pm:ptn-parse (sym)
  (let* ((l (string->list (symbol->string sym)))
         (v (p-result (pm:ptn-parse-p l))))
    (fccase v
      ((S) (x) `(match-symbol ,x))
      (($$ $$1) (a . b)
       (case a
         ((AAS) `(bind-any-as ,@b))
         ((AS)  `(match-as ,@b))
         ((XXX)  `(match-anybutnext ,@b))
         ((FFF)  `(match-guard ,@b))
         ((FF)  `(match-post-function ,@b))
         ((F)  `(match-function ,@b))
         ((RP) `(match-many ,@b))
         ((R)  `(match-range ,@b))
         ((L)  `(match-any-list ,@b))
         ((N)  `(match-any-number ,@b))
         ((S)  `(match-any-string ,@b))
         ((M)  `(match-any-symbol ,@b))))
      (($) (x) `(match-anything ,x))
      ((=) (x) `(match-bindeq ,x))
      (else (ccerror `(pm:error ,sym))))))

);notaruntime
;; 2. Process the pattern.

(notaruntime
(recfunction pm:ptn-process (ptn)
  (cond
    ((null? ptn) `(match-null))
    ((symbol? ptn) (pm:ptn-parse ptn))
    ((and (list? ptn) (symbol? (car ptn)))
     (let ((ss (pm:ptn-parse (car ptn))))
       (fccase ss
         ((match-anybutnext) rest
          `(match-anybutnext
            ,(if (null? rest) nil (car rest))
            ,(pm:ptn-process (cdr ptn))))
         ((match-function match-range) rest
          `(,(car ss) ,(if (null? rest) nil (car rest)) ,@(cdr ptn)))
         ((bind-any-as) rest
          `(bind-any-as ,(car rest)
                        ,@(cdr ptn) ;; metadata
                         ))
         ((match-as) rest
          (if (null? rest) (pm:ptn-process (cadr ptn))
              `(match-as ,(car rest)
                         ,(pm:ptn-process (cadr ptn))
                         ,@(cddr ptn) ;; metadata
                         )))
         ((match-many) rest
          `(match-many ,(if rest (car rest) nil)
                       ,(pm:ptn-process (cadr ptn))
                       ,(pm:ptn-process (cddr ptn))
                       ))
         ((match-guard) rest
          `(match-guard ,(pm:ptn-process (cadr ptn)) ,(caddr ptn)))
         ((match-post-function) rest
          `(match-post-function ,(if (null? rest) nil (car rest))
                                ,(cadr ptn) ,(pm:ptn-process (cddr ptn))))
         (else `(match-cons ,ss ,(pm:ptn-process (cdr ptn)))))))
    ((list? ptn)
     `(match-cons ,(pm:ptn-process (car ptn)) ,(pm:ptn-process (cdr ptn))))
    ((string? ptn)
     `(match-string ,ptn))
    ((number? ptn)
     `(match-number ,ptn))
    (else `(match-any))))

)


;; 2. Unroll the processed pattern into the matching code.

(define pm:ptn-failed 'PM:PTN-FAILED-WILL-CONSIDER-THE-NEXT-ONE) ;;; FAILURE INDICATOR


(macro pm:ptn-try (cnd body)
   `(if ,cnd ,body
             pm:ptn-failed))

(macro pm:ptn-failed-m ()
    'pm:ptn-failed)

(notaruntime
(recfunction pm:ptn-unroll (ptn pas body)
   (fccase ptn
      ((match-cons) (hd tl)
         (with-syms (ch ct)
          `(pm:ptn-try (list? ,pas)
            (let ((,ch (car ,pas))
                  (,ct (cdr ,pas)))
               ,(pm:ptn-unroll hd ch (pm:ptn-unroll tl ct body)) ))))
      ((match-anybutnext) (bnd ptn2)
       (let* ((bbnd (if (null? bnd) (gensym) bnd))
              (lop (gensym))
              (larg (gensym))
              (ltst (gensym)))
         `(let ,lop ((,larg ,pas) (,bbnd nil))
            (let ((,ltst (with-macros ((pm:ptn-failed-m (fun (_)
                                           'pm:ptn-failed))) ,(pm:ptn-unroll ptn2 larg body))))
              (if (eqv? pm:ptn-failed ,ltst)
                  (if (or (null? ,larg)
                          (not (list? ,larg)))
                      (pm:ptn-failed-m)
                      (,lop (cdr ,larg) (append ,bbnd (list (car ,larg)))))
                  ,ltst
                  )))))
      ((match-as) (bnd pattern . metadata)
       `(let-metadata (((,bnd ,@metadata) ,pas)) ,(pm:ptn-unroll pattern bnd body)))
      ((match-many) (bnd ptn2 ptnr)
       (let* ((bbnd (if (null? bnd) (gensym) bnd))
              (lop (gensym))
              (larg (gensym))
              (clarg (gensym))
              (ltst (gensym)))
         `(let ,lop ((,larg ,pas) (,bbnd nil))
            (let ((,ltst (if (and ,larg (list? ,larg))
                              (let ((,clarg (car ,larg)))
                                (with-macros ((pm:ptn-failed-m (fun (_)
                                                'pm:ptn-failed)))
                                             ,(pm:ptn-unroll ptn2 clarg '(quote x))))
                              pm:ptn-failed
                              )))
              (if (eqv? pm:ptn-failed ,ltst)
                  ,(pm:ptn-unroll ptnr larg body)
                  (if (or (null? ,larg)
                          (not (list? ,larg)))
                      (pm:ptn-failed-m)
                      (,lop (cdr ,larg) (append ,bbnd (list (car ,larg)))))
                  )))))
      ((match-function) (bnd fn)
         `(pm:ptn-try (,fn ,pas) ,(if bnd `(let ((,bnd ,pas)) ,body) body)))
      ((match-guard) (ptn fn)
        (pm:ptn-unroll ptn pas `(pm:ptn-try (,fn ,pas) ,body)))
      ((match-post-function) (bnd fn ptn2)
        (with-syms (sss)
          `(let (,@(if (null? bnd) nil `((,bnd ,pas))) (,sss (,fn ,pas))) ,(pm:ptn-unroll ptn2 sss body))))
      ((match-number) (nm)
         `(pm:ptn-try (and (number? ,pas) (eq? ,pas ,nm)) ,body))
      ((match-any) _
         body)
      ((match-bindeq) (vr)
       `(pm:ptn-try (eq? ,pas ,vr) ,body))
      ((match-string) (str)
         `(pm:ptn-try (and (string? ,pas) (eq? ,pas ,str)) ,body))
      ((match-symbol) (sym)
         `(pm:ptn-try (eqv? ,pas (quote ,sym)) ,body))
      ((match-null) _
         `(pm:ptn-try (null? ,pas) ,body))
      ((bind-any-as) (bnd . md)
       `(let-metadata (((,bnd ,@md) ,pas)) ,body))
      ((match-anything) (bnd)
         `(let ((,bnd ,pas)) ,body))
      ((match-any-symbol) x
         `(pm:ptn-try (symbol? ,pas) ,(if (null? x) body `(let ((,@x ,pas)) ,body))))
      ((match-any-list) x
         `(pm:ptn-try (list? ,pas) ,(if (null? x) body `(let ((,@x ,pas)) ,body))))
      ((match-any-string) x
         `(pm:ptn-try (string? ,pas) ,(if (null? x) body `(let ((,@x ,pas)) ,body))))
      ((match-any-number) x
         `(pm:ptn-try (number? ,pas) ,(if (null? x) body `(let ((,@x ,pas)) ,body))))
      ((match-range) (bnd . sms)
         `(pm:ptn-try (and (symbol? ,pas) (or ,@(foreach-map (s sms) `(eqv? ,pas ,s))))
            ,(if (null? bnd) body `(let ((,bnd ,pas)) ,body))))
      (else (ccerror `(pm:unroll ,ptn)))))
)

;; 3. Wrap it into the macro.

(macro p:match (val . ptns)
   ("The most generic form of pattern matching.[br]"
    "Pattern language is following:[br]"
    "[["
    " <pattern>:"
    "   $<ident>  - binds anything to the given identifier"
    " | <symbol>  - matches the symbol value"
    " | <string>  - matches the string value"
    " | <number>  - matches the integer number value"
    " | =<ident>  - checks if the value equals to a given variable value"
    " | $$L[:<ident>] - matches any list"
    " | $$N[:<ident>] - matches any number"
    " | $$S[:<ident>] - matches any string"
    " | $$M[:<ident>] - matches any symbol"
    " | ($$AS:<ident> . <pattern>) - match a pattern and bind a value to a name"
    " | ($$F[:<ident>] <fun(x)>) - checks if <expr> applied to this node gives #t"
    " | ($$R[:<ident>] . <symbol>*) - matches any of the given symbols"
    " | ($$XXX[:<ident>] . <pattern>) - any number of list elements before"
    "          pattern is matched"
    " | ($$FFF <pattern> <fun(x)>) - applies a pattern and additionally applies a guard function"
    " | ($$FF[:<ident>] <fun(x)> . <pattern>) - checks if <fun> is not nil, and"
    "              applies pattern to a function value."
    "              This feature is a tribute to Don Syme's banana brackets."
    " | (<pattern> . <pattern>) - matches a cons cell, applies patterns"
    "                             to its contents"
    "]]"
    ""
    "  See [Pattern matching] documentation section for details."
    )
   (with-syms (top res)
     `(let ((,top ,val))
        ,(let loop ((ps ptns))
           (cond
              ((null? ps) 'nil)
              ((eqv? 'else (caar ps)) (cadar ps))
              (else `(let ((,res
                            ,(pm:ptn-unroll
                              (pm:ptn-process (caar ps))
                              top `(begin ,@(cdar ps)))))
                       (if (eqv? ,res pm:ptn-failed)
                          ,(loop (cdr ps))
                          ,res))))))))


;;; Unit tests for pattern matching:

(unit-test 1 (p:match '(a b c) (($x $y $z) (list z x y))) (c a b))

(unit-test 1 (p:match '(a b a) (($x $y =x) (list x y))) (a b))

(unit-test 1 (p:match '(a b c) (($x $y =x) (list x y)) (else 'nop)) nop)

(unit-test 1 (p:match '(x (y z)) (($a ($$AS:b (y $c))) (list c b a)))
           (z (y z) x))

;(unit-test 1 (p:match '(q 1 2 3) ((($$R:a x y z q v w) . $b)
;                                  (list b a)))
;           ((1 2 3) q))

;(unit-test 1 (p:match '(v 1 2 3) ((($$R:a x y z q v w) . $b)
;                                  (list b a)))
;           ((1 2 3) q))

;(unit-test 1 (p:match '(m 1 2 3) ((($$R:a x y z q v w) . $b)
;                                  (list b a)))
;           ((1 2 3) q))

;(unit-test-defn 3 (function hugelist (n)
;                    (collector (add get)
;                       (for (i 0 n) (add i))
;                       (get))))

;(unit-test 3 (let bigloop ((fn I) (l (hugelist 1000000)))
;               (p:match l
;                 (($hd . $tl) (begin
;                                (fn hd) (bigloop fn tl)))
;                 (() 'DONE))) DONE)


