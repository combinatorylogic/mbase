;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(Section "R6RS--style syntax-case")


;; TODO: MOVE TO LIBRARY
(function init-hash (ht lst)
  (use-hash (ht)
   (map-over lst (fmt (a b) (ht! a b)))))

;;
;; syntax-case/syntax-rules pattern matching/template expansion mechanism
;;

; Utility functions for handling ellipsis bindings
(module syntax-case-util
        (export compile-syntax-rules)
        (export compile-template)
        (export compile-syntax-case)

(function reg-names (mp nm a la)
  (use-hash (mp)
    (mp! nm la)
    (let loop ((l a))
      (p:match l
        (_ nil)
        ($$M (mp! l nm))
        (($a . $b) (loop a) (loop b))
        (else nil)))))

(function get-nm (mp a)
  (use-hash (mp)
    (let loop ((l a))
      (p:match l
        (_ nil)
        ($$M (alet x (mp> l) (if x x nil)))
        (($a . $b) (alet x (loop a) (if x x (loop b))))
        (else nil)))
    (mp> a)))

(function get-ptn (mp nm)
  (use-hash (mp)
    (mp> nm)))

(function reg-binding (mp nm)
  (use-hash (mp)
    (mp! (Sm<< ". " nm) #t)))

(function is-it-a-binding? (mp x)
  (use-hash (mp)
    (mp> (Sm<< ". " x))))

; Translate a syntax-rule pattern into an MBase p:match pattern
(function compile-pattern (capt mp p)
  (let loop ((x p))
    (p:match x
      (($a ... . $rest)
       (with-syms (nm)
         (alet la (loop a)
          (reg-names mp nm a la)
          `(,(Sm<< "$$RP:" nm) ,la ,@(loop rest)))))
      (($a . $b) (cons (loop a) (loop b)))
      (_ '$_)
      ($$M ; todo: check with capt
       (if (memq x capt) x
         (begin
           (reg-binding mp x)
           (Sm<< "$" x))))
      (() x)
      (else (ccerror `(PATTERN: ,x))))))

; Translate a syntax-rule template into a constructor code, using the bindings extracted
;  by a preceeding run of compile-pattern.

(define parse-~ (<r> "~" (p. *)))

(function compile-template-inner (mp add t)
 (use-hash (mp)
  (let loop ((x t))
    (p:match x
      (($a ... . $rest)
       (let* ((nm (get-nm mp a))
              (ptn (get-ptn mp nm))
              (mk-a (loop a)))
         (with-syms (idx)
           `(append (foreach-map (,idx ,nm)
                    (p:match ,idx (,ptn ,(loop a))))
                    ,(loop rest)))))
      (($a . $b) `(cons ,(loop a) ,(loop b)))
      ($$M (if (is-it-a-binding? mp x)
               x
               (if (p-success? (parse-~ (symbol->list x)))
                   (let* ((id (S<< "~. " x))
                          (tst (mp> id))
                          (t1 (if tst tst (with-syms (xx)
                                             (mp! id xx)
                                             xx))))
                     (add t1)
                     t1)
                   `(quote ,x))))
      (() 'nil)
      ))))

(function compile-template (mp t)
  (collector (add get)
    (let* ((inner (compile-template-inner mp add t))
           (g (get)))
      (if g
          `(with-syms ,g ,inner)
          inner))))

; Check the macro pattern head
(function compile-pattern-template (hdref capt p t)
  ; Check its head
  (if (not (eqv? '_ (car p)))
      (if (deref hdref)
          (if (not (eqv? (car p) (deref hdref))) (ccerror `(PATTERN: ,p)))
          (r! hdref (car p))))
  ; Compile pattern to p:match
  (with-hash (mp)
   (let* ((pm (compile-pattern capt mp (cdr p)))
          (tm (compile-template mp t)))
     `(,pm ,tm))))

; Wrapper compiler
(function compile-syntax-rules (capt rules)
  (let* ((hd (mkref nil))
         (patns
          (foreach-map (r rules)
            (format r (p t)
              (compile-pattern-template hd capt p t)))))
    (with-syms (arg)
     `(fun (,arg)
        (p:match ,arg
          ,@patns
          (else (ccerror (quote (RULES-SYNTAX-ERROR ,(car hd))))))
        ))))
; Syntax local macro - built specifically for a leftside pattern
(function preserve-mp (mp)
  (with-syms (tname arg)
    `(fun (,arg)
      (format ,arg (hd arg)
        (with-hash (,tname)
         (init-hash ,tname (quote ,(hashmap (fun (a b) (list a b)) mp)))
         (syntax-case-util:compile-template ,tname arg))))))

; Check the macro pattern head
(function compile-pattern-code (hdref capt p c)
  ; Check its head
  (if (not (eqv? '_ (car p)))
      (if (deref hdref)
          (if (not (eqv? (car p) (deref hdref))) (ccerror `(PATTERN: ,p)))
          (r! hdref (car p))))
  ; Compile pattern to p:match
  (with-hash (mp)
   (let* ((pm (compile-pattern capt mp (cdr p)))
          (hdef (preserve-mp mp)))
     `(,pm (with-macros ((syntax ,hdef))
               ,c)))))

; Wrapper compiler
(function compile-syntax-case (arg capt rules)
  (let* ((hd (mkref nil))
         (patns
          (foreach-map (r rules)
            (format r (p c)
               (compile-pattern-code hd capt p c)))))
    `(p:match ,arg
       ,@patns
       (else (ccerror (quote (RULES-SYNTAX-ERROR ,(car hd))))))
    ))

)

(using-modules (syntax-case-util)
; Macro interface to the syntax-rules functionality
(macro syntax-rules (capt . rules)
  ("An R5RS--alike syntax-rules macro transformer."
   "The only significant difference is the lack of hygiene, which is"
   "leveraged by the \verb|~name|--style templates that explicitly introduces"
   "new names."
   )
  (compile-syntax-rules capt rules))
; Macro interface to the syntax-rules functionality
(macro syntax-case (arg capt . rules)
  ("An R6RS-alike syntax-case implementation, with \verb|(syntax ...)| local"
   "macros to substitute syntactic templates."
   "There is the same way to deal with new bindings as in \verb|syntax-rules|."
   )
  (compile-syntax-case arg capt rules))

(macro define-syntax (nm trans)
  ("An R5RS--compatible define-syntax wrapper to be used with syntax-rules and such.")
  `(defmacro (quote ,(Sm<< nm)) (fun (arg) (,trans (cdr arg)))))
)


(unit-test-defn 2 (define-syntax xxxsn1
                    (syntax-rules ()
                       ((_ x ...) (list (list x ...))))))

(unit-test 2 (xxxsn1 1 2 3) ((1 2 3)))
