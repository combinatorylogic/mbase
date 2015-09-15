;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(Section "LL(1) parsing")

;;; Part I. Parsing engine.

;;
;; NODE (state) FORMAT:

;; ( <variant>* ) | (T <snum> <code>)
;; <variant>:
;;  (S <tk> <nxt>)
;;  (C <dwn> <nxt>)
;; <nxt>: number or _


(macro fsm-get-state (fsm n)
  `(ageto ,fsm ,n))


(recfunction tryfirst (n l)
   (if (null? l) nil
      (if (< n 1) nil
          (cons (car l) (tryfirst (- n 1) (cdr l))))))




(function fsm-next-token (tks)
  (if (null? tks) '(STOP () . ())
      (begin
        (p-lookahead-a-bit tks) ;; use the same "lazy list" unrolling thing as we did for the lexing stuff
        (car tks))))

(function fsm-rest (tks)
  (if (null? tks) tks (cdr tks)))

(define tokentag (Sm<< " *token* "))
(define brokentag (Sm<< " *broken* "))

(function broken? (v)
  (and (list? v)
       (eqv? brokentag (car v))))

(function fsm-token-value (tk)
  (p:match tk
    ((=tokentag $tv . $rest)
     (if rest (car rest) nil))
    ((=brokentag . $_) nil)
    (else tk)))

(function fsm-marktoken (tk) (cons tokentag tk))
(function fsm-markbroken (tk)
  (format tk ((nm) . rst)
    `(,brokentag ,nm ,@rst)))

(function splitlistrev (i ls)
  (let loop ((n 0) (l ls) (rv nil) (fak nil))
    (if (< n i)
        (let ((vv (car l)))
          (if (broken? vv)
              (loop n (cdr l) rv (cons (cons tokentag (cdr vv)) fak))
              (loop (+ n 1) (cdr l) (cons vv rv) fak)))
        (list rv l fak))))

(function fsm-raise (state tokens)
  (r_raise (new t_MBaseException (object
    (if (or (null? tokens) (null? (cddr (car tokens))))
      `(PARSE-ERROR ,state ,(map (fun (x) (if (cdr x) `(,(car x) ,(cadr x)) x))
                                 (tryfirst 100 tokens)))
      `(PARSE-ERROR-AT ,(genposition (car tokens)) ,(list->string (tryfirst 100 (caddr (car tokens)))))
     )))))

(function fsm-mergepoints (sn s v)
  (if (null? (car v)) v
      (cons (append  (list (cons sn s)) (car v)) (cdr v))))

(function fsm-select (fsm state staten tk)
  (let ((tt (car tk)))
   (if (list? tt)
     (cons nil `(WSP ,staten)) ;R
     (let loop ((sn staten) (s state) (nrec (list staten)))
      (if (null? s) nil ;R
         (let* ((ss (car s)) (css (car ss)) (cdss (cadr ss)))
            (case css
              ((C)
               (if (memq cdss nrec)
                   (fsm-mergepoints sn s (loop sn (cdr s) nrec)) ;R
                   (let ((ttt (loop cdss (fsm-get-state fsm cdss)
                                    (cons cdss nrec))))
                     (if (null? ttt) (loop sn (cdr s) nrec)
                         (if (null? (cdr s))
                             (cons nil ss) ;R
                             (cons (list (cons sn (cdr s))) ss) ;R
                             )))))
              ((T) (cons nil ss)) ;R
              (else
               (if (eqv? cdss tt)
                   (cons nil ss) ;R
                   (loop sn (cdr s) nrec))))))))))

(recfunction fsm-parse (parsenv fsm staten0 istack rstack tokens btstack err)
 (let ((state (if (number? staten0) (fsm-get-state fsm staten0)
                 (if (number? (car staten0))
                     (cdr staten0)
                     staten0)))
       (staten (if (number? staten0) staten0
                   (if (number? (car staten0)) (car staten0) -1))))
 (if (eqv? 'T (car state))
  (format state (_ sn cd)
       (letf (( (slc nstk fakstk) (splitlistrev sn rstack)))
         (let* ((reslt (cd (cons slc (cons fakstk parsenv))
                           (map fsm-token-value slc))))
             (if (null? istack)
                 (cons reslt tokens)
                 (fsm-parse parsenv
                            fsm (car istack)
                            (cdr istack)
                            (cons reslt nstk)
                            tokens
                            btstack
                            err
                            )))))
  (let* ((tk (fsm-next-token tokens))
         (v (fsm-select fsm state staten tk)))
    (if (not v)
        (if (null? btstack)
            (begin
              (if err
                  (fsm-raise (car err) (cdr err))
                  (fsm-raise (list staten state (fsm-get-state fsm staten))
                             tokens))
              )
            (btstack (if err err (cons (list staten state
                                             (fsm-get-state fsm staten))
                                       tokens))))
        (alet newbs
          (if (null? (car v)) btstack
              (let iloop ((x (car v)) (bts btstack))
                (if (null? x) bts
                    (iloop (cdr x)
                           (fun (nerr)
                             (fsm-parse parsenv fsm (car x) istack
                                        rstack tokens bts nerr))))))
        (fccase (cdr v)
         ((S)
          (_ n)
            (fsm-parse parsenv
                       fsm n
                       istack
                       (cons (fsm-marktoken tk) rstack)
                       (fsm-rest tokens)
                       newbs
                       err
                       ))
         ((C)
          (d n)
          (fsm-parse parsenv
                     fsm d
                     (cons n istack)
                     rstack
                     tokens
                     newbs
                     err
                     ))
         ((WSP) (st)
          (fsm-parse parsenv
                     fsm st istack (cons (fsm-markbroken tk) rstack)
                     (fsm-rest tokens)
                     newbs
                     err
                     ))
         ((T) _ (fsm-parse parsenv fsm (cdr v) istack rstack tokens
                           newbs
                           err
                           ))
  )))))))

;;; Part II. BNF->FSM compiler.

; trie format:
; (S <node> <tree>)
; (R (<node> <tree>)*)
; (T <node>|<nil> (<n> <code>))
; (XT <n> <code>)

(notaruntime

(function fsm-add (fsm rev v)
  (let ((cnm (hashget rev '*COUNTER*)))
      (hashput rev '*COUNTER* (+ cnm 1))
      (hashput fsm cnm v)
      cnm))

(function list-split (fn ls)
  (let loop ((l ls)
             (a nil)
             (b nil))
    (if (null? l) (list (reverse a) (reverse b))
      (if (fn (car l)) (loop (cdr l) (cons (car l) a) b)
                       (loop (cdr l) a (cons (car l) b))))))

(recfunction bnf-to-trivtree (rst term)
  (if (null? rst)
      `(XT ,@term)
      `(S ,(car rst) ,(bnf-to-trivtree (cdr rst) term)))
)

(recfunction bnf-to-tree (ndef)
  (cond
   ((null? (cdr ndef))
    (format (car ndef) ((nd . rst) term)
       `(S ,nd ,(bnf-to-trivtree rst term)) ))
   (else
    (let ((groups
            (let loop ((nds ndef)
                       (g nil))
              (if (null? nds) g
                (let* ((nd0 (caar nds))
                       (nd (if (null? nd0) nil (car nd0))))
                  (letf (( (l r)
                           (if (null? nd) (list (list (car nds)) (cdr nds))
                            (list-split (fun (x)
                                            (and (not (null? (car x)))
                                                 (eqv? (caar x) nd)))
                                       nds) )))
                    (loop r
                          (cons l g))))))))
      `(R ,@(map-over (reverse groups)
              (fun (grp)
                 `(,(let ((cc (caar grp))) (if (null? cc) cc (car cc)))
                   ,(if (null? (cdr grp))
                        (if (null? (caar grp))
                            (format (car grp) (_ term)
                                    (bnf-to-trivtree nil term))
                            (format (car grp) ((_ . rst) term)
                                    (bnf-to-trivtree rst term)))
                        (bnf-to-tree (map-over grp
                           (fmt ((n1 . rst) term)
                              `(,rst ,term)))))))))
   ))))


(function bnf-emit-nodes (ndn fsm rev ndl)
   (let ((v (map-over ndl
              (fmt (nd nx)
                (if (null? nd) ; terminal-1
                    (begin
                       (hashput rev '*TCOUNTER*
                          (+ 1 (hashget rev '*TCOUNTER*)))
                       `(T ,@nx))
                    (let ((ndv (hashget rev nd))
                          (nnx (if (list? nx)
                                   (begin
                                      (hashput rev '*TCOUNTER*
                                         (+ 1 (hashget rev '*TCOUNTER*)))
                                      (fsm-add fsm rev `(T ,@nx))
                                   )
                                   nx)))
                       (if (null? ndv) ; atomic token
                           `(S ,nd ,nnx)
                           `(C ,ndv ,nnx))))))))
     (if (null? ndn)
         (fsm-add fsm rev v)
         (let ((nn (hashget rev ndn)))
             (hashput fsm nn v)
             nn))))

(function bnf-emit-terminal (ndn fsm rev nd nm cd)
   (hashput rev '*TCOUNTER*
         (+ 1 (hashget rev '*TCOUNTER*)))
   (let ((tnum (fsm-add fsm rev `(T ,nm ,cd))))
      (bnf-emit-nodes ndn fsm rev (list (list nd tnum)))))


(recfunction bnf-compile-0 (fsm rev nde)
  (format nde (ndname . ndef)
    (let ((tree (bnf-to-tree ndef)))
        (let loop ((tr tree) (ndn ndname))
             (fccase tr
               ((S) (nod trest)
                  (let ((nx (loop trest nil)))
                     (bnf-emit-nodes ndn fsm rev (list (list nod nx) ))))
               ((R) stree
                  (bnf-emit-nodes ndn fsm rev (map-over stree
                                               (fmt (nd trx)
                                                   (list nd (loop trx nil))))))
               ((T) (nd (nm cd))
                  (bnf-emit-terminal ndn fsm rev nd nm cd))
               ((XT) (nm cd) ;; special case - must be passed to bnf-emit-nodes
                  `(,nm ,cd))
               )))))

(function bnf-compile-1 (ndes)
   (let ((fsm (mkhash))
         (rev (mkhash)))
     (hashput rev '*COUNTER* 0)
     (hashput rev '*TCOUNTER* 0)
     (iter-over ndes
       (fmt (nm . _)
          (let ((cn (hashget rev '*COUNTER*)))
             (hashput rev '*COUNTER* (+ cn 1))
             (hashput rev nm cn))))
     (foreach (nd ndes)
       (bnf-compile-0 fsm rev nd))
     (if (shashget (getfuncenv) 'compiler-fsm-dots)
       (let ((cnt (hashget rev '*COUNTER*))
             (tcnt (hashget rev '*TCOUNTER*)))
        (print (buildstring "[BNF: " (length ndes) "/"
                            (- cnt tcnt) " nodes, "
                            tcnt " terminals]"))))
     (cons fsm rev)))
)

(function fsm-mk-parser (fsm entrynum)
  (fun (parsenv)
   (fun (tokens)
     (fsm-parse parsenv fsm entrynum nil nil tokens nil nil))))

(notaruntime
(define bnf-node-parser
  (let* ((x0 (<r> (((p. ((! ":") *)) :-> list->symbol )
                   (?? (_ ":") (((! ":") +*) :-> list->symbol))))))
    (fun (nd) (p-result (x0 (string->list (symbol->string nd)))))))

(recfunction bnf-compile-2 (bnf)
  (map-over bnf
    (fmt (nnm . entrys)
       `(,nnm ,@(map-over entrys
                 (fmt (vars0 termbody)
                    (let* ((varsx (map bnf-node-parser vars0))
                           (vars  (map car varsx))
                           (fmtb  (mapi (fun (i x)
                                          (if (null? (cdr x))
                                            (string->symbol (buildstring "$" i))
                                            (cadr x)))
                                        varsx)))
                    `(,vars ,(list (length vars)
                                   (list 'unquote
                                         `(fun (env stack)
                                            (format stack
                                                    ,fmtb ,termbody)))))
                    )))))))

(function bnf-firstentry-name (entrs)
  (format entrs ((e n)) n))

(macro bnf-parser (entrs . bnf)
 ("Defines a parser from BNF-like declaration and a given list of entry points."
  "Entry points are: [(<entry> <name-to-export>)]"
  "[["
  "<node>:"
  "  (<name> <variant>*)"
  ""
  "<variant>:"
  "( (<token>*) <expr>)"
  ""
  "<token>:"
  "  <symbol> - recognises a token, binds it to the variable  '$<number>'"
  "  <symbol>:<name> - recognises a token, binds it to a given name"
  "]]"
  )
  (letf (( (fsm . rev) (bnf-compile-1 (bnf-compile-2 bnf)) ))
    (let* ((s (Sm<< (bnf-firstentry-name entrs) "--array"))
           (fsm-ll (hashget rev '*COUNTER*))
           (fsm-l
               (hashmap (fun (i l)
                         `(asetx ,s ,(S->N i) (quasiquote ,l)))
                fsm))
           (fsm-ls (let loop ((l fsm-l) (n 0) (sps nil) (cur nil))
                     (if (null? l) (if (null? cur) sps (cons cur sps))
                         (if (> n 60)
                             (loop l 0 (cons (reverse cur) sps) nil)
                             (loop (cdr l) (+ n 1) sps (cons (car l) cur))))))
           (syms (map-over fsm-ls (fun (_) (gensym))))
           )
      `(top-begin
        (define ,s (anew t_object ,fsm-ll))
        ,@(let loop ((x syms) (y (reverse fsm-ls)))
            (if (null? y) nil
                (cons `(function ,(car x) () ,@(car y))
                      (loop (cdr x) (cdr y)))))
        (begin
          ,@(map-over
             syms
             (fun (zxz)
                  `(,zxz))))
        ,@(map-over entrs
            (fmt (nm dnm)
                 `(define ,dnm (fsm-mk-parser ,s ,(hashget rev nm)))))
        ))))

)
;; lxer must return a single token or a short sequence of tokens only
(function make-lazy-lexer (lxer)
   (fun (l0)
     (let loop ((l l0) (cnt 0))
      (if (null? l) nil
       (if (> cnt 5000)
        (fun ()
          (loop l 0)) ;; lazy tail
        (do
           (if (p-success? rslt)
               (append (p-result rslt) (loop (p-rest rslt) (+ cnt 1)))
               (r_raise
                  (new t_MBaseException
                     (object `(SYNTAX-ERROR: ,(genlist->string (p-rest l)))))))
           (where (rslt (lxer l)))
        ))))))

(function make-very-lazy-lexer (lxer)
    (fun (l0)
       (let loop ((l l0))
         (if (null? l) nil
             (do
                 (if (p-success? rslt)
                     (if (null? (p-result rslt)) (loop (p-rest rslt))
                         (let* ((ll (p-result rslt))
                                (lt (lasttail ll)))
                           (set-cdr! lt (fun () (loop (p-rest rslt))))
                           ll))
                     (r_raise
                      (new t_MBaseException
                           (object `(SYNTAX-ERROR: ,(genlist->string (p-rest l)))))))
                 (where (rslt (lxer l))))))))

(function lex-and-parse (lxer prser src)
   ("For given lexer, parser and string, return the result of parsing."
    "Lexer results are passed to the parser via a lazy list."
   )

   (do (car (prserx l))
       (where
           (prserx (prser nil))
           (l ((make-lazy-lexer lxer)
               (if (list? src)
                   src
                   (string->list src)))))))


(function lex-and-parse-stream (lxer prser src)
   (do (prserx l)
       (where
           (prserx (prser nil))
           (l ((make-very-lazy-lexer lxer)
               (xread-stream-list-big src))))))

