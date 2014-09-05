;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; new prolog engine

(Section "Embedded Prolog interpreter")

; Data structures:

(def:ast prolog ()
   (*TOP* <clause>)
   (clause (<term:t> . <*term:ts>))
   (term (|
           (var <variable:v>)
           (str <atom:a> . <*term:ts>)))
   (atom name)
   (variable (n . name)) ;; int, string
   )

; Database:

(function clausesFor (a db)
 (hashget db a))

(function addClause (db c)
 (p:match c
   (
    ( (str $a . $_) . $_)
    (let ((d (hashget db a)))
      (hashput db a (cons c d))))
   (else nil)))

(function dbAppend (db-to db-from)
  (let ((newdb (mkhash)))
   (hashiter (fun (key val) (hashput newdb key val)) db-to)
   (hashiter 
     (fun (key val) 
        (let ((d (hashget newdb key)))
          (hashput newdb key (append val d))))
     db-from)
   newdb
 ))

(function renameVars (lev term)
  (<> term
    (ast:visit prolog term
      (variable _ (cons lev name)))))

(function renClauses (db n term)
   (fccase term
      ((var) _ nil)
      ((str) (s . ts)
       (let ((r (cut renameVars n <>)))
         (<L> `(,(r tm) ,@(map r tp)) |
                  (tm . tp) <- (clausesFor s db))))))

(function varsIn (term)
  (collector (add get)
      (<> term
       (ast:iter prolog term
         (variable _ (add node))))
      (get)))

; Unification and substitution:

(function subst-apply (subst term)
  (<> term
    (ast:visit prolog term
      (term DEEP
        ((var (subst (cadr  node)))
         (str node))))))

(function subst-append (a b) 
  (fun (t)
    (subst-apply a (b t))))

(function id-eq? (a b)
   (and (eq? (car a) (car b))
        (eqv? (cdr a) (cdr b))))

(recfunction var-not-in (v l)
  (if (null? l) #t
     (if (id-eq? v (car l)) nil
       (var-not-in v (cdr l)))))

(function ->- (id term)
   (fun (j)
     (if (id-eq? id j) term
	 `(var ,j))))

(function ->-1 (id term)
  (p:match id
    ((var $id)
     (->- id term))
    (else
     (ccerror `(SUBSTITUTION ,id)))))

(function substI (j)
    `(var ,j))

(define l-substI (list substI))

(recfunction unify (list-unify t1 t2)
  (p:match (list t1 t2)       
    ( ((var $x) (var $y)) 
      (if (id-eq? x y) l-substI (list (->- x t2))))
    ( ((var $x) $t2)
      (if (var-not-in x (varsIn t2)) 
	  (list (->- x t2)) 
	  nil)
      )
    ( ($t2 (var $x) )
      (if (var-not-in x (varsIn t2))
	  (list (->- x t2))
	  nil) 
      )
    ( ((str $a . $as) (str $b . $bs))
      (if (eqv? a b)
          (list-unify as bs) 
          nil) )))

(recfunction list-unify (ts1 ts2)
  (cond
    ((null? ts1)
     (if (null? ts2) l-substI nil))
    ((null? ts2) nil)
    (else
     (format (list ts1 ts2) ((t . ts) (r . rs))
       (<L> (subst-append u2 u1)
          | u1 <- (unify list-unify t r)
          | u2 <- (list-unify (foreach-map (x ts)  (subst-apply u1 x))
                              (foreach-map (x rs)  (subst-apply u1 x)))
       )))))

(function new-unify (t1 t2) (unify list-unify t1 t2))

(function isCut? (t) 
  (p:match t
    ((str cut) #t)
    (else nil)))

; Stack solving engine:

(function alts (db n g)
   (<L> `(,tp ,u) | 
         (tm . tp) <- (renClauses db n g)
         | u <- (new-unify g tm) & (not (null? u))
     )) 

(define plg-builtin-ht (mkhash))
(function plg-builtin? (t)
  (p:match t
    ((str ($v) . $_)
     #t)
    (else nil)))

(function plg-builtin-apply (t)
  (p:match t
    ((str ($v) . $args)
     (v args))
    (else (ccerror `(PROLOG-APPLY: ,t)))))

(macro plg-function (nm args . body)
  (let* ((nnm (gensym)))
    `(begin
       (function ,nnm (args)
	  (format args ,args
	     ,@body))
       (hashput plg-builtin-ht (quote ,nm) (list ,nnm)))))
  
(define plg-failure 'Prolog::Failure::Mark)

(function prove (db goals) ; -> substs
  (letrec  ((solve 
              (fun (n s gls ow)
                (if gls
		   (p:match (car gls)
		     ((str cut)
		      (solve n s (cdr gls) nil))
		     ((str ($v) . $ars)
		      (let ((res (v
				  (map (cut subst-apply s <>) ars))))
			(if (and res (eqv? res plg-failure))
			    nil
			(solve n 
			       (if res (subst-append res s) s)
			       (cdr gls) ow))))
		     (else (choose n s (cdr gls) 
				   (alts db n (subst-apply s (car gls)))
				   ow)))
		   (cons s (backtrack n ow))
		   )))
            (choose 
              (fun (n s gs alts ow)
                (cond
                 ((null? alts) (backtrack n ow))
                 (else
                  (format alts ((tp u) . rs)
                    (solve     (+ n 1) 
                               (subst-append u s)
                               (append tp gs)
                               (cons `(,s ,gs ,rs) ow)))))))
            (backtrack
              (fun (n stk)
                 (cond
                  ((null? stk) nil)
                  (else
                    (format stk ((s gs rs) . ow)
                       (choose (- n 1) s gs rs ow)))))))
   (solve 1 substI goals nil)))


(recfunction varsOnce ( vlist alist )
  (if (null? vlist) nil
    (let ((c (car vlist)))
      (if (null? (filter (fun (x) x) (map (cut id-eq? c <>) alist)))
          (cons c (varsOnce (cdr vlist) (cons c alist)))
          (varsOnce (cdr vlist) alist)
          ))))

(function Prolog (db goals)
   (let ((vrs (varsOnce (foreach-mappend (goal goals) (varsIn goal)) nil))
         (sol (prove db goals)))
     (foreach-map (s sol)
       (map (fun (v) `(,(cdr v) ,(s v))) vrs))))

(function plg-termcompile (id)
  (let ((atmpt (hashget plg-builtin-ht id)))
    (if atmpt atmpt id)))

;; Parsing

(make-simple-lexer prolog-lexer
   (ident-or-keyword (p.lcalpha ((p.lcalpha | p.digit) *)) IDEN)
   (simple-tokens
     "(" LB ")" RB "[" LSB "]" RSB "|" PLK "," COMMA ":-" ARRW "=" EQL "!" CUT "." DOT)
   (regexp-tokens
     ((p.ucalpha (p.alpha *)) -> list->symbol) VIDEN
     ("_" -> (fun (x) (gensym))) VIDEN
     (p.integer -> (M@ (cut Sm<< "i" <>) list->string)) IDEN
     )
   (ignore p.whitespace))

(bnf-parser
  ((rule parse-prolog-rule)
   (rules parse-prolog-rules)
   (term parse-prolog-goal)
   (terms parse-prolog-goals)
  )

  (rules
   ( (rule rules)  (cons $0 $1) )
   ( (rule)        (list $0) )
   )
  (rule
   ( (term DOT)    (list $0) )
   ( (term ARRW terms DOT) (cons $0 $2) )
   )
  (terms
   ( (term COMMA terms) (cons $0 $2) )
   ( (term) (list $0) ))
  (termlist
   ( (term COMMA termlist) `(str cons ,$0 ,$2) )
   ( (term PLK term ) `(str cons ,$0 ,$2) )
   ( (term) `(str cons ,$0 (str nil) ) )
   )
  (term
   ( ( pterm:t1 IDEN:i1 pterm:t2 ) `(str ,(plg-termcompile i1) ,t1 ,t2) ) 
   ( ( pterm:t1 EQL pterm:t2 ) `(str equals ,t1 ,t2))
   ( ( pterm ) $0)
   ( ( LB term RB) $1 )
   )
  (pterm
   ( ( LSB RSB ) '(str nil) )
   ( ( LSB termlist:tl RSB) tl )
   ( ( LSB term:t RSB ) `(str cons ,t (const nil)))
   ( ( IDEN:id LB terms:ts RB ) `(str ,(plg-termcompile id) ,@ts) )
   ( ( VIDEN ) `(var ,(cons 1 $0) ) )
   ( ( CUT )   `(str cut) )
   ( ( IDEN  ) `(str ,$0) )
   ( ( INT ) `(str ,(string->symbol (S<< $0))))
   ))

(function pprologrules (str)
 "Parses prolog rules."
 (lex-and-parse prolog-lexer parse-prolog-rules str))

(function pprologgoal (str)
 "Parses a single prolog term."
 (lex-and-parse prolog-lexer parse-prolog-goal str))

(function pprologgoals (str)
 "Parses a comma separated list of prolog terms."
 (lex-and-parse prolog-lexer parse-prolog-goals str))

(recfunction prolog-print-list (prolog-print hd tl)
 (let ((other (fun () (buildstring (prolog-print hd) "|" (prolog-print tl)))))
  (fccase tl
    ((str) (id . rest)
     (if (eq? id 'cons)
       (buildstring (prolog-print hd) "," (prolog-print-list prolog-print
                                               (car rest) (cadr rest)))
       (other)))
    (else (other)))))
 
(recfunction prolog-print (term)
  "Converts a prolog term into a pretty-printed string."
   (fccase term
     ((var) ((n . nm)) nm)
     ((str) (id . rest)
      (cond 
       ((null? rest) (if (eq? id 'nil) "[]" id))
       ((eq? id 'cons)
        (buildstring "["
            (prolog-print-list prolog-print (car rest) (cadr rest))
            "]"))
       (else
         (buildstring
           id "(" (foldl string-append ""
                         (interleave (map prolog-print rest) ","))
           ")"))))))

(function rules->db (rules)
  (let ((db (mkhash)))
   (foreach (r rules) (addClause db r))
   db))

(function prolog-pp-results (res)
 "Creates a list of pretty-printed prolog query evaluation results"
  (foreach-map (r res)
     (foreach-map (v r)
        (format v (s t)
            `(,s ,(prolog-print t))))))

(force-class-flush)

(define DefaultPrologDB
  "Basic prolog definitions: and, or, equals, not, append, ..."
  (rules->db
    (ctime `(quote 
      ,(pprologrules
         (S<<
	  "or(X,Y):-X."
	  "or(X,Y):-Y."
	  "equals(X,X)."
	  "and(X,Y):-X,Y."
	  "not(X):-X,!,false(shmalse)."
	  "not(X)."
	  "append([],L,L)."
	  "append([H|T],L,[H|A]) :- append(T,L,A)."

	  "true.")))))
  )

(function simple-prolog (xdb goals)
  ("Parses a prolog query and executes it over a given rules database."
   "If xdb is null, uses the default one ([DefaultPrologDB])."
  )
   (let ((db (if (null? xdb)
                 DefaultPrologDB
                 (dbAppend DefaultPrologDB (rules->db (pprologrules xdb))))))
     (Prolog db (pprologgoals goals))))
	   


;; list based prolog frontend:

(recfunction make-nested ( f s a )
  (p:match a
    (() nil)
    (($x) (f x))
    (($x $y) `(str ,s ,(f x) ,(f y)))
    (($x . $z) `(str ,s ,(f x) ,(make-nested f s z)))))

(recfunction to-prolog ( lst )
  ("Converts the list-based representation into the correct format."
   "The simplified list-based foramt is following:"
   "[["
   "<term>:"
   "  (quote <symbol>)    -> symbol/0 structure"
   "  <symbol>            -> symbol variable "
   "  (<symbol1> <term>*) -> symbol1(term,...) structure "
   "]]"
  )
  (p:match lst
    (() nil)
    ($$M `(var ,(cons 0 lst)))
    ((quote $v) `(str ,v))
    ((<- $left . $right) ;;; rule
     `(,(to-prolog left) ,@(map to-prolog right)))
    ((($fn . $ars))
     `(,(to-prolog (car lst))))
    ((or . $ars)
     (make-nested to-prolog 'or ars))
    ((and . $ars)
     (make-nested to-prolog 'and ars))
    (($fn . $ars) `(str ,(plg-termcompile fn) ,@(map to-prolog ars)))
    (else nil)))

(function simple-prolog-0 (xdb goals)
   (let ((db (if (null? xdb) DefaultPrologDB
             (dbAppend DefaultPrologDB xdb))))
      (Prolog db goals)))

(function simple-prolog-l (xdb goals)
  ("Converts a list of query goals from the simplified list representation,"
   "evaluates the query over a given rules database (default if null),"
   "returns the results."
  )
   (let ((db (if (null? xdb) DefaultPrologDB
             (dbAppend DefaultPrologDB (rules->db (map to-prolog xdb))))))
      (Prolog db (map to-prolog goals))))


(plg-function print (a)
  (writeline `(PRINT PRED: ,a))
  nil)

(function plg-numvalue (t)
  (p:match t
    ((str $nid)
     (S->N (list->string
	    (cdr (string->list (any->string nid))))))
    (else (ccerror `(PLG-NUMVALUE ,t)))))

(function plg-var? (t)
  (p:match t
    ((var . $_) 'yes)
    (else nil)))

(plg-function plus (a b c)
   (p:match (map plg-var? (list a b c))
     ((yes () ())
      (->-1 a `(str ,(Sm<< 'i (->s (+ (plg-numvalue b)
				      (plg-numvalue c)))))))
     ((() yes ())
      (->-1 b `(str ,(Sm<< 'i (->s (- (plg-numvalue a)
				      (plg-numvalue c)))))))
     ((() () yes)
      (->-1 c `(str ,(Sm<< 'i (->s (- (plg-numvalue a)
				      (plg-numvalue b)))))))
     (else (ccerror `(PROLOG:+ ,a ,b ,c)))))

(plg-function mult (a b c)
   (p:match (map plg-var? (list a b c))
     ((yes () ())
      (->-1 a `(str ,(Sm<< 'i (->s (* (plg-numvalue b)
				      (plg-numvalue c)))))))
     ((() yes ())
      (->-1 b `(str ,(Sm<< 'i (->s (/ (plg-numvalue a)
				      (plg-numvalue c)))))))
     ((() () yes)
      (->-1 c `(str ,(Sm<< 'i (->s (/ (plg-numvalue a)
				      (plg-numvalue b)))))))
     (else (ccerror `(PROLOG:* ,a ,b ,c)))))


(plg-function minus (a b c)
   (p:match (map plg-var? (list a b c))
     ((yes () ())
      (->-1 a `(str ,(Sm<< 'i (->s (- (plg-numvalue b)
				      (plg-numvalue c)))))))
     ((() yes ())
      (->-1 b `(str ,(Sm<< 'i (->s (+ (plg-numvalue a)
				      (plg-numvalue c)))))))
     ((() () yes)
      (->-1 c `(str ,(Sm<< 'i (->s (- (plg-numvalue b)
				      (plg-numvalue a)))))))
     (else (ccerror `(PROLOG:+ ,a ,b ,c)))))

(plg-function write (a)
   (p:match (plg-var? a)
     (()
      (begin
	(println a)
	(fun (j) `(var ,j))))
     (else (fun (j) `(var ,j)))))

;;; Unit tests
(unit-test 3 (prolog-pp-results (simple-prolog nil "append([1,2,c],X,[1,2,c,d,e])"))  (((X "[d,e|[]]"))))
(unit-test 3 (prolog-pp-results (simple-prolog nil "append([1],[2],X)"))  (((X "[i1,i2|[]]"))))
(unit-test 3 (prolog-pp-results (simple-prolog nil "plus(X,2,2)")) (((X i4))))

(unit-test 3 (prolog-pp-results (simple-prolog nil "plus(20,X,10)")) (((X i10))))

(unit-test 3
  (prolog-pp-results
   (simple-prolog
    "last([X],X).last([H|T],R):- last(T,R)."
    "last([1,2,3,4,5],X)"))  (((X i5))))

(unit-test 3
 (prolog-pp-results
  (simple-prolog 
   "vadd([],[],[]).
    vadd([A|T],[B|L],[SH|LL]) :- plus(SH,A,B), vadd(T,L,LL)."
   "vadd([2,3,4],[4,5,6],X)"))
 (((X "[i6,i8,i10|[]]"))))

(unit-test 3
  (prolog-pp-results
   (simple-prolog
    "vsumx(S,[],S).
     vsumx(S,[H|T],R) :- plus(N,S,H),vsumx(N,T,R).
     vsum(L,R) :- vsumx(0,L,R)."
    "vsum([1,2,3,4],R)"))
  (((R i10))))

