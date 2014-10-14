;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;
; Higher-level language frontent for LLtNET backend

(define *lltnet-macros* (mkhash))

; Pass 1.: normalise @-s

(define r@xp (<r> 
	      (
	       (((! ("@" | "#")) +*) :-> list->symbol)
	       |
	       (("@" | "#") :-> list->symbol)) *))

(function match@s (str)
  (p-result (r@xp (string->list str))))

(function normalise@s ( lst )
  (let loop ((l lst))
    (p:match l
      (($$M:h . $rest) `(,@(match@s (symbol->string h)) ,@(loop rest)))
      (($$L:h . $rest) `(,(loop h) ,@(loop rest)))
      (($h . $rest) `(,h ,@(loop rest)))
      (else l))))

; Pass 2.: match expressions and operators

(recfunction compile-type (p)
  (case p
    ((void
      int
      ptr
      short
      long
      char
      byte
      float
      double
      string
      object
      bool
      unknown
      this) (list p))
    (else
     (p:match p
       ((array $x) `(array ,(compile-type x)))
       ((generic $x . $args) `(generic ,x ,@(map compile-type args)))
       (else
	`(T ,@p))))))

(function isitatypename (n)
  (p:match (compile-type n)
    (($x) #t)
    (else
     (not (null? (dotnet (S<< n)))))))

(function checkdotpath (n)
  (let* ((pth
          (let loop ((p n))
            (p:match p
              (($l [#] $r) (append (loop l) (loop r)))
              ($$M (strsplit (<r> ".") (S<< p)))
              (else (ccerror `(LL-DOTPATH ,n))))))
         (resolv 
          (let loop ((p (cdr pth)) (tp (car pth)) (c (car pth)) (lst nil))
            (let* ((atmpt (isitatypename (Sm<< tp))))
              (if p
                  (if atmpt 
                      (loop (cdr p) (S<< tp "." (car p)) tp
                            (list c p))
                      (loop (cdr p) (S<< tp "." (car p)) c lst)
                  )
                  (if atmpt (list tp p) lst)
                  )))))
    (if resolv
        (let loop ((i (reverse (cadr resolv))))
          (p:match i
            (() (Sm<< (car resolv)))
            (($hd . $tl)
             `(,(loop tl) # ,(Sm<< hd)))))
        n)))

(function dotpath? (expr)
  (if (let loop ((n expr))
        (p:match n 
          (($a [#] $b) (loop a))
          ($$M #t)
          (else nil)))
      (checkdotpath expr)
      expr))

(define fltprs 
  (<r>
   (((_ "f") (?? "-") ((p.digit +*) "." (p.digit +*)))
    -> (fun (l)
	 `(fc ,(list->string l))))
   |
   (((_ "d") (?? "-") ((p.digit +*) "." (p.digit +*)))
    -> (fun (l)
	 `(dfc ,(list->string l))))
   |
   (((_ "l.") (?? "-") (p.digit +*))
    -> (fun (l)
	 `(i64c ,(list->string l))))
   |
   (((_ "h.") ((p.digit | p.alpha) +*))
    -> (fun (l)
	 `(ic ,(HX->N (list->string l)))))
   |
   ((_ (p. *)))))

(function tryparsefloat (v)
  (p-result (fltprs (string->list (any->string v)))))

(function lltnet-hl-compile-type (p) (compile-type  p))

(function mkbin (loop op a)
  (let xloop ((l a))
    (p:match l
      (($x) (loop x))
      (($x $y)
       `(bin ,op ,(loop x) ,(loop y)))
      (($x . $rest)
       `(bin ,op ,(loop x) ,(xloop rest))))))
  
(function compile-erest (loop l)
  (p:match l
      ((typeof $expr)
       `(typeof ,(loop expr)))
      ((istype $expr $tp)
       `(istype ,(loop expr) ,(compile-type tp)))
      (([&&] $$M:smthng [@] $$M:name . $argtyps)
       `(funref (( ,(compile-type smthng) (unknown)
		   ,@(map compile-type argtyps)) ,name)))
      (([&&&] $$M:smthng $tpore $$M:name)
       (let* ((tpe (if (isitatypename tpore)
		       (cons
			(compile-type tpore)
			'(null))
		       (cons
			'(unknown)
			(loop tpore))))
	      (tp (car tpe))
	      (ee (cdr tpe)))
	 `(newdelegate ,(compile-type smthng)
		       ,tp
		       ,name ,ee)))
      (($s [@] $$M:name . $rest)
       (alet dp (dotpath? s)
         (if (and (symbol? dp)
                  (isitatypename dp))
             `(asm () (( ,(compile-type dp) (unknown)
                      ,@(map (fun (_) '(unknown)) rest))
                    ,name)
                   ,@(map loop rest))
             `(am () ,(loop dp) (( (unknown) (unknown)
                                ,@(map (fun (_) '(unknown)) rest))
                              ,name)
                  ,@(map loop rest)))))
      (([+] $a $b) `(bin + ,(loop a) ,(loop b)))
      (([-] $a $b) `(bin - ,(loop a) ,(loop b)))
      (([*] $a $b) `(bin * ,(loop a) ,(loop b)))
      (([/] $a $b) `(bin / ,(loop a) ,(loop b)))
      (([mod] $a $b) `(bin mod ,(loop a) ,(loop b)))

      (([-] $a) `(un - ,(loop a)))
      (([not] $a) `(un not ,(loop a)))
      (([isnull] $a) `(un isnull ,(loop a)))
      (([bitnot] $a) `(un bitnot ,(loop a)))

      (([>] $a $b) `(bin > ,(loop a) ,(loop b)))
      (([<] $a $b) `(bin < ,(loop a) ,(loop b)))
      (([>=] $a $b) `(bin >= ,(loop a) ,(loop b)))
      (([<=] $a $b) `(bin <= ,(loop a) ,(loop b)))
      (([==] $a $b) `(bin == ,(loop a) ,(loop b)))
      (([!=] $a $b) `(bin != ,(loop a) ,(loop b)))
      (([and] .  $a) (mkbin loop 'and a))
      (([or] . $a) (mkbin loop 'or a))
      (([xor] $a $b) `(bin xor ,(loop a) ,(loop b)))
      (([binor] $a $b) `(bin binor ,(loop a) ,(loop b)))
      (([binand] $a $b) `(bin binand ,(loop a) ,(loop b)))

      (([lshift] $a $b) `(bin lshift ,(loop a) ,(loop b)))
      (([rshift] $a $b) `(bin rshift ,(loop a) ,(loop b)))

      (null '(null))
      (true '(true))
      (false '(false))
      (self '(ldthis))
      ($$M
       (let* ((tst (tryparsefloat l)))
	 (if tst tst
	     `(v ,l))))
      ($$S
       `(sc ,l))
      ($$N `(ic ,l))
      (($$M:x) (loop x))
      (($$M:mcr . $rst)
       (let ((tst (hashget *lltnet-macros* mcr)))
	 (if tst
	     (loop (normalise@s (tst l)))
	     (ccerror `(EXPR:ERRORM: ,l)))))
      (($$L:l) (loop l))
      (else (ccerror `(EXPR:ERROR: ,l)))))

(function check-leave (v)
  (p:match v
    ((leave $x) x)
    ((return $x) x)
    (else v)))

(function compile-expr-0 (bigloop lst)
  (let loop ((l lst))
    (p:match l
      ((($tp) $expr)
       `(typecast ,(compile-type tp) ,(loop expr)))
      ((type $tp)
       `(typetoken ,(compile-type tp)))
      ((fieldtoken $fld)
       `(fieldtoken ,(loop fld)))
      ((marshal $tp $expr)
       `(marshal ,(compile-type tp) ,(loop expr)))
      ((arr . $exprs)
       `(newarr ,@(map loop exprs)))
      ((arrt $tp . $exprs)
       `(newtarr ,(compile-type tp) ,@(map loop exprs)))
      ((mkarr $tp $expr)
       `(allocarr ,(compile-type tp) ,(loop expr)))
      ((ref $nm)
       `(ref ,nm))
      ((aref $x $idx)
       `(ar ,(loop x) ,(loop idx)))
      ((begin . $l)
       `(embedded-begin ,(map bigloop (cuttail l))
			,(loop (check-leave (car (lasttail l))))))
      ((new $tp . $args)
       `(asm () (( ,(compile-type tp) (unknown)
		,@(map (fun (_) '(unknown)) args))
	      ,(Sm<< ".ctor")) ,@(map loop args)))
      (($$M:smthng [#] $$M:member)
       (if (isitatypename smthng)
	   `(sf ((,(compile-type smthng) (unknown)) ,member))
	   `(f ,(loop smthng) (( (unknown) (unknown)) ,member))))
      (($smthng [#] $$M:member)
       (alet dp (dotpath? smthng)
          (if (symbol? dp)
              `(sf ((,(compile-type dp) (unknown)) ,member))
              `(f ,(loop dp) (( (unknown) (unknown)) ,member)))))
      (($$M:smthng [@] $$M:name . $rest)
       (if (isitatypename smthng)
	   `(asm () (( ,(compile-type smthng) (unknown)
		    ,@(map (fun (_) '(unknown)) rest))
		  ,name)
		 ,@(map loop rest))
	   `(am () ,(loop smthng) (( (unknown) (unknown)
			       ,@(map (fun (_) '(unknown)) rest))
			     ,name)
		,@(map loop rest))))
      (else (compile-erest loop l)))))

(function compile-operator (lms lfs lst )
  (let loop ((l lst))
   (let ((compile-expr (fun (e) (compile-expr-0 loop e))))
    (p:match l
      ((begin . $ops) `(begin
			 ,@(map loop ops)))
      ((quote $$M:l) `(label ,l))
      ((debugpoint . $d) `(debugpoint ,@d))
      ((for (($$M:v $i $s) $e) . $b)
       `(for ((,v ,(compile-expr i)) ,(compile-expr s) ,(compile-expr e))
	   (begin
		,@(map loop b))))
      ((while $e . $b)
       `(while ,(compile-expr e)
	   (begin
		,@(map loop b))))
      ((dowhile $e . $b)
       `(dowhile ,(compile-expr e)
	   (begin
		,@(map loop b))))
      ((foreach ($v $i) . $b)
       `(foreach (,v ,(compile-expr i))
		 (begin ,@(map loop b))))
      ((goto $$M:l) `(goto ,l))
      ((goto-if $e $$M:l) `(gotoif ,(compile-expr e) ,l))
      ((goto-if-not $e $$M:l) `(gotoiff ,(compile-expr e) ,l))
      ((if $e $tr . $fl)
       `(if ,(compile-expr e) ,(loop tr) ,@(map loop fl)))
      ((return $e)
       `(return ,(compile-expr e)))
      ((tail-return $e)
       `(tailreturn ,(compile-expr e)))
      ((leave $e)
       `(leave ,(compile-expr e)))
      ((null)
       `(leave ,(compile-expr '(null))))
      ((lift-method $b)
       (set-cdr! lms (cons b (cdr lms)))
       '(nop))
      ((lift-field $f)
       (set-cdr! lfs (cons f (cdr lfs)))
       '(nop))
      ((nop) '(nop))
      ((return)
       `(vreturn))
      ((try $b (catch ($tp $nm) . $bs))
       `(tryblock
	 ,(loop b)
	 (,nm ,(compile-type tp))
	 (begin
	   ,@(map loop bs))))
      ((throw $e)
       `(throw ,(compile-expr e)))
      ((asm . $body)
       `(embedd-asm ,@body))
      ((declare $$M:tp $$M:v) `(tvardef ,(compile-type tp) ,v))
      (($$M:v [=] $e) `(var ,v ,(compile-expr e)))
      (($$M:tp $$M:v [=] $e) `(tvar ,(compile-type tp) ,v ,(compile-expr e)))
      (($$XXX:lvalue [<-] $e) `(set ,(compile-expr lvalue) ,(compile-expr e)))
      (($$M:mcr . $rst)
       (let ((tst (hashget *lltnet-macros* mcr)))
	 (if tst
	     (loop (normalise@s (tst l)))
	     `(e ,(compile-expr l)))))
      (else `(e ,(compile-expr l)))))))

(function lltnet-hl (lmethods lfields src)
  (compile-operator lmethods lfields (normalise@s src)))

(function lltnet-defmacro (name mcr)
  (hashput *lltnet-macros* name mcr))

(macro lltnet-macro (name args . body)
  ("Defines a Not.Net.hlevel macro.")
  (let ((s (gensym)) (arg (gensym)))
    `(top-begin
       (function ,s (,arg)
	   (format ,arg (macro-name ,@args)
	     ,@body))
       (lltnet-defmacro (quote ,name) ,s)
       (force-class-flush)
       )))


(function not.neth0 (pure? xtp blk? args body )
  (let* ((lmethods (cons nil nil))
	 (lfields (cons nil nil))
	 (domethod 
	  (fun (m)
	    (format m ("method" acc tp nm args . body)
		    `(method ,acc (,(lltnet-hl-compile-type tp) ,nm 
				   ,@(map (fmt (t n)
					       `(,(lltnet-hl-compile-type t)
						 ,n)) args))
			     ,(lltnet-hl lmethods lfields
					 `(begin ,@body))))))
	 (dofield 
	  (fun (f)
	    (format f ("field" type . nameopts)
		(let* ((fname (car nameopts))
		       (fopts (if (null? (cdr nameopts))
				  '((public)) (cdr nameopts))))
		  `(field ,fopts ,(lltnet-hl-compile-type type) ,fname (null))
		  ))))
	 (code (lltnet-hl lmethods lfields `(begin ,@body))))
    (f.not.net.lift pure? xtp blk?
                  (map-over args
                            (fmt (tp nm)
                                 `(,(lltnet-hl-compile-type tp) ,nm)))
                  `(,@(map domethod (cdr lmethods)) ,@(map dofield (cdr lfields))
		    )
                  code
                  )))


(macro not.neth ( args .  body )
  ("Compiles and substitutes a Not.Net simple form code.")
  (not.neth0 nil #t nil args body))

(macro not.nethf ( args .  body )
  ("Compiles and substitutes a Not.Net simple form code.")
  (not.neth0 nil #t #t args body))

(macro not.nethr ( args .  body )
  ("Compiles and substitutes a Not.Net simple form code, adds [[nil]] at the end..")
  (not.neth0 nil nil nil args body))

(macro not.n (oargs . body)
  `(not.neth ,(foreach-map (o oargs) `(object ,o)) ,@body))


(macro not.function (name args . body)
  ("Compiles and substitutes a function containing a Not.Net simple form code.")
  `(function ,name ,(map cadr args)
     (not.nethf ,args
	,@body)
     ))

(define int->byte-hook (cons nil nil))


(function f.not.topexpand (body)
  (foreach-map (b0 body)
    (let loop ((b b0))
      (p:match b
        (($$M:mcr . $rst)
         (let ((tst (hashget *lltnet-macros* mcr)))
           (if tst
               (loop (tst b))
               b)))))))

(function f.not.class (name body0)
  (let* ((body (f.not.topexpand body0))
         (extends (select-car 'extends body))
	 (implements (select-car 'implements body))
	 (fields (select-car 'field body))
	 (ifields (select-car 'initfield body))
	 (methods0 (select-car 'method body))
	 (constrs (select-car 'constructor body))
	 (methods (append (map-over constrs
			    (fmt ("constructor" acc args . body)
				 `(method ,acc this ,(not.ctor-name acc)
                                          ,args ,@body)))
			  methods0))
	 (others (select-car 'custom body))
	 (lmethods (cons nil nil))
	 (lfields (cons nil nil))
	 (domethod 
	  (fun (m)
	    (format m ("method" acc tp nm args . body)
		    `(method ,acc (,(lltnet-hl-compile-type tp) ,nm 
				   ,@(map (fmt (t n)
					       `(,(lltnet-hl-compile-type t)
						 ,n)) args))
			     ,(lltnet-hl lmethods lfields
					 `(begin ,@body))))))
	 (dofield 
	  (fun (f)
	    (format f ("field" type . nameopts)
		(let* ((fname (car nameopts))
		       (fopts (if (null? (cdr nameopts))
				  '((public)) (cdr nameopts))))
		  `(field ,fopts ,(lltnet-hl-compile-type type) ,fname (null))
		  ))))
         (doifield
          (fun (f)
            (format f ("initfield" name opts . data)
              `(initfield ,opts ,name ,@(map (car int->byte-hook) data)))))
	 )
    (lltnet-emit
     (lltnet-prepare-top
      `((,name ,(if (null? extends) 'System.Object (cadar extends))
	       ,@(foldl append nil (map cdr implements)))
	,@(map domethod methods)
	,@(map dofield fields)
        ,@(map doifield ifields)
	,@(map domethod (cdr lmethods)) ;;TODO: lmethods can be updated here
	,@(map dofield (cdr lfields))
	,@others
	))
     nil
     )
    ))

(macro not.class (name . body)
  ("Compiles and substitutes a Not.Net class with methods code in simple form.")
  (f.not.class name body))

(macro not-new-array (type length)
  ("Creates a new array of a given type")
  (with-syms (l)
   `(let ((,l ,length))
      (not.neth ((int ,l))
         (leave ((object)(mkarr ,type ,l)))))))

; Macros

(lltnet-macro print (vlu)
   `(System.Console@WriteLine ,vlu))

(lltnet-macro concat lst
 (let loop ((l lst))
   (if (< (length l) 5)
       `(string@Concat ,@l)
       (p:match l
	 (($a $b $c . $rest)
	  `(string@Concat (string@Concat ,a ,b ,c) ,(loop rest)))
	 (($h . $tl)
	  `(string@Concat ,h ,(loop tl)))))))


(lltnet-macro mbase (fn . args)
   `(Meta.Scripting.Runtime@Apply ,fn (arr ,@(foreach-map (a args)
							  `((object) ,a)))))

(function ll-ftype-hl (tp)
  (alet tt0 (ll-ftype tp)
    (let loop ((tt tt0))
        (p:match tt
          ((T . $v) v)
          (($nm) nm)
          ((array $v) `(array ,(loop v)))
          (else (ccerror `(LL-FTYPE-HL: ,tp)))))))

;;;TODO: pass core fieldinfo and methodinfo values
(function lltnet-core-field (finfo)
  (let* ((ftype (not.neth ((System.Reflection.FieldInfo finfo))
                   (leave (finfo@get_DeclaringType))))
         (fname (string->symbol
                 (not.neth ((System.Reflection.FieldInfo finfo))
                   (leave (finfo@get_Name)))))
         (fstype (ll-ftype-hl ftype)))
    `(,fstype # ,fname)))

(function lltnet-core-method (minfo)
  (let* ((mtype (not.neth ((System.Reflection.MethodInfo minfo))
                   (leave (minfo@get_DeclaringType))))
         (mname (string->symbol
                 (not.neth ((System.Reflection.MethodInfo minfo))
                   (leave (minfo@get_Name)))))
         (mstype (ll-ftype-hl mtype))
         )
    `(,mstype @ ,mname)))

(lltnet-macro mbase-fn (fnm . args)
  (let* ((vlu (net.env.get fnm)))
    (p:match vlu
      ((fldinfo $finf)
       (with-syms (fff)
        `(mbase ,(lltnet-core-field finf) ,@args)))
      ((minfo $mf $nargs)
       (if (not (= nargs (length args)))
           (ccerror `(INVALID-NUMBER-OF-ARGS ,fnm)))
       `(,@(lltnet-core-method mf) ,@(foreach-map (a args)
                                       `((object) ,a)))))))

(lltnet-macro foreach-array ((idx vlu ar) . body)
  (with-syms (x)
    `(begin
       (,x = (,ar @ get_Length))
       (for ((,idx 0 (+ ,idx 1)) (< ,idx ,x))
            (,vlu = (aref ,ar ,idx))
            ,@body))))

(lltnet-macro mblist lst
   (let loop ((l lst))
     (p:match l
       (($hd)
        `(new Pair ((object) ,hd) null))
       (($hd . $tl)
        `(new Pair ((object) ,hd) ((object) ,(loop tl))))
       (() 'null))))

(macro not-init-array (type size init)
  (with-syms (x)
    `(let ((,x (not-new-array ,type ,size)))
       (not.neth (((array ,type) ,x))
         (foreach-array (i v ,x)
            ((aref ,x i) <- ,init))
         (leave ,x)))))

(macro not.aset (type ar pos val)
  (with-syms (x xar xpos)
    `(let ((,x ,val)
           (,xar ,ar)
           (,xpos ,pos))
       (not.neth (((array ,type) ,xar)
                  (int ,xpos)
                  (,type ,x))
          ((aref ,xar ,xpos) <- ,x)
          (leave ((object) ,xar))))))
                
(Par
"\\include{notnet}")