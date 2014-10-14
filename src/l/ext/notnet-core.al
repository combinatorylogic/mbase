
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(function lltnet-expand0 ( body )
  (<> body
    (ast:visit lltnet mtdbody
       (mtdbody DEEP
	  (
	   (debugpoint `(debugpoint ,@d))
	   (for 
	    (let* ((l (gensymp "forlbl"))
		   (l1 (gensymp "forlbl")))
	      `(begin
		 (var ,var ,init)
		 (label ,l1)
		 (gotoiff ,cont ,l)
		 ,body
		 (set (v ,var) ,step)
		 (goto ,l1)
		 (label ,l)
		 )))
	   (dowhile
	    (let* ((l (gensymp "whilelbl")))
	      `(begin
		 (label ,l)
		 ,body
		 (gotoif ,cont ,l))))
	   (foreach
	    (let ((l (gensymp "foreachlbl"))
		  (exit (gensymp "foreachlbl"))
		  (v (gensymp "iter")))
	      `(begin
		 (var ,v (am () ,init
			     (((T . System.IEnumerable) (unknown)) 
			      GetEnumerator)))
		 (label ,l)
		 (gotoiff (am () (v ,v)
			      (((T . System.IEnumerator) (unknown))
			       MoveNext)) ,exit)
		 (var ,var (am () (v ,v) (((T . System.IEnumerator) (unknown))
				       get_Current
				       )))
		 ,body
		 (goto ,l)
		 (label ,exit)
		 )))
	   (while
	    (let* ((l (gensymp "whilelbl"))
		   (l1 (gensymp "whilelbl")))
	      `(begin
		 (label ,l)
		 (gotoiff ,cont ,l1)
		 ,body
		 (goto ,l)
		 (label ,l1))))
	   (else node))))))

(def:ast lltnett ( ( lltnet ( expression iexpression)) )
  (expression (<type:t> . <iexpression:v>)))
		  

; Preparation step: transform variable scope.

(recfunction scopetrans ( src )
  (<> src
    (ast:visit lltnet mtdbody
      (expression DEEP
	 ((embedded-begin
	   `(embedded-begin-1 ,(scopetrans 
				`(begin ,@(append bs `((subst ,e)))))))
	  (else node)))
      (mtdbody DEEP
       ((begin
	 (cons 'begin
 	  (let innerloop ((bdy b))
	   (p:match bdy
	     (( (tvardef $tp $nm . $rest1) . $rest2)
	      `((tvardef ,tp ,nm (begin ,@(append rest1 (innerloop rest2))))))
	     (( (tvar $tp $nm $vl . $rest1) . $rest2)
	      `((tvar ,tp ,nm ,vl (begin ,@(append rest1 (innerloop rest2))))))
	     (( (var $nm $vl . $rest1) . $rest2)
	      `((var ,nm ,vl (begin ,@(append rest1 (innerloop rest2))))))
	     (( (vvar $mtp $ags $nm . $rest1) . $rest2)
	      `((vvar ,mtp ,ags ,nm 
		      (begin ,@(append rest1 (innerloop rest2))))))
	     (( $hd . $tl )
	      (cons hd (innerloop tl)))
	     (() nil)))))
	(else node))))))

;;;;;;;;;;;;;;;;
(define _is_array (r_tbind "System.Type" "get_IsArray"))
(define _elt_type (r_tbind "System.Type" "GetElementType"))
(define _mktype   (r_tsbind "System.Type" "GetType" string))

(recfunction ll-ftype ( tp )
  (let ((v (string->symbol (->s tp))))
    (case v
      ((System.Int32) '(int))
      ((System.IntPtr) '(ptr))
      ((System.String) '(string))
      ((System.Void) '(void))
      ((System.Int16) '(short))
      ((System.Int64) '(long))
      ((System.Char) '(char))
      ((System.Byte) '(byte))
      ((System.Double) '(double))
      ((System.Single) '(float))
      ((System.Object) '(object))
      ((System.Boolean) '(bool))
      (else
       (cond
	((_is_array tp)
	 `(array ,(ll-ftype (_elt_type tp))))
	; TODO: ... for .NET >= 2.0 check for generics here
	(else `(T ,@v)))))))

(function IsValueTypeT (t)
  (p:match t
    ((this) nil)
    (else (IsValueType t))))

(define nst (<r> "/"))
(function looknested (tpn)
  (p:match (strsplit nst (any->string tpn))
    (($a $b . $r)
     (let ((ta (dotnet a)))
       (if (null? ta) nil
	   (let loop ((t ta) (l (cons b r)))
	     (p:match l
	       (() t)
	       (($hd . $tl)
		(let* ((tb ((r_tbind "System.Type" "GetNestedType" string)
			    t
			    hd)))
		  (if (null? tb) nil
		      (loop tb tl))))
	       (else nil))))))))

(function dotnetx (tp)
  (let ((r1 (looknested tp)))
    (if r1 r1
	(let ((ret (dotnet tp)))
	  (if (null? ret)
	      (ccerror `(CANT-RESOLVE-TYPE ,tp)))
	  ret))))

(recfunction ll-nettype ( tp )
  (p:match tp
    ((void) (dotnetx "System.Void"))
    ((int) (dotnetx "System.Int32"))
    ((ptr) (dotnetx "System.IntPtr"))
    ((short) (dotnetx "System.Int16"))
    ((long) (dotnetx "System.Int64"))
    ((char) (dotnetx "System.Char"))
    ((byte) (dotnetx "System.Byte"))
    ((string) (dotnetx "System.String"))
    ((double) (dotnetx "System.Double"))
    ((float) (dotnetx "System.Single"))
    ((object) (dotnetx "System.Object"))
    ((bool) (dotnetx "System.Boolean"))
    ((array $t) (dotnetx (S<< (->s (ll-nettype t)) "[]")))
    ((generic $n . $ts)
     (dotnetx (S<< n "`" (length ts) "["
		   (strinterleave (map ll-nettype ts) ",")
		   "]"
		   )))
    ((ref $t) (dotnetx (S<< (->s (ll-nettype t)) "&")))
    ((boxed $t) (ll-nettype t))
    ((unknown) nil)
    ((this) nil)
    ((T . $tp) (dotnetx (->s tp)))
    (else (ccerror `(BAD-NET-TYPE ,tp)))))

(function ll-nettypeT (tp)
  (p:match tp
    ((this) tp)
    (else (ll-nettype tp))))

(function ll-boxexpression (e)
  (let* ((t (car e))
	 (tt (ll-nettypeT t)))
    (if (and (not (null? tt)) (IsValueTypeT tt))
	(cons `(boxed ,(ll-ftype tt)) (cdr e))
	e
	)))

(function lltnet-manglename (nm args)
  (S<< nm "_--_" (strinterleave (map (M@ ->s ll-nettypeT) args) "_")))

(function tt? (a b)
  (let* ((ta (ll-nettype a))
	 (tb (ll-nettype b)))
    (if (or (null? ta) (null? tb)) #t
	(t_eq ta tb))))

(function signature-ok? (a b)
  (cond
   ((not (= (length a) (length b))) nil)
   ((alltrue (fmt (x . y) (tt? x y)) (czip a b)) #t)
   (else nil)))

(recfunction lookup-localmethod-signature (ctor? al args)
  (p:match al
    (() nil)
    (((method $rename $ret $margs) . $rest)
     (if (signature-ok? args margs)
	 (cons ret `( ((this) ,ret ,@margs) ,(if ctor? `(CTOR ,rename) rename)))
	 (lookup-localmethod-signature ctor? rest args)))))

(function lookup-inherited (lst name ars)
  (let ((res
	 (il-types-havemethod lst name
			      nil (map ll-nettype ars))))
    (if (null? res)
	(ccerror `(CANT-RESOLVE-LOCAL-METHOD ,name ,ars)))
    (p:match res
      ((($base  $mtd . $_) . $_)
	  (let* ((rtyp (ll-ftype (_getrettype mtd)))
		 (args (map ll-ftype (_getargs mtd)))
		 (bt (ll-ftype base)))
	    (cons rtyp
		  `( ( ,bt ,rtyp ,@args) ,name)))))))

(function lookup-localmethod (tp name ars env)
  (format env (cls . ht)
    (cond
     ((p:match tp
	((unknown) #t)
	((this) #t)
	((T . =cls) #t)
	(else nil))
      (let* ((a (hashget ht name)))
	(if a
	    (lookup-localmethod-signature (eqv? nm_ctor name) a ars)
	    (lookup-inherited (hashget ht " PARENTS ") name ars)
	    )))
     (else nil))))

(function prc-field (flst name)
  (format flst ((xtp mr))
     (let* ((vtyp (ll-ftype (_getfldtype mr)))
	    (mtp (ll-ftype xtp))
	    (ltr? ((r_tbind "System.Reflection.FieldInfo"
			    "get_IsLiteral") mr)))
       (cons vtyp 
	     (if ltr?
		 `(L ,((r_tbind "System.Reflection.FieldInfo"
				"GetValue" object) mr nil))
		 `( (,mtp ,vtyp) ,name))))))

(function lookup-inherited-fld (lst name)
  (let* ((l (il-types-havefield lst
				(symbol->string name) nil)))
    (if (not l) nil
	(prc-field l name))))

(function lookup-localfield (tp name env)
  (format env (cls . ht)
    (cond
     ((p:match tp
	((unknown) #t)
	((this) #t)
	((T . =cls) #t)
	(else nil))
      (let* ((a (hashget ht name)))
	(p:match a
	  ((field $tp $stt)
	   (cons tp `( ((this) ,tp) (,stt ,name))))
	  (else
           (alet ax (lookup-inherited-fld (hashget ht " PARENTS ") name)
             (if ax ax
                 ;; Otherwise assume object local field
                 (cons '(object) `( ((this) (object)) (,name ,name)))
                 ))))))
     (else nil))))

(function lltnet:lookup-method (tp name argstypes clsenv)
  (let ((atmpt (lookup-localmethod tp name argstypes clsenv))
	(ntp (ll-nettypeT tp)))
    (if atmpt atmpt
     (let* ((atps (map ll-nettype argstypes))
	    (clst (if (eqv? nm_ctor name)
		      (il-type-constructors ntp atps)
		      nil))
	    (mlst (if clst
		      (list (cons ntp clst))
		      (if (null? ntp) nil
			  (il-types-havemethod (list ntp) name
					       nil atps)))))
       (if (and (not mlst) (IsValueTypeT ntp))
	   (cons tp `( ( ,tp ,tp ) ,name))
	   (begin
	     (if (not mlst)
		 (ccerror `(CANT-RESOLVE-METHOD ,tp ,name ,@argstypes)))
       
	     (p:match mlst
	       ((($base  $mtd . $_) . $_)
		(let* ((rtyp (if clst (ll-ftype ntp) 
				 (ll-ftype (_getrettype mtd))))
		       (args (map ll-ftype (_getargs mtd)))
		       (bt (ll-ftype base)))
		  (cons rtyp
			`( ( ,bt ,rtyp ,@args) ,name)))))))))))
    
(function lltnet:lookup-getter (tp name clsenv)
  (try
   (lltnet:lookup-method tp (Sm<< "get_" name) nil clsenv)
   t_Exception
   (fun (e) nil)))
  
(function lltnet:lookup-field (tp name clsenv)
  (let ((atmpt (lookup-localfield tp name clsenv)))
    (if atmpt atmpt
      (let* ((ntp (ll-nettype tp))
	     (flst (il-types-havefield (list ntp)
				       (symbol->string name) nil)))
	(if (not flst)
	    nil
	    (prc-field flst name))))))

(function ll-lookup-enum (tp name)
  (let ((tt (ll-nettype tp)))
    (if (and tt (IsEnum tt))
	(let ((e (getEnum tt (any->string name))))
	  (if e (ll-ftype tt) nil))
	;; unknown type
	;TODO:!!!!
	nil)))

(function lltnet:lookup-newdelegate (tp h name e clsenv)
  (let* ((dt (ll-nettype tp))
	 (ee (p:match e
	       (($x null)
		(cons '(object) '(null)))
	       (else e)))
	 (hh (p:match h
	       ((unknown) (car ee))
	       (else h)))
	 (dtargs (let* ((mmb (a->l (_getmembers dt)))
			(mtds (filter 
			 (fun (x)
			   (and 
			    (t_ass? t_MethodInfo (r_GetType x))
			    (eqv? (string->symbol (_getmtdname x))
				  'Invoke)))
			 mmb)))
		   (if (null? mtds) (ccerror `(DELEGATE-ERROR: ,tp)))
		   (map ll-ftype (_getargs (car mtds ))))))
    `(asm () ( ( ,tp ,tp (object) (ptr)) ,(Sm<< ".ctor"))
	  ,ee ,(cons '(ptr) 
		     `(funref ,(cdr (lltnet:lookup-method hh name 
							  dtargs clsenv))))
	  )))

(function last-subst (bs)
    (let loop ((v bs))
      (p:match v
	((tvardef $_ $_ . $bbs)
	 (loop (car (lasttail bbs))))
	((tvar $_ $_ $_ . $bbs)
	 (loop (car (lasttail bbs))))
	((begin . $bbs)
	 (loop (car (lasttail bbs))))
	((subst $e) e)
	(else (ccerror `(LLTNET:SUBST ,@bs))))))

(function ll-valuetype ( e )
  (cdr
   (<> e
       (ast:visit lltnett expression
	  (iexpression _
	    (
	     (vl node)
	     (ar node)
	     (else `(lbox (,(car e) ,@node)))))))))

;
; Assume that var and tvar expressions are scoped already
;

(function propagate ( ast types clsenv )
  (<> ast
    (ast:revisit loop ((tps types)) lltnet mtdbody
       ((var
	 (let* ((newname (gensymp "var_"))
		(nval (cadr (loop `(e ,value) tps)))
		(ntp (car nval))
		(ntps (cons (list name newname 'var ntp) tps))
		(tx (ll-nettypeT ntp))
		(vtx (and (IsValueTypeT tx)
			  (p:match (cdr nval)
			    ((asm $_ ($t =nm_ctor))
			     (cons `(,t ,nm_ctor) nil))
			    ((asm $_ ($t =nm_ctor) . $rest)
			     (cons `(,t ,nm_ctor) rest))
			    (else nil)))))
	   (if vtx
	       `(vvar ,(car vtx) ,(cdr vtx) ,newname 
		      ,@(map-over in (cut loop <> ntps)))
	       `(tvar ,ntp ,newname ,nval 
		      ,@(map-over in (cut loop <> ntps))))))
	(tvar
	 (let* ((newname (gensymp "var_"))
		(nval (cadr (loop `(e ,value) tps)))
		(ntps (cons (list name newname 'var tp) tps))
		(tx (ll-nettypeT tp))
		(vtx (and (IsValueTypeT tx)
			  (p:match (cdr nval)
			    ((asm $_ ($t =nm_ctor))
			     (cons `(,t ,nm_ctor) nil))
			    ((asm $_ ($t =nm_ctor) . $rest)
			     (cons `(,t ,nm_ctor) rest))
			    (else nil)))))
	   (if vtx
	       `(vvar ,(car vtx) ,(cdr vtx) ,newname 
		      ,@(map-over in (cut loop <> ntps)))
	       (ast:mknode (name newname) (value nval) 
			   (in (map-over in (cut loop <> ntps)))))))
        (tvardef
	 (let* ((newname (gensymp "var_"))
		(ntps (cons (list name newname 'var tp) tps))
		(tx (ll-nettypeT tp))
		)
           (ast:mknode (name newname)
                       (in (map-over in (cut loop <> ntps))))))
	(tryblock
	 (let* ((newname (gensymp "excp_"))
		(bby (loop body tps))
		(ncatch (loop bcatch (cons (list nm newname 'var mask) tps))))
	   (ast:mknode (nm newname) (body bby) (bcatch ncatch))))
	)
       (
	(expression DEEP
	  (
	   (pass v)
	   (v 
	    (let ((nn (find (fun(x) (eqv? (car x) nm)) tps)))
	      (if (not nn) (ccerror `(LLTNET:VARIABLE-NAME: ,nm))
		  (format nn (_ newnm src typp)
		    (cons typp
		     (case src
		       ((arg) `(va ,newnm))
		       ((var) `(vl ,newnm))))))))
	   (ref
	    (let ((nn (find (fun(x) (eqv? (car x) nm)) tps)))
	      (if (not nn) (ccerror `(LLTNET:VARIABLE-NAME: ,nm))
		  (format nn (_ newnm src typp)
		    (cons `(ref ,typp) `(ref ,newnm))))))
	   (am	
	    (let* ((nmtd (lltnet:lookup-method (car e) (mtd.> name)
					       (map car args)
					       clsenv
					       )))
	      (cons (car nmtd)
		    (if (IsValueTypeT (ll-nettypeT (car e)))
			`(avm () ,(cons (car e)
				    (ll-valuetype e))
			      ,(cdr nmtd)
			      ,@args)
			`(am () ,e ,(cdr nmtd) ,@args)))))
	   (asm
	    (let* ((nmtd (lltnet:lookup-method (mtd.> t) (mtd.> name)
						  (map car args)
						  clsenv
						  )))
	      (cons (car nmtd)
		    `(asm () ,(cdr nmtd) ,@args))))

	   (newdelegate
	    (let* ((degt (lltnet:lookup-newdelegate tp h name e clsenv)))
	      (cons tp degt)))

	   (newarr
	    (cons `(array ,(caar args)) `(newtarr ,(caar args)
						  ,@args)))
	   (newtarr
	    (cons `(array ,tp) node))
	   (allocarr
	    (cons `(array ,tp) node))

	   (funref
	    (cons '(ptr) node))
	   (f
	    (let ((m (lltnet:lookup-field (car e)
					  (fld.> name)
					  clsenv
					  )))
	      (if m
		  (cons (car m) 
			`(f ,(ll-boxexpression e) ,(cdr m)))
		  (let ((m1 (lltnet:lookup-getter (car e)
						  (fld.> name)
						  clsenv)))
		    (if m1
			(cadr (loop `(pass-expr 
				      (am () (pass ,e) (( ,(car e) (unknown))
							,(Sm<< "get_" (fld.> name)))))
				      tps))
			(ccerror `(CANT-RESOLVE-FIELD ,(car e) ,(fld.> name)))
			)))))
	   (sf
	    (let* ((e? (ll-lookup-enum (fld.> t) (fld.> name)))
		   (m (lltnet:lookup-field (fld.> t)
					   (fld.> name)
					   clsenv
					   )))
	      (cond
	       (e?
		(cons e? `(enumc ,e? ,(fld.> name))))
	       (m
		(cons (car m)
		      (p:match (cdr m)
			((L $v)
			 (p:match (car m)
			   ((int) `(ic ,v))
			   ((string) `(sc ,v))
			   (else (ccerror `(CONST: ,(car m))))))
			($x `(sf ,x)))))
	       (else
		(let ((m1 (lltnet:lookup-getter (fld.> t)
						  (fld.> name)
						  clsenv)))
		  (if m1
		      (cadr 
		       (loop `(pass-expr (asm () (( ,(fld.> t) (unknown) )
						  ,(Sm<< "get_" (fld.> name)))))
			     tps))
		      (ccerror `(CANT-RESOLVE-STATIC-FIELD ,fld)))
		  )))))
	   (ar
	    (let ((artyp (car e)))
	      (p:match artyp
		((array $t)
		 (cons t node))
		(else (ccerror `(LLTNET:NOT-AN-ARRAY: ,e))))))
	   (ic
	    (cons '(int) node))
	   (fc
	    (cons '(float) node))
	   (dfc
	    (cons '(double) node))
	   (i64c
	    (cons '(long) node))
	   (sc
	    (cons '(string) node))
	   (enumc
	    (p:match tp
	      ((unknown)
	       (let ((tp2 (ll-lookup-enum tp val)))
		 (if (not tp2)
		     (ccerror `(LLTNET:ENUM-LOOKUP ,val)))
		 (cons tp2 (ast:mknode (tp tp2)))))
	      (else
	       (cons tp node))))
	   (null
	    (cons '(object) node))
	   (true
	    (cons '(bool) node))
	   (false
	    (cons '(bool) node))
	   (ldthis
	    (cons '(this) node))
	   (bin
	    (case op
	      ((+ - * / mod > < >= <= == != or and xor binand binor lshift rshift)
	       (cons (car l) node))
	      (else (ccerror `(LLTNET:NOT-IMPLEMENTED ,node)))))
	   (un
	    (case op
	      ((- not bitnot isnull) (cons (car v) node))
	      (else (ccerror `(LLTNET:NOT-IMPLEMENTED ,node)))))
	   (type
	    (cons tp e))
	   (typecast
	    (cons tp node))
	   (typetoken
	    (cons '(T . System.Type) node))

	   (fieldtoken
	    (cons '(T . System.RuntimeFieldHandle) node))

	   (typeof
	    (cons '(T . System.Type) node))
	   (istype
	    (cons tp node))
	   
	   (marshal
	    (cons tp node))
	   (embedded-begin-1
	    (let* ((ll (last-subst bdy)))
	      (cons (car ll) node)))
	   (else (ccerror `(LLTNET:STUPID ,node)))
	   ))))))

(function propagate-wrapper ( ast types clsenv )
  (if (null? (shashget (getfuncenv) 'debug-lltnet-embedded)) nil
      (begin
        (println "EMB:BODY")
        (iter writeline ast)
        (println "--------")))
  (propagate ast types clsenv)
  )

; Toplevel wrapper
(function isstatic? (acc)
  (if
   (find (fun (x)
	   (p:match x ((static) #t) (else nil))) acc)
   #t
   nil))

(function lltnet-members (members mm)
  (foreach (m members)
      (<> m
	  (ast:iter lltnet classmember
	     (classmember _
		 ((method
		   (let* ((newnm (gensymp "mtd"))
			  (mng (lltnet-manglename name (map car args))))
		     (hashput mm 
			      name
			      (cons
			       `(method ,newnm ,ret ,(map car args))
			       (hashget mm name)))
		     (hashput mm mng newnm)))
		  (field
		   (hashput mm name `(field ,tp ,(isstatic? acc))))
		  (custom nil)
                  (initfield nil)
                  ))))))

(function lltnet-prepare-sets ( bdy )
  (<> bdy
    (ast:visit lltnett mtdbody
     (mtdbody DEEP
       ((set
	 (let ((xx 
		(p:match (cdr lvalue)
		  ((f ($tp vl $nm) . $y)
		   (let ((tx (ll-nettypeT tp)))
		     (if (IsValueTypeT tx)
			 `(xset (,tp vf ,nm ,@y) ,e)
			 nil)))
		  ((f ($tp ar $e1 $e2) . $y)
		   (let ((tx (ll-nettypeT tp)))
		     (if (IsValueTypeT tx)
			 `(xset (,tp vaf ,tp ,e1 ,e2 ,@y) ,e)
			 nil)))
		  (else nil))))
	   (if (null? xx)
	       `(xset ,lvalue ,e)
	       xx)))
	(else node))))))

;TODO: handle non-static methods
(function lltnet-prepare-member (name mm member)
  (<> member
    (ast:visit lltnet classmember
      (classmember _
	((field node)
         (initfield node)
	 (custom node)
	 (method
	  (ast:mknode 
	   (name 
	    (let ((mn (hashget mm
			(lltnet-manglename name (map car args)))))
	      (if (or (eqv? name nm_ctor)
                      (eqv? name nm_cctor))
                  `(CTOR ,mn)
		  (Sm<< name "//" mn))))
	   (body
	    (lltnet-prepare-sets
	    (propagate-wrapper
	     (scopetrans (lltnet-expand0 body))
	     (let* ((sttk? (find (fmt (x) (eqv? x 'static)) acc))
		    (ff (if sttk? (fun (x) x) (fun (x) (+ x 1)))))
	       (mapi
		(fun (i a)
		  `(,(cadr a) ,(ff i) arg ,(car a)))
		args))
	     (cons name mm)))))))))))

(function lltnet-prepare-top ( src )
  (when (shashget (getfuncenv) 'debug-compiler-lltnet)
        (println 'LLTNET-TOP:------------------)
        (iter writeline src)
        (println '----------------LLTNET-TOP---))
  (let ((membtypes (mkhash)))
    (<> src
	(ast:visit lltnet class
	  (class _
	   (begin
	     (hashput membtypes " PARENTS "
		      (cons (ll-nettype `(T ,@parent))
			    (map (fun (x)
				   (ll-nettype `(T ,@x))) interfaces)))
	     (lltnet-members members membtypes)
	     `((,name ,parent ,@interfaces)
	       ,@(map-over members
			   (fun (m)
			     (lltnet-prepare-member name membtypes m)))))))
	)))



(function lltnet-prepare-embedded ( args body )
  
  (lltnet-prepare-sets
   (propagate-wrapper
    (scopetrans (lltnet-expand0 body))
    (map-over args
      (fmt (nm nw tp)
	`(,nm ,nw var ,tp))) 
    (cons nil (mkhash)))))

(function lltnet-prepare-embedded-with-top ( args body liftmembs )
  (with-hash (membtypes)
     (lltnet-members liftmembs membtypes)
     (let ((mmscode
	    (foreach-map (m liftmembs)
			 (lltnet-prepare-member 'this membtypes m))))
       (cons mmscode
        (lltnet-prepare-sets
	 (propagate-wrapper
	  (scopetrans (lltnet-expand0 body))
	  (map-over args
		    (fmt (nm nw tp)
			 `(,nm ,nw var ,tp))) 
	  (cons nil membtypes)))))))

