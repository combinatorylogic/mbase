;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{Caching small functions and closures}
;-

;-
;- The basic idea is to look up short functions and closures by their textual
;- representation as a hash key. In order to do this, all the
;- names must be normalised, including labels. [[cc:fc:mapnames]] will
;- do the job. 
;-
;- BackendAsm constructions normally will have entities which can't be
;- represented in a string form. In order to get rid of them, we can use
;- tokens. This will work for types, methods and fields. Other entities should
;- raise an error condition to stop all the caching efforts for this particular
;- function or closure.
;-

(define t_FieldInfo (sdotnet "System.Reflection.FieldInfo"))
(define t_MethodBase (sdotnet "System.Reflection.MethodBase"))

(force-class-flush)
(function s.typeinfo (tp)
  ((r_bind "System.RuntimeTypeHandle" "get_Value")
   ((r_bind t_Type "get_TypeHandle") tp)))

(function s.methodbase (tp)
  ((r_bind "System.RuntimeMethodHandle" "get_Value")
   ((r_bind t_MethodBase "get_MethodHandle") tp)))

(function s.fieldinfo (tp)
  ((r_bind "System.RuntimeFieldHandle" "get_Value")
   ((r_bind t_FieldInfo "get_FieldHandle") tp)))

(force-class-flush)

(function cc:fc:handleasm (acode rename)
  (foreach-map (ac acode)
    (let loop ((a ac))
      (p:match a
        ((local $nm . $rest)
         `(local ,(rename nm) ,@(loop rest)))
        ((var $nm) `(var ,(rename nm)))
        ((label $nm) `(label ,(rename nm)))
        (($hd . $tl) (cons (loop hd) (loop tl)))
        (() nil)
        (else
         (alet tp (r_GetType a)
          (cond
            ((t_ass? t_Type tp) `($TI ,(s.typeinfo a)  ))
            ((t_ass? t_MethodBase tp)
             `($MI ,(s.methodbase a)))
            ((t_ass? t_FieldInfo tp)
             `($FI ,(s.fieldinfo a)))
            (else a))))
         ))))

;= Now, back to the names handling:

(function cc:fc:mapnames (tdef)
  (with-sequence (x)
  (with-hash (h)
    (alet rname (fun (n)
                  (alet tst (h> n)
                        (if tst tst
                            (alet res (x)
                                  (h! n res)
                                  res))))
      (cc:mbcoreast:visit liftop tdef
        (expr DEEP
          (
           (Label (ast:mknode (id (rname (Sm<< "label: " id)))))
           (GotoLabel (ast:mknode (id (rname (Sm<< "label: " id)))))
           (Var (ast:mknode (id (rname id))))
           (Arg (ast:mknode (id (rname id))))
           (Recref (ast:mknode (id 'self)))
           (Clenv (ast:mknode (id (rname id))))
           (XSet  (ast:mknode (nm (rname nm))))
           (PatchClosure (ast:mknode (nm (rname nm))))
           (Fun (ast:mknode (recname 'self) (args (map rname args))))
           (BackendAsm (ast:mknode (body (cc:fc:handleasm body rname))))
           (else node)))
        (letarg DEEP (ast:mknode (nm (rname nm))))
        (captcharg DEEP (ast:mknode (ref (rname ref))))
        (liftop DEEP
          ((Closure (ast:mknode (name '*) (usename '*) (args (map rname args))))
           (Simple (ast:mknode (name '*) (usename '*)))
           (else node)))
        )))))

;- 
;- We already have a node counting feature from an optimiser layer, so it
;- can be reused in order to estimate, if there's a reason to cache a functions.
;- The final decision is made using the length of a resulting hash string, which
;- depends not only on a number of nodes, but on a content of BackendAsm blocks
;- and length of constants as well.
;-

(function cc:estimate:top ( liftop )
  (let ((res (cons 0 nil)))
    (cc:mbcoreast:iter liftop liftop
       (expr DEEP (forall (set-car! res (+ (car res) 1)))))
    (car res)))

;= Current threshold: 400 ast nodes or 10000 characters.
(function cc:decide ( liftop )
  (alet est (cc:estimate:top liftop)
    (when (< est ##optimise-cache-threshold)
       (let* ((src (cc:fc:mapnames liftop))
              (tsrc (to-string src))
              (len ((r_tbind string "get_Length") tsrc))
              )
         (when (< len ##optimise-cache-length-threshold)
            (list liftop src tsrc))))))

;-
;- Now, the whole cache lookup sequence is following:
;- 1) Make a list of short functions
;- 2) Collapse similar functions, substitute
;- 3) Find cache hits, substitute, rebuild normalised list
;- 4) Repeat 3) unless no more hits are found
;- 5) Pass the remaining items to be cached after the compilation
;-


;= A helper function --- retrieve an identity
(function cc:identity ( liftop )
  (cc:mbcoreast:visit liftop liftop
    (liftop _
      ((Simple name)
       (Closure name)
       (else nil)))))

;= Select all expressions that qualifies
(function cc:cachefilter ( exprs )
  (foreach-mappend (e exprs)
    (let* ((id (cc:identity e))
           (tst (when id (cc:decide e))))
      (if tst (list tst) nil))))

;= Select similar entries in one bunch, returns a list of cacheable entries to remain and a list of
;= entries to substitute.
(function cc:cachecollapse ( fexprs dbg )
  (with-hash (hs)
  (collector (radd rget)
  (collector (cadd cget)
     (foreach (f fexprs)
	 (format f (src _ txt)
	   (alet tst (hs> txt)
	     (if tst
		 (alet idd (cc:identity src)
		       (when dbg (println (S<< "Cache collapsing: " idd " to " tst)))
		       (cadd (list idd tst)))
		 (alet idd (cc:identity src)
		   (radd f)
		   (hs! txt idd))))))
     (list (rget) (cget))))))

;= Build a list of cache hits
(function cc:cachehits ( cache fexprs )
  (use-hash (cache)
  (collector (add get)
    (iter-over fexprs
       (fmt (src _ txt)
          (alet tst (cache> txt)
             (when tst
               (add (list (cc:identity src) tst))))))
    (get))))

;= Substitute selected references, remove optimised out definitions
(function cc:cachepass ( parenv reslt code dbg )
  (with-hash (xnames)
    (foreach (r reslt)
      (when dbg (println (S<< "Cache hit: " (cadr r))))
      (env:inc: parenv ctr-cache)
      (xnames! (car r) (cadr r)))
    (alet replace
      (fun (id)
        (alet tst (xnames> id)
              (if tst tst id)))
      (foreach-mappend (c code)
        (if (alet nn (cc:identity c)
                  (if nn (xnames> nn) nil)) nil
            (wrap
             (cc:mbcoreast:visit liftop c
               (expr DEEP
                     ((Glob (ast:mknode (id (replace id))))
                      (Funref (ast:mknode (id (replace id))))
                      (else node)))
	       (liftop DEEP
		       ((Funref (ast:mknode (iname (replace iname))))
			(else node)))
               (captcharg DEEP (ast:mknode (ref (replace ref))))))
            )))))

;= Fill in the cache
(function cc:tocache ( cacheenv toc code dbg )
  (use-hash (cacheenv)
  (foreach (t toc)
    (format t (src _ txt)
      (cacheenv! txt 'TEMP)
      (when dbg (println (S<< "Caching: " (cc:identity src) " AS " txt)))
      (cacheenv! " temporary pool " (cons (list txt (cc:identity src))
                                          (cacheenv> " temporary pool ")))))
  code))

;= Perform cache pass as many times as necessary
(function cc:cacheloop ( parenv cacheenv code0 )
  (let loop ((code code0) (tocache (cc:cachefilter code0))
	     (dbg (shashget (getfuncenv) 'debug-compiler-cache)))
      (if (null? tocache)
          code
          (let*  ((nw (cc:cachecollapse tocache dbg))
		  (remains (car nw))
		  (hits0 (cadr nw))
		  (hits1 (cc:cachehits cacheenv remains))
		  (hits0prim (with-hash (hs)
				 (foreach (h hits1)
				   (hs! (car h) (cadr h)))
				 (foreach-map (h hits0)
				   (format h (f t)
				      (alet x (hs> t)
					(list f (if x x t)))))))
		  (hits (append hits0prim hits1)))
                (if (null? hits)
                    (cc:tocache cacheenv remains code dbg)
                    (alet subst (cc:cachepass parenv hits code dbg)
                          (loop subst (cc:cachefilter subst) dbg)
                          ))))))

;- Interface functions:

(function cc:cachetop (env e)
  (if (shashget (getfuncenv) 'compiler-optimise-cache)
      (cc:cacheloop env (hashget env '-fun-cache-) e)
      e))

(define compiler-optimise-cache ##optimise-cache)

