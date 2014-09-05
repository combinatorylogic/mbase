;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define simple-threshold 15)

(force-class-flush)

(cmacro tblsymget (tbl min max els sym level)
  (with-syms (done else)
  `(n.asm (,tbl ,sym)
      (expr ,tbl)
      (Castclass ,t_ByteArray)
      (expr ,sym)
      ,(_ldc_i4 level)
      (Call ,mtd_hash)
      ,(_ldc_i4 min)
      (Sub)
      (Dup)
      ,(_ldc_i4 0)
      (Blt (label ,else))
      (Dup)
      ,(_ldc_i4 (- max min))
      (Bgt (label ,else))
      (Ldelem_I1)
      (Br (label ,done))

    (label ,else)
      (Pop) (Pop)
      ,(_ldc_i4 els)
    (label ,done)
    )))

(cmacro tblget (tbl i)
  (with-syms (ii)
    `(let ((,ii ,i))
      (n.asm (,tbl ,ii)
          (expr ,tbl)
          (Castclass ,t_ByteArray)
          (expr ,ii)
          (Unbox ,t_int)
          (Ldind_I4)
          (Ldelem_I1)
          (Box ,t_int)
          ))))

(cmacro __symhash (sm lev)
  (with-syms (s)
    `(let ((,s ,sm))
       (n.asm (,s)
         (expr ,s)
         ,(_ldc_i4 lev)
         (Call ,mtd_hash)
         (Box ,t_Int32)))))

(cmacro __isymhash (min max els sym level)
  (with-syms (done else)
  `(n.asm (,sym)
      (expr ,sym)
      (Castclass ,t_Symbol)
      ,(_ldc_i4 level)
      (Call ,mtd_hash)
      ,(_ldc_i4 min)
      (Sub)
      (Dup)
      ,(_ldc_i4 0)
      (Blt (label ,else))
      (Dup)
      ,(_ldc_i4 (- max min))
      (Bgt (label ,else))
      (Br (label ,done))

    (label ,else)
      (Pop)
      ,(_ldc_i4 els)
    (label ,done)
    )))

(cmacro switch (var . opts)
  ;TODO: verify if opts starts with 0 and increments by 1
  (let* ((oopts (map cadr (qsort (fun (a b) (< (car a) (car b))) opts))))
    `(inner.switch ,var ,@oopts)))

(cmacro with-table (nt . body)
  (format nt (nm tbl)
     `(let ((,nm (quote ,(corelib:mkbytevector tbl))))
        ,@body)))

;----------------

; Entry format: (symbol label)
(function cc-htab (entries)
  (let loop ((es entries) (l 0))
    (let* ((e1 (map-over es (fmt (s lbl) `(,(symhash s l) ,s ,lbl))))
           (e1h (with-hash (hs)
                   (iter-over e1 (fmt (n s lbl)
                                   (hs! n (cons `(,s ,lbl) (hs> n)))))
                   (collector (add cget)
                   (collector (add1 cget1)
                   (hashiter (fun (k v)
                               (let* ((nk (S->N k))
                                      (ln (length v)))
                                 (if (> ln 1)
                                     (add `(,nk ,v))
                                     (add1 `(,nk ,@v)))))
                             hs)
                   (list (cget1) (cget)))))))
      `(,l ,@(car e1h)
            ,@(let ((nl (+ l 1)))
                (map (fmt (nk v)
		       (if (and (> (length v) simple-threshold)
				(> nk 0))
			   `(,nk ,(loop v nl))
			   `(,nk (() ,@v))))
                  (cadr e1h)))))))


; Tree format (<level> (<hash> (<sym> <label)) | (<hash (<level> ...)) *)

(function cc-mark (vn tree erlabel)
  (let loop ((t tree))
    (format t (lev . entries)
      (let* ((sorted (qsort (fun (a b) (< (car a) (car b))) entries))
             (nens (filter (fmt (a (v . _)) 
				(or (null? v) (number? v))) sorted)))
        `(symcase (,lev ,vn ,(null? nens) ,erlabel)
          ,@(map-over sorted
               (fmt (k (s . v))
		 (cond
		  ((null? s)
		   `(,k () (simple-case ,vn
			     ,@(map-over v
				 (fmt (s e)
				    `(,s (n.goto ,e))))
			     (else (n.goto ,erlabel)))))
		  ((symbol? s)
                   `(,k ,s (n.goto ,@v)))
		  (else
                   `(,k () ,(loop `(,s ,@v))))))))))))

(cmacro symcase (hdr . opts)
   (format hdr (lev vn simple? exit)
      (if (and simple? (< (length opts) simple-threshold))
          `(simple-case ,vn
             ,@(map-over opts
                  (fmt (h k v)
                    `(,k ,v)))
             (else (n.goto ,exit)))
          `(switch-case (,lev ,vn ,exit)
             ,@(map-over opts
                  (fmt (h k v)
                     (if k
                         `(,h (if (eqv? ,vn (quote ,k)) ,v
                                  (n.goto ,exit)))
                         `(,h ,v))))))))

(cmacro simple-case (vn . opts)
   (let loop ((o opts))
     (p:match o
       (((else $v)) v)
       ((($k . $act) . $tl)
        `(if (eqv? ,vn (quote ,k)) (begin ,@act) ,(loop tl))))))

(cmacro switch-case (hdr . opts)
   (format hdr (lev vn exit)
      (let* ((minmax (let ((min (mkref -1))
                           (max (mkref -1)))
                       (foreach (vv opts)
                         (format vv (v . _)
                                 (if (or (< v (deref min))
                                         (< (deref min) 0)) (r! min v))
                                 (if (> v (deref max)) (r! max v))))
                       (list (deref min) (deref max))))
             (min (car minmax))
             (max (cadr minmax))
             (rtable (with-hash (hs)
                       (iter-over opts
                          (fmt (v act)
                            (hs! v act)))
                       (formap (i min (+ max 1))
                          (alet t (hs> i)
                                (if t t exit)))))
             (last (+ 1 (- max min))))
        `(inner.nswitch (__isymhash ,min ,max ,last ,vn ,lev)
              ,@rtable
              (n.goto ,exit)
              ))))

(function cc-newcase-f (vn opts)
  (with-syms (pfx exit)
  (alet ex (mkref '(nil))
  (collector (add get)
  (with-hash (sh)
     (let loop ((o opts) (i 0))
       (when o
        (alet lbl (Sm<< pfx "__" i)
           (format o ((ss . act) . tl)
             (if (eqv? ss 'else) 
                 (begin
                   (r! ex act)
                   )
                 (begin
                   (foreach (s ss)
                     (sh! s lbl))
                   (add `(,lbl (begin ,@act)))
                   (loop tl (+ i 1))))))))
     (let* ((intry
             (cc-mark vn 
                      (cc-htab (hashmap (fun (s v) `(,(Sm<< s) ,v)) sh))
                      exit))
            (icode
             `(inner.labels (,exit (begin ,@(deref ex)))
                      ,@(get))))
       `(begin
          ,intry
          ,icode)))))))

(macro cc-newcase (v . opts)
  (with-syms (vv)
    `(let ((,vv ,v))
       ,(cc-newcase-f vv opts))))



(define cc-newgencase-elsesym (Sm<< "            " (gensym) "----- - - -"))

(macro cc-newgencase (v . opts)
  (with-syms (vv v1)
    `(let* ((,vv ,v)
            (,v1 (if (null? ,vv) 
                     (quote ,cc-newgencase-elsesym)
                     (if (symbol? ,vv) ,vv (quote ,cc-newgencase-elsesym))
                     )))
       ,(cc-newcase-f v1 opts))))

(unit-test 3 (cc-newcase 'a ((x y z) 1) ((u v w) 2) ((a b c) 3)) 3)
(unit-test 3 (cc-newcase 'a
                         ((x11 y11 z11) 1) ((u11 v11 w11) 2) ((a11 b11 c11) 3)
                         ((x12 y12 z12) 1) ((u12 v12 w12) 2) ((a12 b12 c12) 3)
                         ((x13 y13 z13) 1) ((u13 v13 w13) 2) ((a13 b13 c13) 3)
                         ((x14 y14 z14) 1) ((u14 v14 w14) 2) ((a14 b14 c14) 3)
                         ((x15 y15 z15) 1) ((u15 v15 w15) 2) ((a15 b15 c15) 3)
                         ((x16 y16 z16) 1) ((u16 v16 w16) 2) ((a16 b16 c16) 3)
                         ((x17 y17 z17) 1) ((u17 v17 w17) 2) ((a b17 c17) 3)
                         ((x18 y18 z18) 1) ((u18 v18 w18) 2) ((a18 b18 c18) 3)
                         ((x19 y19 z19) 1) ((u19 v19 w19) 2) ((a19 b19 c19) 3)
                         ((x20 y20 z20) 1) ((u20 v20 w20) 2) ((a20 b20 c20) 3)
                         )
           3)

