;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(Section "Misc stuff")

(define t_FieldInfo (sdotnet "System.Reflection.FieldInfo"))

(macro class-match (v . args)
  (with-syms (nv)
    `(alet ,nv (r_GetType ,v)
       (cond
        ,@(foreach-map (a args)
            (format a (tp . act)
               (if (eqv? tp 'else) `(else (begin ,@act))
                   `((t_ass? ,tp ,nv) (begin ,@act)))))))))

(not.function s.typeinfo ((System.Type tp))
  (return ((object)((tp@get_TypeHandle)@get_Value))))

(not.function s.methodinfo ((System.Reflection.MethodInfo tp))
  (return ((object)((tp@get_MethodHandle)@get_Value))))

(not.function s.fieldinfo ((System.Reflection.FieldInfo tp))
  (return ((object)((tp@get_FieldHandle)@get_Value))))

(function mc-nonstatic (acc)
  (not 
   (filter (fun (x)
             (p:match x
               ((static) #t)
               (else nil))) acc)))

(macro mixed-class (name . body)
  ("(mixed-class name [(extends ...)] [(implements ...)] ...)"
   "defines a class with both Not.Net and MBase method definitions."
   "Similar to {\tt not.class} macro, with an addition of {\tt lmethod} entry."
   )
  (let* ((f (tailsplit (fun (l)
                         (p:match l
                           ((lmethod . $_) #t)
                           (else nil))) body))
         (nbody (cdr f))
         (lbody (car f))
         (ncode
          (foreach-map (n lbody)
            (p:match n
              ((lmethod $acc $type $name0 $args . $body)
               (alet ns (mc-nonstatic acc)
               (format (p:match name0
                                     (($a $b) (list a b))
                                     (else (list (gensym) name0)))
                       (nnm name)
                 `((recfunction ,nnm (,@(if ns '(this) nil)
                                   ,@(map cadr args)) ,@body)
                   (method ,acc ,type ,name ,args
                           (return ((,type)
                                    (mbase-fn ,nnm 
                                              ,@(if ns
                                                    `(((object) self))
                                                    nil)
                                              ,@(foreach-map (a args)
                                                  (format a (atyp anam)
                                                          anam
                                                          )))
                                    )))))))))))
    `(top-begin
       ,@(map car ncode)
       (force-class-flush)
       (not.class ,name ,@nbody ,@(map cadr ncode)))
    ))


    
(function lookupresource (xxnm nm)
  (alet xtp (dotnet xxnm)
   (try
   (list xtp
    (not.neth ((string xxnm) (string nm) (System.Type xtp))
       (leave
        (new System.IO.MemoryStream
          (((array byte))
           ((new System.Resources.ResourceManager
                 xxnm (xtp@get_Assembly))
            @ GetObject nm))))))
   t_Exception
   (fun (e)
     (list 'ERROR xtp (->s e))))))

(macro include-res (xnm nm)
  (let* ((xxnm (S<< xnm))
         (str0 (lookupresource xxnm nm))
         (ms(cadr str0))
         (_ 
          (when (eqv? 'ERROR (car str0)) 
                (ccerror `(CAN NOT LOCATE RESOURCE ,nm))))
         (strm 
          (not.neth ((System.IO.Stream ms))
           (leave
             (new System.IO.StreamReader ms))))
         (fi (mkreader strm))
         (res 
          (collector (add get)
             (let loop ()
               (alet r (xio-read fi)
                     (if (null? r) nil
                         (begin (add r)
                                (loop)))))
             (get))))
    (xio-close fi)
    `(top-begin ,@res)))

(function read-lisp-file (fn)
  "Read list of s-expressions from a given file"
  (let* ((fi0 (io-open-read fn))
         (fi  (mkreader fi0))
         (res (let loop () (let ((r (xio-read fi)))
				(if (null? r) nil
				    (cons r (loop))))))
         )
    (xio-close fi)
    res))

(function int->byte (i)
  (not.neth ((int i))
    (leave ((object)
            ((byte)i)))))

(set-car! int->byte-hook int->byte)

(function intbytelist (data)
  (foreach-mappend (d data)
    ;;;TODO!
    `(,d 0 0 0)))

(macro not.staticdata (name type . data) 
  ("Initialise a static data array. Only byte and int types are allowed.")
  (let* ((tdata (case type
                  ((byte) data)
                  ((int) (intbytelist data))))
         (tlength (length data)))
   (with-syms (s1)
    `(top-begin
       (not.class ,s1 (extends System.Object)
         (initfield ,(Sm<< s1 "_f") ((public) (static))
                    ,@tdata))
       (define ,name
         (not.neth ()
          (varr = (mkarr ,type ,tlength))
          (System.Runtime.CompilerServices.RuntimeHelpers
                  @ InitializeArray 
                    ((System.Array)varr)
                    ((System.RuntimeFieldHandle)
                     (fieldtoken
                      (,s1 # ,(Sm<< s1 "_f")))))
          (leave ((object)varr))))))))

