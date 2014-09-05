;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define IsPrimitive (r_tbind "System.Type" "get_IsPrimitive"))
(define IsClass (r_tbind "System.Type" "get_IsClass"))
(define IsEnum (r_tbind "System.Type" "get_IsEnum"))
(define IsValueType (r_tbind "System.Type" "get_IsValueType"))
(define UnderlyingSystemType (r_tbind "System.Type" "get_UnderlyingSystemType"))

(define IsVirtual (r_tbind "System.Reflection.MethodInfo" "get_IsVirtual"))

(define _ldind_ar `((,t_Boolean . I1)
                    (,t_Byte . I1)
                    (,t_SByte . I1)
                    (,t_Int16 . I2)
                    (,t_UInt16 . U2)
                    (,t_Int32 . I4)
                    (,t_UInt32 . U4)
                    (,t_Int64 . I8)
                    (,t_UInt64 . I8)
                    (,t_Char . I4)
		    (,t_IntPtr . I)
                    (,t_Double . R8)
                    (,t_Single . R4)))

(function _ldind (ttp)
 (let ((tp (if (IsEnum ttp) t_Int32 ttp)))
  (let ((x (filter (fun (x) (t_eq tp (car x))) _ldind_ar)))
   (if (null? x) `(Ldobj ,ttp)
    (wrap 
     (string->symbol 
       (buildstring 'Ldind_ (cdar x))))))))



(expand-if (not ##dotnet-2)
(function _unbox_type (tp)
  `((Unbox ,tp) ,(_ldind tp)))

)
(expand-if ##dotnet-2

(function _unbox_type (tp)
  `((Unbox_Any ,tp)))

)

(function isBoxed (tp)
 (or (IsPrimitive tp) (IsEnum tp)
     (IsValueType tp)
 ))

(macro r_tbind0 (tp class method . args)
   (let* ((alist (mapi (fun (i x) (cons
                                    (string->symbol (buildstring "arg-" i))
                                    (r_typerx x)))
                        args))
          (metod (try (r_mtdf class method args) t_Exception (fun (_) nil)))
          (o (if (> tp 0) '(*O*) nil))
          (classt (r_typerx class))
          (rett (RetType metod))
	  (isbool (r_boolmethodp metod))
          )
     (if (null? metod) 
       `(r_bind0 ,tp ,class ,method ,@args)
       `(fun (,@o ,@(map-car alist))
           (n.asm (,@o ,@(map-car alist))
             ,@(if (null? o) o `((expr *O*) 
                     ,@(if (isBoxed classt) (_unbox_type classt)
                                          `((Castclass ,classt)))
               ))

             ,@(pre-emit (foreach-map (a alist)
                             `((expr ,(car a))
                                ,@(if (isBoxed (cdr a))
				      (_unbox_type (cdr a))
                                     `((Castclass ,(cdr a))))
                               )
                         ))
             ,@(if (IsVirtual metod) `((Callvirt ,metod)) `((Call ,metod)))
	     ,@(if isbool
		 (with-syms (TRUE NEXT)
	           `((Brtrue (label ,TRUE))
		     (Ldnull)
		     (Br (label ,NEXT))
		     (label ,TRUE)
		     (Ldsfld ,fld_True)
		     (label ,NEXT)))
                 (if (void-p metod) '((Ldnull)) 
		     (if (not (isBoxed rett))
			 `(
			   (Castclass ,t_object)
			   ) 
			 `((Box ,rett)))
		     ))
	     )))))

(macro r_constr (tp . args)
 (let* ((alist (mapi (fun (i x) (cons
                                    (string->symbol (buildstring "arg-" i))
                                    (r_typerx x)))
                        args))
        (mettod (r_getconstructorf tp args))
        (rett (r_typerx tp))
        )
   `(fun ,(map-car alist)
      (n.asm ,(map-car alist)
           ,@(pre-emit (foreach-map (a alist) 
                             `((expr ,(car a))
                                ,@(if (isBoxed (cdr a))
				      (_unbox_type (cdr a))
                                     `((Castclass ,(cdr a))))
                               )
                         ))
           (Newobj ,mettod)
           ,@(if (not (isBoxed rett))
                 `(
                   (Castclass ,t_object)
                  ) 
           `((Box ,rett)))
        ))))




(cmacro r_tbind rest `(r_tbind0 1 ,@rest))
(cmacro r_tsbind rest `(r_tbind0 0 ,@rest))

(cmacro new (cls . args)
   `((r_constr ,cls ,@(map-car args)) ,@(map cadr args)))

