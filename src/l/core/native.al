;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(using ("System.Reflection" "System.Reflection.Emit" "System.Runtime.InteropServices")
    (net.types
     MethodBase
     MethodBuilder
     CallingConventions
     CharSet
     CallingConvention ;; Note the difference CallingConvention (not CallingConventions)
     MethodImplAttributes
     )
    
    (define _define_pinvoke (r_tbind t_TypeBuilder "DefinePInvokeMethod" string string
				     t_MethodAttributes t_CallingConventions t_type "System.Type[]" t_CallingConvention t_CharSet))

    (define _set_impl_flags (r_tbind t_MethodBuilder "SetImplementationFlags" t_MethodImplAttributes))

    (define _get_impl_flags (r_tbind t_MethodBuilder "GetMethodImplementationFlags"))

    (function emit-pinvoke (cls nm dllname attrs conv ret artyps)
      (let* ((iattr (enumOr t_MethodAttributes attrs))
	     (iconv (enumOr t_CallingConventions conv))
	     (ichst (enumOr t_CharSet '()))
	     (iccnv (enumOr t_CallingConvention '(Winapi)))
	     (args (if (null? artyps)
		       (anew t_Type 0)
		       (mkvector artyps)))
	     (mtd (_define_pinvoke cls nm dllname iattr iconv ret args iccnv ichst))
	     (en (getEnum t_MethodImplAttributes "PreserveSig")))
	(_set_impl_flags mtd (enum-or en (_get_impl_flags mtd)))))

    (function f.emit-pinvoke (cls dllnm fncnm rtype argtyps)
      (emit-pinvoke cls fncnm dllnm '(Public Static PinvokeImpl) 
		    '(Standard) rtype argtyps))

(function f.make-pinvoke (dll nm ret ars)
  `(pinvoke ,dll ,nm ,ret ,ars))

(macro native imports
  ("Generates a class with native P$/$Invoke entries."
   "Entry format is:"
   " [[(import dll-name func-name return-type arg-type ...)]]"
   "Optional class name parameter:"
   "  (classname Namespace.Class)"
   )
  (let* ((cnmr (mkref (gensym)))
	 (body (foreach-mappend (i imports)
			(p:match i
			  ((import $dll $nm $ret . $args)
			   (let* ((rett (read-int-eval `(r_typerx ,ret)))
				  (arst (foreach-map (a args)
					      (read-int-eval `(r_typerx ,a)))))
			     (list
			      (f.make-pinvoke (S<< dll) (S<< nm)
					     rett arst
					     ))))
			  ((classname $nm) (begin (r! cnmr nm) nil))
			  (else nil)
			  )))
	 (cnm (deref cnmr))
	 (icode `(:classwrap ,cnm ()
		    ,@body))
	 )
    `(top-begin
       ,icode
       (force-class-flush)
       (net.types ,(Sm<< cnm))
       ,@(foreach-mappend (i imports)
	   (p:match i
	     ((import $_ $nm $ret . $args)
	      (let* ((ars (map (fun (x) (gensym)) args)))
		`((define ,(Sm<<  nm)
		    (r_tsbind ,(Sm<< "t_" cnm) ,(S<< nm) ,@args))))))))))

(set-car! _pinvoke_maker_hook f.emit-pinvoke)
