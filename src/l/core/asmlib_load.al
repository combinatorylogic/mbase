;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define _t_call_generic (ctime `(dotnet ,(S<< generic_pfx "CALL_GENERIC"))))
(define m_Call_Generic_00 (r_mtd _t_call_generic "call_generic"
                              t_object_array t_object))

(define ms_Call_Generics_00 
  (ctime `(list
           ,@(formap (i 0 SMAXARGS) 
                     `(r_mtd _t_call_generic ,(buildstring "call_generic__" i) ,@(formap (j 0 i) 't_object) t_object)))))

(define ms_Call_RevGenerics_00 
  (ctime `(list
           ,@(formap (i 0 SMAXARGS) 
                     `(r_mtd _t_call_generic ,(buildstring "call_r_generic__" i) ,@(formap (j 0 i) 't_object) t_object)))))

;;; now register the delegate callback:
(let ((register (r_sbind _t_call_generic "register")))
  (register))
