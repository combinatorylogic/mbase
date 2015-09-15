;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define r_getFields (r_tbind "System.Type" "GetFields"))
(define r_getField (r_tbind "System.Type" "GetField" string))
(define r_GetType (r_tbind "System.Object" "GetType"))
(define r_FI-GetValue (r_tbind "System.Reflection.FieldInfo" "GetValue" object))

(define r_FI-SetValue (r_tbind "System.Reflection.FieldInfo" "SetValue" object object))

(function type_array (tp)
   (r_GetType (anew tp 0)))

(function r_GetTypeX (x) (if (null? x) t_object (r_GetType x)))
(function ->  (vl typ fld)
   "Gets the field of a given type of [vl]"
   (r_FI-GetValue (r_getField (r_typer typ) (any->string fld)) vl))

(function :-> (vl fld)
   "Get the field [fld] of the object [vl]."
   (r_FI-GetValue (r_getField (r_GetType vl) (any->string fld)) vl))

(function <- (vl typ fld val)
   "Set the field [fld] value of the object [vl] into [val], assuming the given [vl] type [typ]."
   (r_FI-SetValue (r_getField (r_typer typ) fld) vl val))

(function <-: (vl fld val)
   "Set the field [fld] value of the object [vl] into [val]. Type of [vl] is evaluated using [GetType]."
   (r_FI-SetValue (r_getField (r_GetType vl) fld) vl val))

(function s<-: (vl fld val)
   "Sets a property value."
   (let* ((t (r_GetType vl))
          (b (r_bind t (string-append "set_" fld) (r_GetType val))))
     (b vl val)))

(function s-> (typ fld)
   "Get a static field value."
   (r_FI-GetValue (r_getField (r_typer typ) fld) nil))

(function /-> (vl fld)
   "Gets a value of the field [fld] of the object [vl]."
   (r_FI-GetValue (r_getField (r_GetType vl) fld) vl))

(function g-> (vl fld)
   "Gets a property value."
   (let* ((t (r_GetType vl))
          (b (r_bind t (string-append "get_" fld))))
     (b vl)))

(function sg-> (t fld)
   "Gets a static property value"
   (let ((b (r_sbind t (string-append "get_" fld))))
     (b)))

(define _gt_constr (r_tbind t_type "GetConstructor" "System.Type[]"))

(macro r_getconstructor (tp . args)
    `(let ((t (r_typer ,tp)))
        (_gt_constr t ,(if (null? args) `(anew t_type 0) `(vector ,@(map (fun (x) `(r_typer ,x)) args))))))


(function r_getconstructorf (tp args)
    (let* ((t (r_typerx tp))
           (res (_gt_constr t (if (null? args) (anew t_type 0) (mkvector (map (fun (x) (r_typerx x)) args))))))
      res))

(function instanceof (v t)
  (let ((tv (r_GetType v)))
   ((r_tbind "System.Type" "IsAssignableFrom" "System.Type") tv t)))


