;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(function cc:sanitise-type (t)
  `(othertype ,(S<< t)))

(function cc:sanitise-method (m)
  `(othermethod ,(S<< ((r_tbind "System.Reflection.MemberInfo" "get_DeclaringType") m))
           ,((r_bind t_MethodInfo "get_Name") m)
           ,(foreach-map (a (a->l ((r_bind t_MethodInfo "GetParameters") m)))
              ((r_tbind "System.Reflection.ParameterInfo" "get_ParameterType") a))
           ))

(function cc:sanitise-constructor (m)
  `(otherctor ,(S<< ((r_tbind "System.Reflection.MemberInfo" "get_DeclaringType") m))
           ,(foreach-map (a (a->l ((r_bind t_ConstructorInfo "GetParameters") m)))
              ((r_tbind "System.Reflection.ParameterInfo" "get_ParameterType") a))
           ))

(function cc:sanitise-field (f)
  `(otherfield ,(S<< ((r_tbind "System.Reflection.MemberInfo" "get_DeclaringType") f))
               ,((r_tbind "System.Reflection.FieldInfo" "get_Name") f)))

(function cc:core-sanitise-tokens-inner (body)
  (let loop ((bd body))
    (p:match bd
      (($a . $b)
       (cons (loop a)
             (loop b)))
      ($$M bd)
      ($$S bd)
      ($$N bd)
      (() nil)
      (else
       (type-case-hier bd
         (t_Type
          (cc:sanitise-type bd))
         (t_MethodInfo
          (cc:sanitise-method bd))
         (t_ConstructorInfo
          (cc:sanitise-constructor bd))
         (t_FieldInfo
          (cc:sanitise-field bd))
         (else (ccerror `(CC:SANITISE-UNKNOWN-TYPE ,(S<< (r_GetType bd)))))))
      )))
