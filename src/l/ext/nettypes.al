;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; .NET types handling

(Section "\NET{} types handling library")

(Par "This library is designed mainly for Not.Net target sublanguage.")

(define _underlying_type
  (r_tbind "System.Type" "get_UnderlyingSystemType"))

(function il-type-class-int ( ttp )
  ("Classifies the given .NET type (an instance of System.Type) by its storage."
   "Possible values are: I, I1, I2, I4, I8, R4, R8, Ref.")
  (cond
   ((not (isBoxed ttp)) 'Ref)
   (else
    (let ((tp (if (IsEnum ttp) t_int ttp)))
      (let ((x (filter (fun (x) (t_eq tp (car x))) _ldind_ar)))
        (if (null? x) 'Obj
            (cdar x)))))))

(define _get_interfaces
  (r_tbind "System.Type" "GetInterfaces"))

(define _get_basetype
  (r_tbind "System.Type" "get_BaseType"))

(define _getmembers
  (r_tbind "System.Type" "GetMembers"))

(define _getconstructors
  (r_tbind "System.Type" "GetConstructors"))

(define _getfields
  (r_tbind "System.Type" "GetFields"))

(define _isbyref?
  (r_tbind "System.Type" "get_IsByRef"))

(define _getmtdname
  (r_tbind "System.Reflection.MethodBase" "get_Name"))

(define _getfldname
  (r_tbind "System.Reflection.FieldInfo" "get_Name"))

(define _getrettype
  (r_tbind "System.Reflection.MethodInfo" "get_ReturnType"))


(define _getfldtype
  (r_tbind "System.Reflection.FieldInfo" "get_FieldType"))

(define _isfldstatic
  (r_tbind "System.Reflection.FieldInfo" "get_IsStatic"))

(define _getbasedef
  (r_tbind "System.Reflection.MethodInfo" "GetBaseDefinition"))

(define _getfldbasedef
  (r_tbind "System.Reflection.FieldInfo" "get_DeclaringType"))

(define _getdecltyp
  (r_tbind "System.Reflection.MethodInfo" "get_DeclaringType"))

(function _getargs (mi)
  (let ((aa ((r_tbind "System.Reflection.MethodBase" "GetParameters") mi)))
    (net.map (fun (pi)
             ((r_tbind "System.Reflection.ParameterInfo" "get_ParameterType") pi))
           aa)))

(function il-types-assignable ( ttp )
  ("Returns a list of types assignable from a given one: all the interfaces it"
   "implements and all its direct ancestors.")
  (let* ((ints (a->l (_get_interfaces ttp)))
         (bass (let loop ((t ttp))
                 (if (null? t) nil
                     (cons t (loop (_get_basetype t)))))))
    (append ints bass)))

(define t_MethodInfo (dotnet "System.Reflection.MethodInfo"))
(define t_ConstructorInfo (dotnet "System.Reflection.ConstructorInfo"))
(define nm_ctor  (Sm<< ".ctor"))
(define nm_cctor  (Sm<< ".cctor"))

(function il-types-havemethod ( ltps mtdname rettype signature )
  ("Returns a list of types which have a method of a given name and fits a given signature."
   "If a return type or some of argument types are unknown, they can be null.")
  (let* ((nargs (length signature))
         (teqq? (fun (t1 t2)
                  (if (null? t2) #t
                      (t_ass? t1 t2))))
         (chksign (fun (rt atps)
                    (cond
                     ((not (= (length atps) nargs)) nil)
                     ((not (teqq? rt rettype)) nil)
                     ((alltrue (fmt (a . b) (teqq? a b)) (czip atps signature)) #t)
                     (else nil)))))
    (filter (fun (x) (not (null? x)))
      (map-over ltps
         (fun (tp)
           (let* ((mmb (a->l (_getmembers tp)))
                  (mtds (filter
                         (fun (x)
                           (and
                            (t_ass? t_MethodInfo (r_GetType x))
                            (eqv? (string->symbol (_getmtdname x))
                                  mtdname)))
                         mmb))
                  (mr (filter (fun (mx)
                                (chksign (_getrettype mx) (_getargs mx)))
                              mtds)))
             (if (null? mr) nil
                 (cons tp mr))))))))

(function il-type-constructors ( tp signature )
  ("Returns a list of types which have a constructor that fits a given signature."
   "If some of argument types are unknown, they can be null.")
  (let* ((nargs  (length signature))
         (teqq? (fun (t1 t2)
                  (if (null? t2) #t
                      (t_eq t1 t2))))
         (chksign (fun (atps)
                    (cond
                     ((not (= (length atps) nargs)) nil)
                     ((alltrue (fmt (a . b) (teqq? a b))
                               (czip atps signature)) #t)
                     (else nil)))))
    (let* ((mmb (a->l (_getconstructors tp)))
           (mr (filter (fun (mx)
                         (chksign (_getargs mx)))
                       mmb)))
      mr)))


(function il-types-havefield ( ltps fldname fldtype )
  ("Returns a list of types which have a field of a given name and type."
   )
  (let* ((teqq? (fun (t1 t2)
                  (if (null? t2) #t
                      (t_eq t1 t2)))))
    (filter (fun (x) (not (null? x)))
      (map-over ltps
         (fun (tp)
           (let* ((mmb (a->l (_getfields tp)))
                  (mtds (filter (fun (x)
                                  (eq? (_getfldname x) fldname))
                                mmb))
                  (mr (if (null? fldtype) mtds
                          (filter (fun (x) (t_eq fldtype (_getfldtype x)))
                                  mtds))))
             (if (null? mr) nil
                 (cons tp mr))))))))

(function il-types-havemethod-refined ( ltps mtdname rettype signature )
  ("Returns a refined result of [il-types-havemethod], leaving method declaring types only.")
  (let ((ml (il-types-havemethod ltps mtdname rettype signature))
        (tps (mkhash)))
    (foreach (t ml) (hashput tps (->s (car t)) #t))
    (filter
     (fun (x) (not (null? (cdr x))))
     (map-over ml
       (fmt (tp . mtds)
            (cons tp
                  (filter
                   (fun (m)
                     (let* ((b (_getdecltyp (_getbasedef m))))
                       (not (and (not (t_eq b tp)) (hashget tps (->s b))))))
                   mtds
                   )))))))

(function il-types-havefield-refined ( ltps fldname fldtype )
  ("Returns a refined result of [il-types-havefield], leaving method declaring types only.")
  (let ((ml (il-types-havefield ltps fldname fldtype ))
        (tps (mkhash)))
    (foreach (t ml) (hashput tps (->s (car t)) #t))
    (filter
     (fun (x) (not (null? (cdr x))))
     (map-over ml
       (fmt (tp . flds)
            (cons tp
                  (filter
                   (fun (m)
                     (let* ((b (_getfldbasedef m)))
                       (not (and (not (t_eq b tp)) (hashget tps (->s b))))))
                   flds
                   )))))))

(function il-types-havemethods-agressive ( ltps mtds flds )
  (let loop ((l ltps))
    (let* ((tps0
             (foldl append nil
             (append
              (map-over mtds
               (fmt (mtdname rettype signature)
                 (map-car
                   (il-types-havemethod-refined ltps mtdname rettype signature))))
              (map-over flds
               (fmt (fldname fldtype)
                 (map-car
                   (il-types-havefield-refined ltps fldname fldtype)))))))
           (tps (genunifiq t_eq tps0)))
      (if (= (length tps) (length l)) l
          (loop tps)))))


