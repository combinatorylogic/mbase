;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define t_assembly (r_typebyname "System.Reflection.Assembly"))
(define t_appdomain (r_typebyname "System.AppDomain"))

(define list-assembly
  (let* ((LoadFrom (r_tsbind t_assembly "LoadFrom" string))
         (GetName0 (r_tbind t_assembly "GetName"))
         (a_name (r_tbind "System.Reflection.AssemblyName" "get_Name"))
         (GetName (lambda (as) (a_name (GetName0 as))))
         (GetTypes (r_tbind t_assembly "GetTypes")))
    (lambda (nm)
      (let* ((as (LoadFrom nm))
             (typs (a->l (GetTypes as)))
             (names (map (lambda (n) (r_typename n)) typs)))
         (list (GetName as) names)))))

(function ->s (o)
  "Calls the [ToString] method of [o]."
  ((r_tbind t_object "ToString") o))

(set! assmpool nil)

(function AssemblyName (as)
  ((r_tbind "System.Reflection.AssemblyName" "get_Name")
   ((r_tbind t_assembly "GetName") as)))

(define lookup-assembly-inner
   (let* ((LoadFrom (r_tsbind t_assembly "LoadFrom" string))
          (Load     (r_tsbind t_assembly "LoadWithPartialName" string)))
     (lambda (nm)
       (let ((as (if (string? nm) (LoadFrom nm)
                      (if (symbol? nm)
                          (Load (any->string nm))
                          nm))))
         (if (or (null? nm) (null? as) (null? *asms*))
             (begin
               (writeline (list 'ASSM:ERROR: nm as
                                (hashmap (fun (a b)
                                           (list a b))
                                         *asms*)))
               nil)
             as)))))

(define add-assembly-inner
  (lambda (as)
    (let* ((an (AssemblyName as))
           (chk (hashget *asms* an)))
;      (if (not chk)
;          (begin
            (hashput *asms* (AssemblyName as) as)
            (set! assmpool (cons as (! assmpool)))
;            ))
      as
      )))

;; AppDomain.CurrentDomain.GetAssemblies()
(define try-context-assembly
  (let* (
         (CurrentDomain (r_tsbind t_appdomain "get_CurrentDomain"))
         (GetAssemblies (r_tbind t_appdomain "GetAssemblies"))
         )
    (lambda (nm)
      (let* ((ass (GetAssemblies (CurrentDomain)))
             (asl (if ass (a->l ass))))
        (let loop ((a asl))
          (if (null? a) (begin
                          (println (list "COULD NOT LOAD" nm " FROM " asl))
                          nil
                          )
              (let* ((as (car a))
                     (an ((r_tbind t_assembly "get_FullName") as)))
                (if (eq? an nm)
                    (add-assembly-inner as)
                    (loop (cdr a))))))))))

(function try-context-assembly-wrapper (as asn)
  (if as
      (add-assembly-inner as)
      (try-context-assembly asn)))

(macro add-assembly (nm)
  "[(add-assembly <filename>)] adds an assembly to the local lookup cache."
  `(add-assembly-inner (lookup-assembly-inner ,nm)))

(macro load-assembly (nm)
  "[(load-assembly <name>)] function adds an assembly to the local lookup cache."
   (let* ((LoadFrom (r_tsbind t_assembly "LoadFrom" string))
          (Load     (r_tsbind t_assembly "LoadWithPartialName" string))
          (getName  (r_tbind t_assembly "get_FullName"))
          )
     (let ((as (if (string? nm) (LoadFrom nm)
                   (if (symbol? nm)
                       (Load (any->string nm)) nm))))
       (if (or (null? nm) (null? as) (null? *asms*))
           (writeline (list 'ASSM:ERROR: nm as
                            (hashmap (fun (a b)
                                       (list a b))
                                     *asms*)))
           (let* ((fnm (getName as))
                  (anm (AssemblyName as)))
             (writeline `(ASS: ,as ,fnm))
             (hashput *asms* anm as)
             (set! assmpool (cons as (! assmpool)))
             `(add-assembly-fun ,anm ,fnm))))))

(define __assembly_Load (r_tsbind t_assembly "Load" string))

(function add-assembly-fun (anm fnm)
  (if (not (hashget *asms* anm))
   (let* ((as (__assembly_Load fnm)))
     (hashput *asms* (AssemblyName as) as)
     (set! assmpool (cons as (! assmpool))))))

(force-class-flush)
(expand-if (shashget (getfuncenv) 'asmlib-final)
 (add-assembly-inner ((r_tsbind t_assembly "GetExecutingAssembly"))))

;(ctimex (when (shashget (getfuncenv) 'asmlib-final)
;            (add-assembly stage5-assembly)))

(define AS-GetType (r_tbind t_assembly "GetType" string))

(function r_lookupall (pfx nm)
  (let* ((asms (hashmap (fun (a b) b) *asms*))
         (nm1 (buildstring pfx "." nm)))
    (let loop ((a asms))
      (if (null? a) nil
          (let* ((tr1 (AS-GetType (car a) nm1))
                 (tr2 (AS-GetType (car a) nm)))
            (if tr1 tr1
                (if tr2 tr2
                    (loop (cdr a)))))))))


(function r_lookup (asm nm)
  "[(r_lookup <assemblyname> <typename>)] function searches for a type in a given assembly"
  (let ((as (if (or (string? asm) (symbol? asm)) (hashget *asms* asm) asm))
        (asmn (if (or (string? asm) (symbol? asm)) asm
                  (AssemblyName asm))))
    (if (null? as)
        (let ((finl
               (r_typebyname (buildstring asmn "." nm))))
          (if finl finl
                          (r_lookupall asm nm))
          )
        (let ((tr (AS-GetType as (buildstring asmn "." nm))))
          (if tr tr
              (let ((tr1 (AS-GetType as nm)))
                (if tr1 tr1
                    (let ((finl
                           (r_typebyname (buildstring asmn "." nm))))
                      (if finl finl
                          (r_lookupall asm nm))

                      ))))))))

(set! using '( "System" "System.Collections" "Meta.Scripting" )   )

(function r_set_using0 (us)
  (set! using us))

(macro r_push_using (lst0)
  (let ((lst (cadr lst0)))
  `(top-begin
    (ctimex
     (begin
       (macro r_prev_using ()
         (list 'quote (cons (r_current_using) (r_prev_using))))
       (macro r_current_using ()
         (list 'quote (append (quote ,lst) (r_current_using))))))
    (r_set_using0 (r_current_using)))))

(macro r_pop_using _
  `(top-begin
    (ctimex
     (begin
       (macro r_current_using ()
         (list 'quote
               (car (r_prev_using))))
       (macro r_prev_using ()
         (list 'quote
               (cdr (r_prev_using))))))
    (r_set_using0 (r_current_using))))

(macro r_current_using _
  '(quote ("System" "Meta.Scripting")))

(macro r_prev_using _
  '(quote ("System" "Meta.Scripting")))

(recfunction r_try_list2 (l t)
  (if (null? l) nil
      (let ((tt (r_lookup (car l) t)))
        (if (null? tt) (r_try_list2 (cdr l) t) tt))))

(recfunction r_try_list (l t)
   (if (null? l) nil
       (let ((tt (r_lookup (car l) t)))
         (let ((tt2
                (if (null? tt) (r_try_list (cdr l) t) tt)))
           (let ((res
                  (if (null? tt2)
                      (r_try_list2 (! assmpool) t)
                      tt2)))
             (if (null? res)
                 (begin
                   ;; (println (list "TYPE ERROR: " t l (! assmpool)))
                   nil)
                 res))))))

(function dotnet (t)
  ("Returns a [Type] instance for a given type name,"
   "using the local assembly cache for lookup."
   )
   (let ((r (r_typebyname t)))
     (if (null? r)
       (r_try_list (! using) t)
       r
       )))

;(macro dotnet (t)
;  (let ((stk (shashget (getfuncenv) '**cc-expand-stack**)))
;    (if stk
;        (if (cdr stk)
;            (if (cdr (cdr stk))
;                (println (list 'DOTNET-MACRO t (car (cdr (cdr stk)))))))))
;  `(dotnet_inner ,t))

(function udotnet (path t)
  ("Returns a [Type] instance for a given type name,"
   "using the local assembly cache for lookup."
   )
   (let ((r (r_typebyname t)))
     (if (null? r)
       (r_try_list path t)
       r
       )))

(function r_TypeFullName (tp)
  ((r_tbind "System.Type" "get_FullName") tp)
  )

(macro sdotnet (t)
  `(dotnet ,(let* ((tp (udotnet (read-int-eval '(car (list (r_current_using)))) t))
                   (tpn (r_TypeFullName tp)))
              tpn)))

(macro usdotnet (pth t)
  `(dotnet ,(let* ((tp (udotnet pth t))
                   (tpn (r_TypeFullName tp)))
              tpn)))

(macro using (lst . rest)
  ("Adds a list of named assemblies to the current local assembly cache,"
   "for an inner context only.")
   `(top-begin
      (r_push_using (quote ,lst))
      ,@rest
      (r_pop_using)
      ))

