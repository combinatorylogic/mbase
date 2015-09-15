;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Class wrapper

(Section "CLI class generation")

(define t_string_array (dotnet "System.String[]"))

(notaruntime
(function mtdnames (m)
  (let* ((s (strsplit (<r> "//") (any->string m))))
    (p:match s
      (($x $y) s)
      (($x) x)
      (else (any->string m)))))

(function wrap-a-method (nm0 wnm fags ret args)
  (let* ( (nm (core:lookup-global nm0))
          (flags (if (null? fags) '(Public) fags))
          (d (if (not (memq 'Static fags)) 1 0))
          (decl (net.env.get nm)))
     (if (or (null? decl)
             (not (eq? 'minfo (car decl))))
       (ccerror `(WRAPPING an incorrect method ,nm)))
     `(method (,(mtdnames wnm) ,flags (Standard) ,ret ,args)
         ,@(if (> d 0) `((Ldarg_0) (Castclass ,t_object)) nil)
         ,@(pre-emit
             (mapi
               (fun (i a)
                `(,(_ldarg (+ d i))
                  ,@(if (isBoxed a)
                     `((Box ,a))
                     `((Castclass ,t_object)))))
               args))
         (Call ,(cadr decl))
         ,@(if (t_eq ret t_void) '((Pop))
               (if (isBoxed ret)
                  (_unbox_type ret)
                 `((Castclass ,ret) )))
         (Ret)
         )
       ))
       )

(define _aasm (r_tbind "System.Reflection.Emit.ModuleBuilder" "get_Assembly"))


;; use it with ctime macro to substitute the type values
(recfunction f.:classwrap.inner (dest nm fags body)
 (let* ((extends (select-car 'extends body))
        (implements (select-car 'implements body))
        (methods (select-car 'method body))
        (tmodule (select-car 'target-module body))
        (constrs (select-car 'xconstr body))
        (mainp (select-car 'main body))
        (fields (select-car 'field body))
        (ifields (select-car 'initfield body))
        (xmethods (select-car 'xmethod body))
        (pinvokes (select-car 'pinvoke body))
        (inners (foreach-map (c (select-car 'class body))
                  (format c (_ nm1 fags1 . body1)
                     (car (f.:classwrap.inner nil nm1 fags1 body1)))))

        (cls
         `(class (,(any->string nm) ,@fags) ,@extends ,@implements
            ,@inners
            ,@ifields
            ,@fields
            ,@(foreach-map (p pinvokes)
                `(pinvoke-method ,@(cdr p)))
            ,@(foreach-map (m xmethods)
                (format (cdr m) ((nm . r1) . r2)
                    `(method (,(mtdnames nm) ,@r1) ,@r2)))
            ,@(foreach-map (m methods)
                (format (cdr m) ((wnm ret . args) fags nm)
                 (wrap-a-method nm wnm fags ret args)))
            ,@(foreach-map (c constrs)
                (format (cdr c) (nickname acc xx args . body)
                   `(constructor (,nickname ,acc ,xx ,args) ,@body
                                 )))
          )))
   (list cls mainp tmodule)
   ))

(function f.:classwrap (dest nm fags body)
  (format (f.:classwrap.inner dest nm fags body)
          (cls mainp tmodule)
  (let* ((modl
          (if dest dest
              (if tmodule (format tmodule ((_ vnm))
                                  (read-int-eval `(car (list ,vnm))))
                  (net.current-module))))

         (ccls (clr:emit.class modl cls)))
    (if mainp
        (_set_exe_entry_point
         (_aasm modl)
         (r_mtdf ccls (cadr (car mainp)) (list t_string_array))
         (if (null? (cddr (car mainp)))
             "ConsoleApplication"
             (caddr (car mainp)))
         ))
    (cons modl ccls))))

(macro :classwrap (nm fags . body)
 ("Creates a class with a given name [nm] and attributes [fags].[br]"
  "[body] format is:"
  "[["
  "(extends <classname>)  - parent class, default is System.Object"
  "(main <methodname> <exetype>) - sets exe file entry point."
  "(implements <interface>*) - interfaces implemented"
  "(xmethod ...) - CLI method, see Emit definition for reference."
  "(method (<name> <rettype> <argtype>*)"
  "             <attributes> <lisp-function>) - binds a lisp function"
  "(constr ...) - constructor"
  "(field <name> <type> <attrs>) - class field"
  "]]"
  )
 (let* ((mccls (f.:classwrap nil nm fags body))
        (ccls (cdr mccls))
        (modl (car mccls))
        (as ((r_tbind "System.Reflection.Emit.ModuleBuilder" "get_Assembly")
             modl))
        (ax (hashget *asms* ((r_tbind "System.Reflection.AssemblyName" "get_Name")
                             ((r_tbind t_assembly "GetName")
                              as)))))
   (if (null? ax)
       (add-assembly-inner as))

   `(ctimex
     (define ,(string->symbol (buildstring "C_" nm)) (quote ,ccls)))))



