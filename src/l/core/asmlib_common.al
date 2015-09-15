;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define MAXARGS 20)
(define SMAXARGS 11)
(define SSMAXARGS (- SMAXARGS 1))

(net.types SimpleCode Code Closure Symbol Pair IntPtr Runtime MiscCall AltClosure Delegate)
(net.types Exception MulticastDelegate)

(define m_SimpleCode_Invoke (r_mtd t_SimpleCode "Invoke" t_object_array))
(define m_Code_Run (r_mtd t_Code "run" t_object_array t_object_array))
(define f_Closure_frame (r_getField t_Closure "frame"))
(define f_Closure_c (r_getField t_Closure "c"))
(define scons (r_typer "System.Console"))
(define m_writeline (r_mtd scons "WriteLine" t_string))

(function __mkname (n i)
  (string-append n (any->string i)))

(function __mksname (n i)
  (string->symbol (__mkname n i)))

(ctime `(top-begin
          ,@(formap (i 0 MAXARGS)
                    `(define ,(__mksname "AltFun" i)
                       (dotnet ,(__mkname "AltFun" i))))
          ,@(formap (i 0 MAXARGS)
                    `(define ,(__mksname "AltClosure" i)
                       (dotnet ,(__mkname "AltClosure" i))))
          ,@(formap (i 0 MAXARGS)
                    `(define ,(__mksname "AltClFun" i)
                       (r_mtd ,(__mksname "AltClosure" i)
                               "run" ,@(formap (j 0 i) 'object))))

          (define AltFuns (list ,@(formap (i 0 MAXARGS)
                                          (__mksname "AltFun" i))))
          (define AltClosures (list ,@(formap (i 0 MAXARGS)
                                          (__mksname "AltClosure" i))))
          (define AltClFuns (list ,@(formap (i 0 MAXARGS)
                                          (__mksname "AltClFun" i))))
          ))

(define t_nint32 (r_typebyname "System.Int32"))
;; 1st try loading the existing dll



