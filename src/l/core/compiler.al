;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \section{$L_1'$ compiler}
;-

(Section "Compiler")

;-
;-  First, some utility functions.
;-
;= Report a compiler specific error, to be processed in a main loop.
(function cc:comperror (e)
  (ccerror `(COMP ,e)))

(define t_ass? 
  (r_tbind "System.Type" "IsAssignableFrom" "System.Type"))
  
;-
;-
;-

(include "./cc-netdefs.al");-I
(include "./cc-ast.al");-I
(include "./cc-core.al");-I
(force-class-flush)
(include "./cc-tail.al");-I
(force-class-flush)
(include "./cc-transforms.al");-I
(include "./cc-optimise.al");-I
(force-class-flush)
(include "./cc-ast-flat.al");-I
(include "./cc-flat.al");-I
(force-class-flush)
(include "./cc-schedule.al");-I
(force-class-flush)
(expand-if (shashget (getfuncenv) 'compiler-final)
(include "./cc-funcache.al");-I
)

(include "./cc-plugins.al");-I

(function cc:compile-stage1 ( env expr )
  ("Performs the first stage of compilation, taking a Core AST as a source"
   "and producing a Flat output."
   "This stage does not depend on any environment and guaranteed to be stable"
   "against given source."
   )
  (<> expr
   (pipeline>
     cc:scope-transform   ; make unique names
     (fun (e)
       (ctime 
        (if (shashget (getfuncenv) 'compiler-final)
            '(cc:optimise e)
            'e)))
     ;;TODO
     ;(fun (e)
     ;  (cc:resolve-globals env e))
     (fun (e)
       (when (shashget (getfuncenv) 'debug-compiler-prelift)
	     
	     (println '-PRE-LIFT-)
	     (iter writeline e)
	     (println '-------------------PRE-LIFT))
       e
       )

     cc:pre-lift-plugins  ; Call pre-lifting user code

     cc:lift-lambdas      ; lift nested lambda expressions
     cc:clean-dummy       ; remove trash to maintain correct return values
     cc:fix-closures      ; add references to closure captured variables
                          ; and perform tail calls optimisation

     cc:after-lift-plugins ; Call post-lifting user code

     (fun (e)
       (when (shashget (getfuncenv) 'debug-compiler-postlift)
	     (println '-POST-LIFT-)
	     (iter writeline e)
	     (println '-------------------POST-LIFT))
       e
       )

     (fun (e)
       (ctime
        (if (shashget (getfuncenv) 'compiler-final)
            '(cc:cachetop env e)
            'e)))
     cc:compile-lifted    ; compile into flat pseudocode

     cc:flat-plugins      ; Call flat stage user code

     (fun (e)
       (when (shashget (getfuncenv) 'debug-compiler-flat0)
	     (println '-PRE-SCHEDULE-)
	     (iter writeline e)
	     (println '-------------------PRE-SCHEDULE))
       e
       )
     (fun (e) 
      (if (and (shashget (getfuncenv) 'core-environment-compiled)
	       (not (shashget (getfuncenv) 'compiler-scheduler-off)))
          (cc:lifted-reschedule e) ; get rid of redundant local variables
          e))
     (fun (e)
       (when (shashget (getfuncenv) 'debug-compiler-flat)
	     (println '-FLAT-)
	     (iter writeline e)
	     (println '-------------------FLAT))
       e
       )
     )))

;-
;- Compiler backend components
;-

(force-class-flush)
(include "./cc-environment.al");-I
(force-class-flush)
(include "./cc-dotnet.al");-I
(force-class-flush)
(include "./cc-driver.al");-I
(force-class-flush)
(include "./binder.al");-I
