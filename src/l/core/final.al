;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(function *the-init* () ;; delayed default module initialization
   (read-int-eval '(n.module Default))
   nil)

(define *toplevel-module-initp?* (mkref))

(function read-compile-eval (lst)
  ("Redefinition of [(read-compile-eval ...)], now it is a compiler's frontend."
   "It should not normally be used from the user's code, but serves as a default callback for"
   "wrappers."
   )
  (when (not (car *toplevel-module-initp?*))
        (read-int-eval '(n.module mbase_default_toplevel))
        )
  (let* ((env (shashget (getfuncenv) '*current-cc-env*)))
    (cc:toplevel-devour env lst)))

(function read-compile-eval-t (lst)
  (when (not (car *toplevel-module-initp?*))
        (read-int-eval '(n.module mbase_default_toplevel))
        )
  (let* ((env (shashget (getfuncenv) '*current-cc-env*)))
    (cc:toplevel-devour-transparent env lst)))

(macro n.module (name . r)
  ("Defines a module with a given {\tt name} and type. Default type is dll,"
   "other types available are: exe, winexe.")
    (let* ((env (alet tst (shashget (getfuncenv) '*current-cc-env*)
                      (if tst tst (cc:newenv))))
           (sver (shashget (getfuncenv) 'assembly-version))
           (kfile (shashget (getfuncenv) 'assembly-keyfile)))
      (set-car! *toplevel-module-initp?* #t)
      (cc:env:defmodule:strong env (S<< name) (if r (car r) 'dll) sver kfile)
      (shashput (getfuncenv) '*current-cc-env* env)
      (add-assembly-inner (env:get: env dotnet-assembly))
      (flush-target-dependencies)
      `(top-begin )))

(function n.save.f ()
  (cc:dump-module (shashget (getfuncenv) '*current-cc-env*)))

(macro n.save ()
  ("Save the current module to an exe or dll file.")
  `(late-ctime (begin (n.save.f) '(top-begin))))

(macro n.report ()
  ("Print a compiler statistics for the current module to the standard output.")
  `(late-ctime
    (begin
      (cc:env:printreport
       (cc:env:report
        (shashget (getfuncenv) '*current-cc-env*)))
      '(top-begin)
      )))

(function net.env.get (nm)
  (cc:extract-method (shashget (getfuncenv) '*current-cc-env*) nm))

(function net.current-module ()
  (alet env (shashget (getfuncenv) '*current-cc-env*)
        (cc:env:getmodule env)))
