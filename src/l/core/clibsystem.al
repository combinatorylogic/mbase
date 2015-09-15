;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-assembly 'System)

(force-class-flush)

(function system (a b onout)
    (let* ((nsti  (r_constr (sdotnet "System.Diagnostics.ProcessStartInfo")
                            t_string t_string))
           (info (nsti a b))
           )
      ((r_tbind (sdotnet "System.Diagnostics.ProcessStartInfo")
                "set_UseShellExecute" t_Boolean) info #f)
      ((r_tbind (sdotnet "System.Diagnostics.ProcessStartInfo")
                "set_CreateNoWindow" t_Boolean) info #t)
      ((r_tbind (sdotnet "System.Diagnostics.ProcessStartInfo")
                "set_RedirectStandardOutput" t_Boolean) info #t)
      (let* ((p
              ((r_tsbind (sdotnet "System.Diagnostics.Process")
                         "Start"
                         (sdotnet "System.Diagnostics.ProcessStartInfo"))
               info))
             )
        (process-stream (g-> p "StandardOutput") onout)
        (if (not (g-> p "HasExited"))
            ((r_tbind (sdotnet "System.Diagnostics.Process")
                      "WaitForExit" int) p 100))
        nil
        )))

(function system-lazy (a b)
  (let* ((nsti  (r_constr (sdotnet "System.Diagnostics.ProcessStartInfo")
                           t_string t_string))
          (info (nsti a b))
          )
    ((r_tbind (sdotnet "System.Diagnostics.ProcessStartInfo")
               "set_UseShellExecute" t_Boolean) info #f)
     ((r_tbind  (sdotnet "System.Diagnostics.ProcessStartInfo")
               "set_CreateNoWindow" t_Boolean) info #t)
     ((r_tbind  (sdotnet "System.Diagnostics.ProcessStartInfo")
               "set_RedirectStandardOutput" t_Boolean) info #t)
     (let* ((p
             ((r_tsbind  (sdotnet "System.Diagnostics.Process")
                        "Start"  (sdotnet "System.Diagnostics.ProcessStartInfo"))
              info))
            (stdo (g-> p "StandardOutput"))

            (r (read-stream-big
                stdo
                (fun ()
                     (if (not (g-> p "HasExited"))
                         ((r_tbind  (sdotnet "System.Diagnostics.Process")
                                   "WaitForExit" int) p 100))))))
       ((r_tbind  (sdotnet "System.Diagnostics.Process") "Close") p)
       r
       )))


(define *BUILD*
  "Long version string."
  (ctime
   (S<< "MBase v." ##version ", " (date-now) " ["
        (->s (sg-> (dotnet "System.Environment") "Version"))    "] "
        ##copyright
        )))

(define *BUILD-VERSION*
  "Current build version."
  ##version)

(define *BUILD-OS*
  "Build OS string."
  (ctime (->s (sg-> (dotnet "System.Environment") "OSVersion"))))

(define t_Assembly (dotnet "System.Reflection.Assembly"))

(force-class-flush)

(function rundll (str)
  ((r_tsbind t_Runtime "rundll" string) str))

(function runassembly (str)
  ((r_tsbind t_Runtime "runassembly" t_Assembly) str))

(function __genclv000 ()
; N.B., this function must be compiled into MBaseBin.dll
; in order to return the proper assembly. It should be used just once,
; when defining the *CORE-LIBRARY-VERSION* string constant below.
  (let* ((name1 ((r_tbind t_Assembly "get_FullName")
                  ((r_tsbind t_Assembly "GetExecutingAssembly"))))
          (npos ((r_tbind t_string "IndexOf" t_string) name1 ", ")))
     ((r_tbind t_string "Substring" int) name1 (+ 2 npos))))

(force-class-flush)

; TODO: think how to make in static again:
(define *CORE-LIBRARY-VERSION*
   (__genclv000))

(force-class-flush)

(function __findsystemassembly (nm)
  ((r_tsbind t_Assembly "Load" string)
   (S<< nm ", " *CORE-LIBRARY-VERSION*)))

(macro usedll (nm)
  "Loads a DLL produced by MBase. Must refer to the same version of runtime."
  `(ctimex (rundll ,(S<< (corelib:get-lookup-path) "/" nm ".dll"))))

(macro sysdll (nm)
  ("Loads a system DLL produced by MBase. "
   "Must be of the same version (and signed the same way) as "
   "the MBase core library.")
  `(ctimex (runassembly
            (__findsystemassembly ,(S<< nm)))))

