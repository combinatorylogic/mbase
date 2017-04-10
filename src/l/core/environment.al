;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A new, cleaner way of handling the compilation environment.

(using ("System.Threading")

 (define t_mutex (dotnet "System.Threading.Mutex"))
 (force-class-flush)

 (function thr:mkmutex ()
   (new t_mutex))

 (function thr:mutex_wait (mtx)
   ((r_tbind t_mutex "WaitOne") mtx))

 (function thr:mutex_release (mtx)
   ((r_tbind t_mutex "ReleaseMutex") mtx))

 )

 (macro block-on (wh . body)
   (with-syms (res ex)
     `(begin
        (thr:mutex_wait ,wh)
        (let ((,res (try
                     (begin ,@body)
                     t_Exception
                     (fun (,ex)
                       (thr:mutex_release ,wh)
                       (r_raise ,ex)))))
          (thr:mutex_release ,wh)
          ,res))))




