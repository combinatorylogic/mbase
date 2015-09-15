;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(Section "Threads support")

(Par
 "This module provides a high level interface to \\NET{} threading."
 )

;; Basic .NET threading functionality bindings.

(using ("System.Threading")


(module threads
        (export thr:mkrunner
                thr:mkthread
                thr:start
                thr:abort
                thr:mkmanual
                thr:mkmutex
                thr:mutex_wait
                thr:mutex_release
                thr:manual_wait
                thr:manual_set
                thr:manual_reset
                thr:mkworker
                thr:mkpool
                thr:pool-add
                thr:pool-add-env
                thr:pool-send
                thr:pool-kill
                thr:mkqueue
                thr:queue-add
                thr:queue-kill
                thr:queue-start
                )

(function _caller (f) (f))
(define t_thread (sdotnet "Thread"))
(define t_thrdelegate (sdotnet "ThreadStart"))
(define t_thrdelegate_ctr (r_getconstructor t_thrdelegate t_object t_IntPtr))
(define t_manual (sdotnet "ManualResetEvent"))
(define t_mutex (sdotnet "Mutex"))

(late-ctime
 `(:classwrap ThreadRunner (Public)
    (field "clbk" ,t_object (Public))

    (xmethod ("setcallback" (Public) (Standard) ,t_void (,t_object))
       (Ldarg_0)
       (Ldarg_1)
       (Stfld (field "clbk"))
       (Ret))
    (xmethod ("mkdelegate" (Public) (Standard) ,t_thrdelegate ())
       (Ldarg_0)
       (Ldftn (method "run"))
       (Newobj ,t_thrdelegate_ctr)
       (Ret))

    (xmethod ("run" (Public) (Standard) ,t_void ())
       (Ldarg_0)
       (Ldfld (field "clbk"))
       (Call (method "crun"))
       (Ret)
       )

    (method (crun ,t_void ,t_object) (Public Static) _caller)
  ))

(force-class-flush)
(topblock
 (function thr:mkrunner (fn)
  (let ((x (new C_ThreadRunner)))
   ((r_tbind C_ThreadRunner "setcallback" t_object) x fn)
   ((r_tbind C_ThreadRunner "mkdelegate") x)))

 (function thr:mkthread (fn)
  "Makes a thread with a given controller function."
  (let ((runer (thr:mkrunner fn)))
    (new t_thread (t_thrdelegate runer))))

 (function thr:start (t)
  "Starts a thread."
  ((r_tbind t_thread "Start") t))

 (function thr:abort (t)
   "Aborts a thread's execution".
   ((r_tbind t_thread "Abort") t))

 (function thr:mkmanual ()
   "Makes a manual switch object."
   ((r_constr t_manual t_Boolean) #f))

 (function thr:mkmutex ()
   "Makes a mutex object."
   (new t_mutex))

 (function thr:mutex_wait (mtx)
   "Waits for a mutex."
   ((r_tbind t_mutex "WaitOne") mtx))

 (function thr:mutex_release (mtx)
   "Releses a mutex object."
   ((r_tbind t_mutex "ReleaseMutex") mtx))

 (function thr:manual_wait (m)
   "Waits for a manual switch."
   ((r_tbind t_manual "WaitOne") m))

 (function thr:manual_set (m)
   "Sets a manual switch."
   ((r_tbind t_manual "Set") m))

 (function thr:manual_reset (m)
   "Resets a manual switch."
   ((r_tbind t_manual "Reset") m))

 (function thr:mkworker (threnv bodyfun)
   (
    "Returns a pair of a thread worker controller function and"
    "a message sending function to trigger the execution of the controller."
    )
   (let* ((man (thr:mkmanual))
          (mtx (thr:mkmutex))
          (envelop (cons nil nil))
          (worker (fun ()
                    (n.label infty)
                    (thr:manual_wait man)
                    (thr:mutex_wait mtx)
                    (thr:manual_reset man)
                    (let ((val (cdr envelop)))
                       (set-cdr! envelop nil)
                       (if (eqv? val 'SUICIDE) (n.goto exit))
                       (bodyfun threnv val)
                       (thr:mutex_release mtx)
                       (n.goto infty)
                       (n.label exit)
                       (thr:mutex_release mtx)
                       )))

          (sender (fun (val)
                    (thr:mutex_wait mtx)
                    (set-cdr! envelop val)
                    (thr:manual_set man)
                    (thr:mutex_release mtx)
                    )))
     (cons worker sender)))

;; Thread pool implementation:

 (function thr:mkpool (endgame)
   ("Makes a thread pool."
    )
    (let* ((man (thr:mkmanual))
           (mtx (thr:mkmutex))
           (egmtx (thr:mkmutex))
           (thctr (cons nil 0))
           (stack (cons nil nil))
           (altadd (cons nil nil))
           (incthctr (fun ()
                       (thr:mutex_wait egmtx)
                       (set-cdr! thctr (+ 1 (cdr thctr)))
                       (thr:mutex_release egmtx)
                       ))
           (decthctr (fun ()
                       (thr:mutex_wait egmtx)
                       (set-cdr! thctr (- (cdr thctr) 1))
                       (if (< (cdr thctr) 1)
                           (if endgame (thr:manual_set endgame)))
                       (thr:mutex_release egmtx)
                       ))
           (add (fun (x)
                   (thr:mutex_wait mtx)
                   (if (null? (cdr altadd)) (begin
                     (cons-cdr! stack x)
                     (thr:manual_set man))
                    ((cdr altadd) x))
                   (thr:mutex_release mtx)))
           (pop (fun ()
                   (thr:mutex_wait mtx)
                   (let ((v (cadr stack)))
                      (set-cdr! stack (cddr stack))
                      (if (null? (cdr stack)) (thr:manual_reset man))
                      (thr:mutex_release mtx)
                      v)))
           (workerbody (fun (env v)
                         (format v (thr . msg)
                          (msg env) ;; message is a function
                          (add thr) ;; return the thread back to the stack
                          )))
           (newthrd (fun (env)
                      (format (thr:mkworker env workerbody) (t . s)
                        (thr:start (thr:mkthread t))
                        (incthctr)
                        (add s))))
           (kill (fun ()
                   (thr:mutex_wait mtx)
                   (set-cdr! altadd (fun (x)
                                       (x 'SUICIDE)
                                       (decthctr)
                                       ))
                   (foreach (x (cdr stack)) (x 'SUICIDE) (decthctr))
                   (thr:mutex_release mtx)))
           (send (fun (msg)
                   (thr:manual_wait man)
                   (let ((thr (pop)))
                      (thr (cons thr msg))))))
      (list newthrd send kill)))

 (function thr:pool-add (pool)
   "Adds one new thread to a given pool."
    ((car pool) nil))

 (function thr:pool-add-env (pool env)
   "Adds one new thread with a given environment to a given pool."
   ((car pool) env))

 (function thr:pool-send (pool msg)
   "Sends a message to a given pool. [msg] is a function with one argument."
    ((cadr pool) msg))

 (function thr:pool-kill (pool)
   "Terminates a given thread pool after all the outstanding messages are executed."
    ((caddr pool)))

;; Generic consumer queue (to feed the pool with messages)
;; N.B.: must own a separate controller thread
;; N.B.: when used with a thread pool the controller blocks if the pool is empty
 (function thr:mkqueue (nthr? consumer)
   ("Makes a consumer queue with a given consumer processor function."
    "If [nthr?] is [#t], makes a dedicated queue controller thread, otherwise uses the current one."
    )
    (let* ((mtx (thr:mkmutex))
           (man (thr:mkmanual))
           (qhead (cons nil nil))
           (qtail (cons nil nil))
           (kil   (cons nil nil)) ;; the last words to say
           (add (fun (x)
                   (thr:mutex_wait mtx)
                   (let ((nw (cons x nil)))
                     (if (null? (cdr qhead))
                       (begin
                         (set-cdr! qhead nw)
                         (set-cdr! qtail nw))
                       (begin
                         (set-cdr! (cdr qtail) nw)
                         (set-cdr! qtail nw)) ))
                   (thr:manual_set man)
                   (thr:mutex_release mtx)
                   ))
           (get (fun ()
                   (thr:mutex_wait mtx)
                   (if (null? (cdr qhead))
                       (begin
                         (thr:mutex_release mtx) nil)
                       (let ((v (cadr qhead)))
                          (if (null? (cddr qhead))
                             (begin
                                (set-cdr! qhead nil)
                                (set-cdr! qtail nil)
                                (thr:manual_reset man))
                             (begin
                                (set-cdr! qhead (cddr qhead)))
                          )
                          (thr:mutex_release mtx)
                          v))))
           (proc (fun ()
                   (n.label infty)
                   (thr:manual_wait man)
                   (let ((v (get)))
                     (if (eqv? v 'ABORT)
                        (begin
                          (if (null? (cdr kil)) nil ((cdr kil)))
                          (n.goto exit)) )
                     (consumer (car v)))
                   (n.goto infty)
                   (n.label exit) (n.null)
                   ))
           (thr  (if nthr? (thr:mkthread proc) nil)) ;; a dedicated controller thread
           (kill (fun (kilf)
                    (set-cdr! kil kilf)
                    (add 'ABORT) ;; kill after all current queries are executed.
                    )))
      (if nthr?
          (thr:start thr))
      (list (fun (x) (add (list x))) kill (if nthr? nil proc))))

  (function thr:queue-add (q v)
    "Adds a value to the consumer queue."
      ((car q) v))

  (function thr:queue-kill (q f)
    ("Kills a given consumer queue [q], evaluating [f] in the queue's"
     "controller context before termination.")
      ((cadr q) f))

  (function thr:queue-start (q)
    ("Starts the queue controller (either in the current thread or in a dedicated one).")
     (let ((proc (caddr q)))
      (if proc (proc) nil)))

)
));using



(macro with-mutex (mtx . body)
  (with-syms (res)
  `(begin
     (threads:thr:mutex_wait ,mtx)
     (let ((,res (begin ,@body)))
       (threads:thr:mutex_release ,mtx)
       ,res))))