;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(using ("System.Net.Sockets" "System.IO" "System")
(module tcprepl
        (using threads)
        (export spawn-mbase-repl tcprepl)


 (not.function tcplistener ((int port))
     (return (new System.Net.Sockets.TcpListener port)))

 (not.function tcpwaiter ((System.Net.Sockets.TcpListener l) (object proc))
     (l@Start) ;; start listening
     (while true ;; loop endlessly
        (client = (l@AcceptTcpClient))
        (mbase proc client)
        )
     (l@Stop)
     (return null) ; never comes here
     )

 (not.function tcpgetstream ((System.Net.Sockets.TcpClient c))
     (return (c@GetStream)))

(define treadline (r_tbind "System.IO.TextReader" "ReadLine"))

(function tprintln ( a b )
  ((r_tbind "System.IO.TextWriter" "WriteLine" string) a b)
  ((r_tbind "System.IO.TextWriter" "Flush") a))

(function tprint ( a b )
  ((r_tbind "System.IO.TextWriter" "Write" string) a b)
  ((r_tbind "System.IO.TextWriter" "Flush") a))

(recfunction tcprepl (fi fo)
  (try
   (let loop ()
     (print ">>")
     (let* ((rdr (fun () (let ((res (treadline fi)))
                           (if (string=? res "#quit")
                               nil
                               res))))
            (a0x (mbase-parse-repl rdr (fun () (tprint fo "%> ")))))
       (if a0x
           (let ((v (read-compile-eval a0x)))
             (tprintln fo (S<< "<< "
                               (to-string v)))
             (loop)
             ))))
   t_MBaseException
   (fun (ex)
     (writeline (mbaseerror ex))
     (tcprepl fi fo)
     )
   ))

(function tcpreplproc (client)
 (let ((bld *BUILD*) (replfun tcprepl))
   (not.neth ((System.Net.Sockets.TcpClient client)
              (string bld) (object replfun))
      (sre = (client@GetStream))
      (sr = (new System.IO.StreamReader ((System.IO.Stream)sre)))
      (sw = (new System.IO.StreamWriter ((System.IO.Stream)sre)))
      (Runtime@setConsole ((System.IO.TextWriter)sw))
      (sw@WriteLine bld)
      (mbase replfun sr sw)
      (client@Close)
      (leave null)
      ))
 )

(function spawn-mbase-repl (port)
  (let* ((t (thr:mkthread (fun ()
                            (tcpwaiter (tcplistener port) tcpreplproc)))))
    (thr:start t)))



)

)