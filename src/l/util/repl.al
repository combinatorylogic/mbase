;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ctimex (include "../version.al"))
(n.module repl exe)

(function strvec (l)
  (if (null? l)
      (not.neth () (leave ((object)(mkarr string 0))))
      (let* ((n (length l)))
        (not.neth ((Pair l) (int n))
          (ar = (mkarr string n))
          (i = 0)
          (foreach (p l)
            ((aref ar i) <- p)
            (i <- (+ i 1)))
          (leave ((object)ar))))))

(function repl-top ()
   (println *BUILD*)
   (read-int-eval '(define compiler-optimise-cache nil))
   (read-int-eval '(n.module REPLDefault dll))
   (not.neth ()
      (sre = (new System.IO.StreamReader (System.Console@OpenStandardInput)))
      (Runtime@runfile sre true)
      (null)))

(function repl-top-int ()
   (println *BUILD*)
   (not.neth ()
      (sre = (new System.IO.StreamReader (System.Console@OpenStandardInput)))
      (Runtime@runfile "read-int-eval" sre true)
      (null)))

(function repl-top-int-file (fn arest)
  (read-int-eval '(n.module REPLDefault dll))
  (not.neth ((string fn) ((array string) arest))
      (sr = (new System.IO.StreamReader fn))
      (xpath = (System.IO.Path@GetDirectoryName fn))
      (Runtime@setargs arest)
      (Runtime@runfile "read-int-eval" sr false)
      (sr@Close)
      (null)))

(function repl-file (fn arest)
  (read-int-eval '(n.module REPLDefault dll))
  (not.neth ((string fn) ((array string) arest))
      (Runtime@setargs arest)
      (Runtime@runfile fn)
      (null)))

(function repl-file-emit (rep? fn arest)
  (repl-file fn arest)
  (when rep? (read-compile-eval '(n.report)))
  (read-compile-eval '(n.save))
  )

(function repl-eval-string (str)
  (not.neth ((string str))
     (sr = (new System.IO.StringReader str))
     (Runtime@runfile "read-int-eval" sr false)
     (sr@Close)
     (null)))

(function repl-help ()
  (iter println
        '("MBase REPL utility usage:"
          ""
          "repl /help        : see this help"
          "repl -            : evaluate from the standard input"
          "repl /i-          : REPL in an interpretation mode"
          "repl /v           : display version number and quit"
          ""
          "repl [options] [file-option] file-name.al ..."
          "                  : evaluate commands from file-name.al"
          ""
          " where options are:"
          "     /b           : fail on a first error"
          "     /l dll-name  : pre-load a given dll"
          "     /e string    : evaluate a string, exit if no input file given"
          ""
          " and file-option is:"
          "     /emit        : save a generated assembly after execution"
          "     /emits       : save a generated strong named assembly after execution"
          "     /i           : run in an interpretation mode"
          ""))
  (quit)
  )

(function main ( )
  (try
   (try
    (let* ((a (a->l *CMDLINE*)))
      (shashput (getfuncenv) 'main nil)
      (let loop ((args a))
       (p:match args
        (("/help" . $_) (repl-help))
        (("-h" . $_) (repl-help))
        (("--help" . $_) (repl-help))
        (("-") (repl-top))
        (("/i-") (repl-top-int))
        (("/b" . $rest) (shashput (getfuncenv) 'debug-compiler-failure #t)
                        (loop rest))
        (("/i" $fn . $rest) (repl-top-int-file fn (strvec rest)))
        (("/v") (println (ctime (S<< *VERSION*))))
        (("/emit" $fn . $rest) (repl-file-emit nil fn (strvec rest)))
        (("/emits" $fn . $rest) (repl-file-emit #t fn (strvec rest)))
        (("/l" $nm . $rest) (read-compile-eval `(sysdll ,(Sm<< nm))) (loop rest))
        (("/e" $str . $rest) (repl-eval-string str)
         (if rest (loop rest)))
        (($fn . $rest) (repl-file fn (strvec rest)))
        (else (repl-top)))))
    t_MBaseException
    (fun (me)
      (println "REPL MBase exception: ")
      (writeline (mbaseerror me))
      (println (->s me))
      (exit -1)
      ))
   t_Exception
   (fun (ex)
     (println "REPL Generic exception: ")
     (println (->s ex))
     (exit -1)
     )))

(n.save)