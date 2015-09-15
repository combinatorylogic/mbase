;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define  t_runtime (dotnet "Runtime"))
(function runfile (filename)
  ((r_tsbind t_runtime "runfile" string) filename))

;; R5RS-alike

(function call-with-input-file (fn f)
  ("Opens an input stream for the file [fn] and passes it as an argument to [f]."
   "After [f] execution, closes the stream and returns [f] evaluation value.")
  (let* ((fi (io-open-read fn))
         (res (f fi)))
    (io-close fi)
    res))

(function read-stream (fi)
  ("Returns a list of strings from the input stream [fi]."
   "Use it with caution!")
  (let* ((top (cons nil nil)))
   (let loop ((cur top) (ns (readline fi)))
      (if (null? ns) nil
         (let ((nw (cons ns nil)))
           (set-cdr! cur nw)
           (loop nw (readline fi)))))
   (cdr top)))


(function read-file (fn)
  ("Returns a list of strings from the text file [fn]."
   "Use it with caution!")
  (call-with-input-file fn read-stream))

(function call-with-output-file (fn f)
  ("Opens an output stream for the file [fn] and passes it as an argument to [f]."
   "After [f] execution, closes the stream and returns [f] value.")
  (let* ((fi (io-open-write fn))
         (res (f fi)))
    (io-wclose fi)
    res))

(function fprint (ostream string)
  "Writes a string into [ostream]."
  ((r_tbind "System.IO.StreamWriter" "Write" t_string) ostream string))

(function fprintln (fil str)
  "Writes a string and an endline into [ostream]."
  (fprint fil str) (fprint fil "\n"))


(define strNewline (list->string '(#\Newline)))

(function read-stream-list-big (fi)
  (let ((rrs (cons nil nil)))
       (let loop ((ns (readline fi)) (rs rrs))
          (if (null? ns) nil
              (let ((ll (string->list (string-append ns strNewline))))
                  (set-cdr! rs ll)
                  (loop (readline fi) (lasttail ll)))))
       (cdr rrs)
       ))

(define xread
  (r_tbind "Meta.Scripting.ExtendedReader" "Read"))

(function xread-stream-list-big (xfi)
  (let loop ((rs (cons nil nil)))
    (let ((ns (xread xfi)))
      (if (< ns 0) (begin
                          (set-cdr! rs nil)
                          nil)
          (let* ((ch (cons (mkchar ns) nil)))
            (set-cdr! rs ch)
            (set-cdr! ch (fun () (loop ch)))
            ch
            )))))


(function process-stream (fi fn)
  "Reads a stream [fi] line by line, applying a given function [fn] to each string."
  (let loop ((nl (readline fi)))
      (if (null? nl) nil
          (begin
            (fn nl)
            (loop (readline fi))))))

(function read-file-list-big (fn)
  (call-with-input-file fn read-stream-list-big))

(function read-file-list (fn)
  ("Reads a given file [fn] into a lazy list.")
  (let* ((fi (io-open-read fn))
         (clsd (cons nil nil))
         (rrs (cons 1 nil)))
       (let loop ((ns (readline fi)) (rs rrs) (cnt 0))
          (if (> cnt 1000)
            (begin
              (set-cdr! rs
                (fun ()
                  (if (cdr clsd) (cdr rrs)
                      (loop ns rrs 0))))
              (let ((ret (cdr rrs)))
                (set-cdr! rrs nil)
                ret))
            (if (null? ns) (begin (io-close fi) (set-cdr! clsd #t) (cdr rrs))
                (let ((ll (string->list (string-append ns strNewline))))
                  (set-cdr! rs ll)
                  (loop (readline fi) (lasttail ll) (+ cnt 1))))))
       ))


(function read-stream-big (fi final)
  (let ((trrs (cons 1 nil)))
       (let loop ((ns (readline fi)) (rs trrs) (rrs trrs) (cnt 0))
          (if (> cnt 150)
            (begin
               (set-cdr! rs
                  (fun ()
                    (let ((nrrs (cons 1 nil)))
                     (loop ns nrrs nrrs 0))))
               (let ((ret (cdr rrs)))
                  (set-cdr! rrs nil)
                  ret))
             (if (null? ns) (begin (final) (io-close fi) (cdr rrs))
                 (let ((ll (list ns)))
                     (set-cdr! rs ll)
                     (loop (readline fi) ll rrs (+ cnt 1))))))
       ))

(function read-file-big (fn)
  (let ((fi (io-open-read fn)))
     (read-stream-big fi (fun () nil))))

(macro cpath (str)
  ("Builds a proper lookup path for the given relative one."
   "Behaviour is similar to the (include ...) macro."
   )
  (let* ((oxpath (corelib:get-lookup-path))
         (fn  (buildstring oxpath "/" str)))
    fn))


(define streamreader.peek (r_tbind "System.IO.StreamReader" "Peek"))
(define extendedreader.peek (r_tbind "Meta.Scripting.ExtendedReader" "Peek"))

(function eof? (strm)
  (< (streamreader.peek strm) 0))

(function xeof? (strm)
  (< (extendedreader.peek strm) 0))