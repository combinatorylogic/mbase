;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(function peg-extract-bindings (expr)
  (collector (add get)
    (packrat:iter expr expr
      (expr DEEP
        ((bind (p:match e
                 ((terminal $id . $rec)
                  (add `(,name ,id)))
                 (else nil)))
         (bind-terminal
          (add `(,fname ,tname)))
         (else nil))))
    (get)))

(function peg-terminal-structure (tcode)
  (collector (add get)
  (<> tcode
      (ast:revisit loop ((mult nil))
                   packrat expr

          ((palt (foreach (e es) (loop e nil)))
           (pdalt (foreach (e es) (loop e #t)))
           (notp nil) ;; no bindings are allowed inside 'not'
           (star (loop e #t))
           (maybe (loop e #t))
           (plus (loop e #t))
           (withignore (loop e mult))
           (bind
            (add `(,name ,mult _)))
           (bind-terminal
            (if (not (eqv? fname '*val*))
                (add `(,fname ,mult ,tname))))
           ) ()))
  (if (null? (get)) ;; no bindings or topalts
      nil ;;`((val () ,(car tcode) ()))
      (get))))


(function n2s (och)
  (if (null? och) ""
      (not.neth ((int och))
        (ch = ((char)och))
        (leave ((object)ch)))))

(function peg-terminal-getrange (tcode)
  (collector (add0 get)
  (let loop ((e tcode) (ignor nil))
    (alet add (fun (v) (add0
                         (if ignor `(IGNORE ,ignor ,v) v)))

    (packrat:iter expr e
      (expr _
        ((seq (if es (loop (car es) ignor)))
         (palt (foreach (ee es) (loop ee ignor)))

         (trivial (add p))
         (terminal (add `(REF ,name)))
         (simple (add `(REF ,name)))
         (bind-terminal (add `(REF ,tname)))

         (withignore (loop e terms))

         (else-deep
          (forall node)))))))
  (get)))


;(function peg-range-merge (r1 r2)
;  (p:match (list r1 r2)
;    ((

;(function peg-terminal-packrange (range)
;  (foldl peg-range-merge '(nop) range))



;;;; Benchmarking tools

(function mkstopwatch ()
  (not.neth ()
            (leave ((object)(new System.Diagnostics.Stopwatch)))))

(function stopwatch-start (sw)
  (not.neth ((System.Diagnostics.Stopwatch sw))
            (sw@Start)
            (leave null)))

(function stopwatch-stop (sw)
  (not.neth ((System.Diagnostics.Stopwatch sw))
            (sw@Stop)
            (leave null)))

(function stopwatch-elapsed (sw)
  (not.neth ((System.Diagnostics.Stopwatch sw))
            (leave ((sw@get_Elapsed)@ToString))))

(macro swbenchmark (msg code)
    (with-syms (sw ret)
       `(let* ((,sw (mkstopwatch))
               (_ (stopwatch-start ,sw))
                      (,ret ,code))
          (stopwatch-stop ,sw)
          (println (S<< "Elapsed time (" ,msg "): " (stopwatch-elapsed ,sw)))
          (return ,ret))))

(macro swbenchmark0 (msg code)
  (if (shashget (getfuncenv) 'debug-compiler-benchmarks)
      `(swbenchmark ,msg ,code)
      code))