;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(n.module pfrepl exe)
(sysdll MBaseFront)

(function pf-read-eval-print (lst)
  (alet res (read-compile-eval `(pfront-expand-string ,(list->string lst)))
    (println (S<< ":>> " (to-string res)))
    ))

(define sem (car (string->list ";")))
(function pf-read-eval-print-loop (redr)
  (let loop ((buf nil))
    (if (null? buf) (print "<< "))
    (format
     (p:match buf
       (($a $b . $r) (list a b))
       (else (list 0 0)))  (a b)
     (if (and (eq? a sem) (eq? b sem))
         (begin
           (pf-read-eval-print (reverse (cddr buf)))
           (loop nil))
         (alet chr (not.neth ((System.IO.StreamReader redr))
                        (chr = (redr@Read))
                        (object ret = null)
                        (if (>= chr 0) (ret <- ((object)((char)chr))))
                        (leave ret))
           (if (not chr) nil
               (loop (cons chr buf))))))))

(function main ()
  (shashput (getfuncenv) 'main nil)
  (corelib:set-lookup-path (not.neth () (leave
                                         (System.IO.Directory@GetCurrentDirectory))))
  (read-int-eval `(n.module DefaultPF))
  (pf-read-eval-print-loop
   (not.neth ()
       (leave ((object)(new System.IO.StreamReader
                            (System.Console@OpenStandardInput)))))))

