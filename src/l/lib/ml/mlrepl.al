;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(n.module mlrepl exe)
(sysdll MBaseML)

(function ml-read-eval-print (lst)
  (alet res (read-compile-eval `(ml ,(list->string lst)))
    (println (S<< ":>> " (ml-pprint-value res)))
    ))

(define sem (car (string->list ";")))
(function ml-read-eval-print-loop (redr)
  (let loop ((buf nil))
    (if (null? buf) (print "<< "))
    (format 
     (p:match buf
       (($a $b . $r) (list a b))
       (else (list 0 0)))  (a b)
     (if (and (eq? a sem) (eq? b sem))
         (begin
           (ml-read-eval-print (reverse buf))
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
  (read-int-eval `(n.module DefaultML))
  (ml-read-eval-print-loop
   (not.neth ()
       (leave ((object)(new System.IO.StreamReader 
                            (System.Console@OpenStandardInput)))))))

