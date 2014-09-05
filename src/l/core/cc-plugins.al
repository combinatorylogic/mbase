;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cc:core-plugins-list (cons nil nil))
(define cc:pre-lift-plugins-list (cons nil nil))
(define cc:after-lift-plugins-list (cons nil nil))
(define cc:flat-plugins-list (cons nil nil))
(define cc:dotnet-plugins-list (cons nil nil))

(function cc:apply-plugins (lst0 e0)
  (let loop ((lst (cdr lst0)) (e e0))
   (if (null? lst) e
       (alet res ((car lst) e)
	     (loop (cdr lst) res)))))

(function  cc:core-plugins (e)
  (cc:apply-plugins cc:core-plugins-list e))

(function  cc:pre-lift-plugins (e)
  (cc:apply-plugins cc:pre-lift-plugins-list e))

(function  cc:after-lift-plugins (e)
  (cc:apply-plugins cc:after-lift-plugins-list e))

(function  cc:flat-plugins (e)
  (cc:apply-plugins cc:flat-plugins-list e))
 
(function  cc:dotnet-plugins (e)
  (cc:apply-plugins cc:dotnet-plugins-list e))

(function cc:add-plugin (part fn)
  ("Add a plugin function to the compilation chain."
   "Possible part names are: core, pre-lift, after-lift, flat, dotnet."
   "Plugin function takes one argument and returns the value of the same format as its argument."
   )
  (alet wh
    (case part
      ((core) cc:core-plugins-list)
      ((pre-lift) cc:pre-lift-plugins-list)
      ((after-lift) cc:after-lift-plugins-list)
      ((flat) cc:flat-plugins-list)
      ((dotnet) cc:dotnet-plugins-list)
      (else (ccerror `(NO-SUCH-STAGE ,part))))
    (set-cdr! wh (cons fn (cdr wh)))
    (cdr wh)
    ))


