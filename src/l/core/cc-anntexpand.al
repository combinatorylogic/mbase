;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(recfunction cc:aexpand (mcenv l postn)
  (alet amcenv (cons *cmhash* mcenv)
  (cond
    ((null? l) l)
    ((asymbol? l)
     (alet l (asymbol l)
      (if (corelib:symbol-starts-with '## l)
        (let ((v (hashget-seq amcenv l))) (if v ((hashget-seq amcenv l)) '#f))
        l
        )))
    ((list? l)
     (cond
      ((aeqv? (car l) 'quote) (a:sanitise l))
      ((aeqv? (car l) 'inner-expand-with)
       (let ((nenv (cons (cadr l) mcenv))
             (code (caddr l)))
       (cc:aexpand nenv code postn)))
      ((aeqv? (car l) 'inner-expand-first)
       (cc:aexpand mcenv
                (map (lambda (v) (cc:aexpand mcenv v)) (cdr l))
                postn
                ))
      (else
       (let ((sh (if (asymbol? (car l))
                     (hashget-seq amcenv (asymbol (car l)))
                     #f)))
         (if sh
             (try
              (let ((pso (asymbol-pos (car l)))
                    (res (sh l)))
                (cc:aexpand amcenv res pso))
              t_Exception
              (lambda (x)
                (cc:comperror
                 `(CC04:EXPANDING ,(cc:elaborate-exception x) IN ,l))
                )
              )
             (map (lambda (ll) (cc:aexpand amcenv ll postn)) l)
             )))))
    (else l))))

(function cc:adefexpand (expr)
  (cc:aexpand (getmacroenv) expr nil))