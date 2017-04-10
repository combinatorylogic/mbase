;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(function cc:elaborate-exception (ex)
  (if (instanceof ex t_MBaseException)
      (mbaseerror ex)
      (->s ex)))

(recfunction cc:expand:inner (expenv stk mcenv l)
  (let ((amcenv (cons *cmhash* mcenv))
        (nstk (cons l stk)))
  (shashput (getfuncenv) '**cc-expand-stack** nstk)
  (cond
    ((null? l) l)
    ((symbol? l)
     (if (corelib:symbol-starts-with '## l)
       (let ((v (hashget-seq amcenv l))) (if v ((hashget-seq amcenv l)) 'nil))
       l
     ))
    ((list? l)
     (let ((cl (car l)))
     (cond
      ((eqv? cl 'quote) l)
      ((eqv? cl 'unquote-spec) l)
      ((eqv? cl 'inner-expand-with)
       (let ((nenv (cons (cadr l) mcenv))
             (code (caddr l)))
         (cc:expand:inner expenv nstk nenv code)))
      ((eqv? cl 'inner.lambda)
       `(inner.lambda ,(cadr l)
          ,@(map (lambda (ll) (cc:expand:inner expenv nstk amcenv ll)) (cddr l))))
      ((eqv? cl 'inner.reclambda)
       `(inner.reclambda ,(cadr l) ,(caddr l)
          ,@(map (lambda (ll) (cc:expand:inner expenv nstk amcenv ll)) (cdddr l))))
      ((eqv? cl 'inner.with-added-metadata)
       `(inner.with-added-metadata ,(cadr l)
          ,@(map (lambda (ll) (cc:expand:inner expenv nstk amcenv ll)) (cddr l))))
                                   
      ((eqv? cl 'inner-expand-first)
       (cc:expand:inner expenv nstk mcenv
                (map (lambda (v) (cc:expand:inner expenv nstk mcenv v)) (cdr l))))
      ((eqv? cl 'inner.debugpoint)
       (begin (ohashput expenv 'debugpoint (cdr l))
              (return l)))
      (else
       (let ((sh (p:match cl
                   ($$M (hashget-seq amcenv cl))
                   ((inner.identmetadata $id . $md)
                    (let ((tmp (hashget-seq amcenv id)))
                      (if tmp ((deref cc:process-variable-metadata)
                               'macro id md))
                      tmp))
                   (else nil))))
         (if sh
             (try
              (let ((res (sh l)))
                (cc:expand:inner expenv nstk amcenv res))
              t_Exception
              (lambda (x)
                (alet lastdbg (ohashget expenv 'debugpoint)
                (cc:comperror
                 `(CC04:EXPANDING ,@(if lastdbg `((NEAR ,lastdbg))) ,(cc:elaborate-exception x) IN ,l))
                ))
              )
             (map (lambda (ll) (cc:expand:inner expenv nstk amcenv ll)) l)
             ))))))
    (else l))))

(function cc:expand (mcenv l)
  (cc:expand:inner (mkhash) nil mcenv l))

(function cc:defexpand (expr)
  (cc:expand (getmacroenv) expr))