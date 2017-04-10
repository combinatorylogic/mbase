;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define pfront-macros-defined #t)

(macro flatbegin-inside-begin-with-defs code
  `(begin-with-defs ,@code))

(macro pfront.debugpoint x
  'nil)

(cmacro pfront.debugpoint x
  `(inner.debugpoint ,@x))

(macro pfront.fixlocal x
  'nil)

(cmacro pfront.fixlocal x
  `(inner.fixlocal ,@x))

(macro pfront.with-format (e f body)
  `(p:match ,e
     (,f ,body)
     (else (ccerror (list 'FORMAT-FAILED ,(list 'quasiquote f))))))

(macro pfront-collector (args . body)
  (format args (addnm getnm)
  (with-syms (clec cren v v1)
    `(let* ((,cren (noconst (cons nil nil)))
            (,clec (mkref ,cren)))
       (let-metadata 
        ((,(cdr addnm) (fun (,v) (alet ,v1 (cons ,v nil)
                                       (set-cdr! (deref ,clec) ,v1)
                                       (r! ,clec ,v1)
                                       ,v)))
         (,(cdr getnm) (fun () (cdr ,cren))))
        ,@body)))))


(macro begin-with-defs code
  `(begin
     ,@(let loop ((c code))
         (p:match c
           (((flatbegin-inside-begin-with-defs . $r1) . $rest)
            (loop `(,@r1 ,@rest)))
           (((inblock-def (var $nm . $md) $v) . $rest)
            `((let-metadata (((,nm ,@md) ,v))
                ,@(if (shashget (getfuncenv) 'compiler-debug-enabled)
                      `((pfront.fixlocal ,nm)) nil)
                (begin-with-defs ,@rest))))
           (((inblock-def-format $f $v) . $rest)
            `((p:match ,v (,f (begin-with-defs ,@rest))
                     (else (ccerror (list 'FORMAT-FAILED ,(list 'quasiquote f)))))))
           (($hd . $tl)
            (cons hd (loop tl)))
           (() nil)))))

