;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; some extra common macros
;;; UNDOCUMENTED - for internal use only!

(recfunction extra:find-opt-argvalue (lst nm)
  (p:match lst
    (() 'nil)
    ((=nm $v . $rest) v)
    (($x $v . $rest) (extra:find-opt-argvalue rest nm))
    (else 'nil)))

(macro extra:with-optional-args ( rlist alist . body)
  (let ((code
	 (foreach-map (a alist)
	   `(,a ,(extra:find-opt-argvalue rlist
					  (string->symbol
					   (S<< ":" a)))))))
    `(let ,code
       ,@body)))


(macro extra:generic-include ( reader fnm )
  (let* 
      ((misc-reader (eval reader))
       (oxpath (corelib:get-lookup-path))
       (fn  (buildstring oxpath "/" fnm))
       (fi (mkreader (io-open-read fn)))
       (res `(top-begin
	       (ctimex (corelib:set-lookup-path ,(_getpath fn)))
	       ,@(let loop () (let ((r (misc-reader fi)))
				(if (null? r) nil
				    (cons r (loop)))))
	       (ctimex (corelib:set-lookup-path ,oxpath))
	       )))
    (xio-close fi)
    res))


