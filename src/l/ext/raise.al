;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; raise static ref

(macro straise2 (code)
  (with-syms (nm xnm)
   `(begin
      (not.neth ()
        (lift-field (field object ,nm (private) (static)))
        (leave null)
        )
      (if (null? (not.neth () (leave ((object)(this # ,nm)))))
          (alet ,xnm ,code
                (not.neth ((object ,xnm))
                    (this # ,nm <- ,xnm)
                    (leave ,xnm)))
          (not.neth ()
                    (leave ((object)(this # ,nm))))))))



(unit-test 3 (let ((abc (fun (x) (list (straise2 (cons x x)) x))))
               (list (abc 10) (abc 20) (abc 30)))
           ( ((10 . 10) 10)
             ((10 . 10) 20)
             ((10 . 10) 30)))


