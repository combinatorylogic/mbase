;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
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
    