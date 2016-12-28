;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Generic parsing combinators.

; Sequence combinator
(function p<+> (p1 p2)
  "Sequence combinator."
   (fun (l)
      (let ((r1 (p1 l)))
         (if (p-fail? r1) r1
            (let ((r2 (p2 (p-rest r1))))
               (if (p-fail? r2)
                   (p-mkfail `(,(p-result r1) + ,(p-result r2)) l)
                   (p-mkresult (append (p-result r1) (p-result r2)) (p-rest0 r2))
                  ))))))


(macro pm<+> parsers
   "Sequence combinator, arbitrary number of parsers."
   (if (null? parsers) 'p.any
       (if (null? (cdr parsers)) (car parsers)
           `(p<+> ,(car parsers) (pm<+> ,@(cdr parsers))))))

; Selection combinator
(function p<|> (p1 p2)
   "Variant combinator."
   (fun (l)
      (let ((r1 (p1 l)))
         (if (p-success? r1)
               r1
               (p2 l)))))

(macro pm<|> parsers
   "Variant combinator, arbitrary number of parsers."
   (if (null? parsers) 'p.any
       (if (null? (cdr parsers)) (car parsers)
           `(p<|> ,(car parsers) (pm<|> ,@(cdr parsers))))))

; Negation combinator
(function p<!> (p)
  "Negation combinator"
   (fun (l)
      (if (null? l) (p-mkfail nil nil)
       (let ((r (p l)))
          (if (p-success? r)
              (p-mkfail `(not ,(p-result r)) l)
              (p-mkresult (list (car l)) (cdr l)))))))

; Multiplication combinator - N.B. - always gives a result.
(int-only
(function p<*> (p)
   "Parse none-or-many combinator."
   (fun (l)
      (let loop ((ll l) (rs nil))
         (let ((r (p ll)))
            (if (p-success? r)
                (loop (p-rest r) (append rs (p-result r)))
                (p-mkresult rs ll))))))
)
(cli-only
(function p<*> (p)
   "Parse none-or-many combinator."
  (fun (l)
    (let* ((res (noconst (cons 1 nil)))
           (ll l)
           (rs res))
       (n.label LBL)
       (let ((r (p ll)))
          (if (p-success? r)
              (begin
                 (set-cdr! rs (p-result r))
                 (n.stloc! rs (lasttail rs))
                 (n.stloc! ll (p-rest r))
                 (n.goto LBL) )))
       (p-mkresult (cdr res) ll))))
)

; One-or-many combinator
(function p<+*> (p)
  "Parse-one-or-many combinator."
  (p<+> p (p<*> p)))


; Result processing combinator

(function p<R> (p f)
  "Parsing result processing combinator."
  (fun (l)
     (let ((r (p l)))
        (if (p-success? r)
            (p-mkresult (f (p-result r)) (p-rest0 r))
            r))))

(function p<xR> (p f)
  (fun (l)
     (let ((r (p l)))
        (if (p-success? r)
            (p-mkresult (f (p-result r) (p-rest r)) (p-rest0 r))
            r))))

(function p<dR> (p f)
  (fun (l)
     (let ((r (p l)))
        (if (p-success? r)
            (p-mkresult (f (p-result r) nil) (p-rest0 r))
            r))))


(function p<?> (p)
  (fun (l)
     (let ((r (p l)))
        (if (p-success? r) r
            (p-mkresult nil l)))))


(function p<?|> (p dr)
  (fun (l)
     (let ((r (p l)))
        (if (p-success? r) r
            (p-mkresult (list dr) l)))))

(function p<drop> (p)
  (fun (l)
      (let ((r (p l)))
         (if (p-success? r)
             (p-mkresult nil l)
             (p-mkfail nil l)))))

