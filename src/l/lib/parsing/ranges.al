;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(function packrat-skip-terminal-p (env tname)
  (hashget env (S<< " skip: " tname)))

(function packrat-trivial-range (p)
  (pktrivial:visit pred p
     (pred DEEP (
        (fail '(-))
        (char `(,chr))
        (anychar '(-))
        (range `((,from ,to)))
        (or (foreach-mappend (p ps) p))
        (string (if str (list (car str))))
        (sstring (alet ss (string->list str)
                   (if ss (list (ascii (car ss))))))))))

(function packrat-skip-trivial-p (p)
  (pktrivial:visit pred p
     (pred DEEP (
        (or (if (filter I ps) #t))
        (string (if str nil #t))
        (sstring (alet ss (string->list str)
                   (if ss nil #t)))
        (else nil)
        ))))

(function packrat-skip-p (env e)
  (packrat:visit expr e
    (expr DEEP
      ((palt
        (if (filter I es) #t))
       (pdalt
        (if (filter I es) #t))
       (merge e)
       (seq
        (let loop ((e es))
          (p:match e
            (($hd . $tl) (and hd (loop tl)))
            (() #t))))
       (notp #t)
       (plus e)
       (star #t)
       (maybe #t)
       (withfilter e)
       (trivial (packrat-skip-trivial-p p))
       (withignore e)
       (bind-terminal
        (packrat-skip-terminal-p env tname))
       (terminal
        (packrat-skip-terminal-p env name))
       (simple
        (packrat-skip-terminal-p env name))
       (bind e)
       (else nil)
       ))))

(function packrat-pack-ranges (lst)
  (collector (add get)
   (let loop ((l (cdr lst)) (p (car lst)) (b (car lst)))
    (alet check (fun ()
                  (if (eq? p b) (add p)
                      (add `(,b ,p))))
    (p:match l
      (($h . $t)
       (if (> (- h p) 1)
           (begin
             (check)
             (loop t h h))
           (loop t h b)))
      (else (check)))))
   (get)))

(function packrat-expand-ranges (add minus xs)
  (foreach (x xs)
    (p:match x
      (- (r! minus #t))
      (($f $t) (for (i f (+ t 1)) (add i)))
      ($o (add o))
      )))

(function packrat-merge-ranges (env rs)
  (alet undef (mkref nil)
  (alet nxt
    (collector (add get)
     (alet minus (mkref nil)
      (foreach (r rs)
        (p:match r
          ((T $nm)
           (alet t (hashget env nm)
              (if t
                  (if (eqv? t 'undef)
                      (r! undef #t)
                      (packrat-expand-ranges add minus t)
                      )
                  (r! undef #t)
                  )))
          ((V . $xs)
           (packrat-expand-ranges add minus xs)
           )))
      (if (deref minus) nil
          (alet g (get)
            (qsort < g)))))
    (if (deref undef) 'undef
      (if nxt
        (packrat-pack-ranges nxt)
        'undef)))))

(function packrat-get-leftstuff (env e)
  (collector (add get)
   (let loop ((expr e))
     (packrat:iter expr expr
      (expr _
       ((palt
         (foreach (e es)
           (loop e)))
        (pdalt
         (foreach (e es)
           (loop e)))
        (merge
         (loop e))
        (seq
         (p:match es
           (($fst . $nxt)
            (begin
              (loop fst)
              (if (packrat-skip-p env fst)
                  (loop `(seq ,@nxt)))))))
        (terminal
         (add `(T ,name)))
        (bind-terminal
         (add `(T ,tname)))
        (simple
         (add `(T ,name)))
        (andp (loop e))
        (plus (loop e))
        (star (loop e))
        (maybe (loop e))
        (withfilter (loop e))
        (withignore (loop e))
        (bind (loop e))
        (trivial (add `(V ,@(packrat-trivial-range p))))
        (else nil)
        ))))
   (packrat-merge-ranges env (get))
   ))

(function packrat-reckon-ranges-0 (deps code)
  (let* ((env (mkhash))
         (undefn (mkref nil))
         (pass (fun ()
                 (foreach (c code)
                   (p:match
                       (p:match c
                         ((binaries $_ $_ $name $v . $_)
                          `(T ,name ,v))
                         ((terminal $_ $_ $name $v . $_)
                          `(T ,name ,v)))
                     ((T $name $v)
                      (hashput env (S<< " skip: " name)
                               (packrat-skip-p env v))
                      (alet chk1 (hashget env name)
                        (when (or (null? chk1) (eqv? chk1 'undef))
                           (alet rs (packrat-get-leftstuff env v)
                                 (if (eqv? rs 'undef) (r! undefn #t))
                                 (hashput env name rs))))))))))
    (foreach (d deps)
      (p:match d (($nm $rng) (hashput env nm rng))))
    (foreach (c code)
      (p:match c
        ((terminal $_ $_ $name $v . $_)
         (hashput env name 'undef))
        ((binaries $_ $_ $name $v . $_)
         (hashput env name 'undef))
        ))
    (let loop ((cnt 0))
      (when (< cnt 9)
       (r! undefn nil)
       (pass)
       (if (deref undefn) (loop (+ cnt 1)))))
    env
    ))

(function packrat-reckon-ranges (deps code)
  (ctime (if ##packrat-optimised
             `(packrat-reckon-ranges-0 deps code)
             `(mkhash))))
