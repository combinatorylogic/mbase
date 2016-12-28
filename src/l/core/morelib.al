;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(Section "Miscellaneous")

(topblock
;;;
(function read-some-streams (fl)
   "Performs a parallel stream reading, [fl] is a list of input streams."
   (let* ((tops (map (fun (x) (mkref)) fl)))
     (let loop ((curs tops) (nss (map readline fl)))
       (if (null? (filter (fun (x) (not (null? x))) nss))
           nil
           (let* ((zzz (czip curs nss))
                  (nxt (map-over zzz
                           (fmt (cr . ns)
                             (if (null? ns) cr
                               (let ((nw (cons ns nil)))
                                 (set-cdr! cr nw)
                                 nw))))))
              (loop nxt (map-over (czip nss fl)
                         (fmt (ns . fi)
                            (if (null? ns) nil (readline fi))))))))
     (map cdr tops)))

;; strinterleave

(function strinterleave (lst str)
  "Builds a string of lst elements interleaved with str."
  (foldl string-append ""
    (interleave lst str)))

;; map shortcuts

(macro map-car (l) "Same as (map car ...)" `(map car ,l))
(function map-car (l) "Same as (map car ...)" (map car l))
(macro map-cadr (l) "Same as (map cadr ...)" `(map cadr ,l))
(function map-cadr (l) "Same as (map cadr ...)" (map cadr l))
(macro map-cdr (l) "Same as (map cdr ...)" `(map cdr ,l))
(function map-cdr (l) "Same as (map cdr ...)" (map cdr l))

(function select-car (v l) (filter (fun (x) (eqv? (car x) v)) l))


(macro [ (idx ] arr)
   "(aget arr idx) wrapper"
   (if (not (eqv? ] '])) `([...] error)
      `(aget ,arr ,idx)))

(function multihashput (ht key val)
   (let* ((v (hashget ht key))
          (nv (if v (cons val v) (list val))))
      (hashput ht key nv)))

(function dhashput (ht key1 key2 val)
  (let* ((v (hashget ht key1))
         (h2 (if (null? v) (let* ((hh (mkhash))) (hashput ht key1 hh) hh)
                 v)))
    (hashput h2 key2 val)))

(function dhashget (ht key1 key2)
  (let ((h (hashget ht key1)))
    (if (null? h) nil
        (hashget h key2))))


(function cons-cdr! (l v)
   (set-cdr! l (noconst (cons v (cdr l)))))

(macro <..> (a b) `(fromto ,a (+ 1 ,b)))

(function partition (f lst)
  (let* ((rst (mkref))
         (r (let loop ((l lst))
              (cond
               ((null? l) l)
               ((f (car l)) (cons (car l) (loop (cdr l))))
               (else (set-cdr! rst l) nil)))))
    (cons r (cdr rst))))

(function alltrue (f lst)
  (let loop ((l lst))
    (cond
     ((null? l) #t)
     ((f (car l)) (loop (cdr l)))
     (else nil))))

(function genunifiq (cmp lst)
  (let loop ((crnt nil) (l lst))
    (if (null? l) crnt
        (let ((cl (car l)))
          (if (alltrue (fun (x) (not (cmp x cl))) crnt)
              (loop (cons (car l) crnt) (cdr l))
              (loop crnt (cdr l))
              )))))

)

(macro lazy (ex)
  "Makes a lazy value of an expression. Evaluation can be forced later with [(lazyref ...)]."
  (let ((v (gensym)))
    `(let ((,v (noconst (cons nil 'lazy:not-instantiated))))
       (fun ()
         (if (eqv? (cdr ,v) 'lazy:not-instantiated) (set-cdr! ,v ,ex))
         (cdr ,v)
         ))))

(macro lazyref (v)
  "Forces an evaluation of a lazy value."
  `(,v))

(macro with-hash (names . body)
  ("Defines new hash tables with given names and creates shortcut macros for"
   " accessing them. For example: [[(with-hash (ht) (ht! 'a 1) ht)]]")
  `(__with-hash () ,names ,@body))

(macro with-ohash (names . body)
  ("Defines new hash tables with given names and creates shortcut macros for"
   " accessing them. For example: [[(with-ohash (ht) (ht! 'a 1) ht)]]")
  `(__with-hash #t ,names ,@body))

(macro __with-hash (o? names . body)
  (let ((Xhashput (if o? 'ohashput 'hashput))
        (Xhashget (if o? 'ohashget 'hashget)))
  `(let ,(foreach-map (n names) `(,n (mkhash)))
     (with-macros (
                   ,@(foreach-map (n names)
                      `(,(Sm<< n "!")
                        (fmt (_ a b) (list (quote ,Xhashput) (quote ,n) a b))))
                   ,@(foreach-map (n names)
                      `(,(Sm<< n ">")
                        (fmt (_ a) (list (quote ,Xhashget) (quote ,n) a))))
                   ,@(foreach-map (n names)
                      `(,(Sm<< n "+!")
                        (fmt (_ a b) (list (quote ,Xhashput) (quote ,n) a
                                         (list 'cons b
                                               (list (quote ,Xhashget) (quote ,n) a))
                                         ))))
                   )
                  ,@body))))

(macro use-hash (names . body)
  ("Creates shortcut macros for accessing listed hashtables.")
  `(__use-hash () ,names ,@body))

(macro use-ohash (names . body)
  ("Creates shortcut macros for accessing listed hashtables.")
  `(__use-hash #t ,names ,@body))

(macro __use-hash (o? names . body)
  (let*
       ((outer (filter (M@ not null?)
                       (foreach-map (n names)
                         (if (list? n) n nil))))
        (Xhashput (if o? 'ohashput 'hashput))
        (Xhashget (if o? 'ohashget 'hashget))
        (iner
         `(with-macros
           ,(foldl append nil
                   (foreach-map (n (map (fun (x) (if (list? x) (car x) x)) names))
                     `((,(Sm<< n "!")
                        (fmt (_ a b) (list (quote ,Xhashput) (quote ,n) a b)))
                       (,(Sm<< n ">")
                        (fmt (_ a) (list (quote ,Xhashget) (quote ,n) a)))
                       (,(Sm<< n "+!")
                        (fmt (_ a b) (list (quote ,Xhashput) (quote ,n) a
                                         (list 'cons b
                                               (list (quote ,Xhashget) (quote ,n) a))
                                         )))
                       )))
           ,@body)))
    (if outer `(let ,outer ,iner) iner)))


(recfunction iso (a b)
  (cond
   ((and (null? a)
         (null? b)) #t)
   ((and (symbol? a)
         (symbol? b)
         (eqv? a b)) #t)
   ((and (list? a)
         (list? b)
         (iso (car a) (car b))
         (iso (cdr a) (cdr b))) #t)
   ((and (string? a)
         (string? b)
         (eq? a b)) #t)
   (else (eq? a b))))
