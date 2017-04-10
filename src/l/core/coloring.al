;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; graph:
;; ((vertex arity . <*vertices>)*)

(macro eqvertex? (v1 v2)
  `(eqv? ,v1 ,v2))

(function remove-vertex ( graph vertex )
  (<L> (let ((nv (<L> x | x <- vs & (not (eqvertex? x vertex)))))
         `(,v ,(length nv) ,@nv))
       | (v n . vs) <- graph & (not (eqvertex? v vertex))))

(macro sort-graph0 ( cf  graph )
  `(mqsort (fun (a b)
            (,cf (cadr a) (cadr b))) ,graph))

(ctime (if (shashget (getfuncenv) 'stage-final)
           '(macro sort-graph< ( graph ) `(__array_sort ,graph))
           '(macro sort-graph< ( graph ) `(sort-graph0 < ,graph))
           ))

(function prepare-graph ( ghash )
  (hashmap (fun (k v)
             `(,(string->symbol k) ,(length v) ,@v)) ghash))

(function prepare-lgraph ( grph )
  (map-over grph
            (fmt (k . v) `(,k ,(length v) ,@v))))


;; graph -> stack
(function lgraph-pushit ( graph )
  (let loop ((g graph) (s nil))
    (p:match g
      (() s)
      ((($vertex $num . $xxx) . $tl)
       (let* ((nw (remove-vertex tl vertex))
              (ns (sort-graph< nw)))
         (loop ns (cons `(,vertex ,@xxx) s)))))))

(function set-substr (a b)
  (let loop ((aa a) (bb b) (rr nil))
    (cond
     ((null? aa) (append bb rr))
     (else (let ((x (car aa)))
             (if (memq x bb)
                 (loop (cdr aa)
                       (filter (fun (v) (not (eqv? x v))) bb)
                       rr)
                 (loop (cdr aa) bb (cons x rr))))))))

(function findcolor (gencolor lst)
  (let* ((fixs (gencolor #t))
         (dif (set-substr fixs lst)))
    (if (null? dif) (gencolor nil)
        (car dif))))

(function wrap2 (x) (if (null? x) nil (wrap x)))

(function colourthestack ( precolor gencolor stak )
  (let loop ((done precolor)
             (s stak))
    (p:match s
      (() done)
      ((($vertex . $edges) . $next)
       (let* ((spars (<L> clr | e <- edges |
                          clr <- (wrap2 (lookup-env-car done e))))
              (clr (findcolor gencolor spars)))
         (loop `((,vertex ,clr) ,@done)
               next))))))

(function alltogether ( ghash colrs seq fixs )
  (let* ((precolor (map (fun (a) (list a a)) (append colrs fixs)))
         (colx (noconst (cons nil colrs)))
         (gencolor (fun (f)
                     (if f (cdr colx)
                         (let* ((nw (seq)))
                           (set-cdr! colx (cons nw (cdr colx)))
                           nw)))))
    (colourthestack
     precolor
     gencolor
     (lgraph-pushit
      (sort-graph<
       (prepare-graph ghash))))))



(recfunction eqd? (l1 l2)
  (cond
   ((symbol? l1) (eqv? l1 l2))
   ((and (list? l1) (list? l2))
    (and (eqd? (car l1) (car l2)) (eqd? (cdr l1) (cdr l2))))
   ((and (null? l1) (null? l2)) #t)
   (else nil)))

(function r3:graphcoloring ( result grph pfx nvars vars vfixs )
  (let* ((r (mkref 0))
         (seq (fun () (r! r (+ (deref r) 1)) (string->symbol (S<< pfx "_" (deref r))))))

    (let ((res (alltogether grph vars seq (hashmap (fun (x y) (string->symbol x))
                                                   vfixs))))
      (iter-over res
         (fmt (k v)
              (hashput result k v))))))

;; Allocate registers for the given list of instructions.
(function r3:allocateregisters ( registers graphs )
  ("Allocates registers for a given variables dependency graph, using a naive"
   "graph colouring heuristical algorithm.")
  (let ((subst (mkhash)))

    ;; Allocate registers for all the available type groups

    (format graphs (vs vf gs)
       (iter (cut hashput vf <> #t) registers)
       (foreach (g gs)
          (let ((vars
                 (map-car (filter (fmt (_ t) (eqv? t (car g))) vs)))
                (n (+ 1
                    (let ((nn (mkref 0)))
                     (hashiter (fun (k v)
                                 (let ((ll (length v)))
                                   (if (> ll (deref nn))
                                       (r! nn ll))))
                               (cadr g))
                     (deref nn)))))
            ;(writeline `(GG3: ,@vars ,n))
            ;; n is MAX COLORS
            (r3:graphcoloring subst (cadr g) (car g)
                              (+ 1 (/ n 2))
                              (map-car (filter (fmt (_ t) (eqv? t (car g)))
                                               registers)) vf))
            ))
    ;; Return the resulting substitution hashtable
    subst))

