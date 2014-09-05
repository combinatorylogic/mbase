;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2014, Meta Alternative Ltd. All rights reserved.
;; This file is distributed under the terms of the Q Public License version 1.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(Section "Generic register scheduling library")

;;;
;;; Aux generic register machine used to
;;;   reschedule variables.
;;;

(def:ast reg3a ( )
  (*TOP* <exprs>)
  
  (exprs <*expr:es>)

  (expr
   (| 
    (mov <rvalue:tgt> <rvalue:src>)
    (gen .  <*rvalue:srcs>)
    (kill . <*rvalue:tgts>)
    (genkill <*rvalue:tgts> <*rvalue:srcs>)
    (label <ident:l>)
    (goto <ident:l>)
    (nop)
    (gotocond <*rvalue:cnd> <ident:l>)
    (gotoconds <*rvalue:cnd> <*ident:l>)
    ;; Compiler control
    (fixvar <varname:r>)
    ))

  (rvalue
   (| (V . <var:n>)
      (C)
      (D . <*var:ns>)))

  (var (<varname:nm> <type:tp>))
  )

(rec:def rreg3a_i lbl gen kill in out e)

(function r3:addrvalue (addf rval)
  ((ast:iter reg3a rvalue
   (var _ (addf node)))
   rval))

(function r3:addrvalue-k (addg addk rval)
  (case (car rval)
    ((V) (r3:addrvalue addk rval))
    ((D) (r3:addrvalue addg rval))
    (else nil)))

(function r3:mkgenkill ( expr )
 (collector (genadd genget)
  (collector (killadd killget)
   (let ((addgen (cut r3:addrvalue genadd  <>))
         (addkil (cut r3:addrvalue-k genadd killadd <>)))
    (<> expr
     (ast:iter reg3a expr
        (expr _
          ((mov (addkil tgt) (addgen src)) ;; special case!
           (gen (iter addgen srcs))
           (kill (iter addkil tgts))
           (genkill
            (begin
              (iter addgen srcs)
              (iter addkil tgts)))
           (gotocond (iter addgen cnd))
           (gotoconds (iter addgen cnd))
           (else nil))))))
   (list (genget) (killget) nil nil))))

(function r3:intermediate1 ( exprs )  
  (with-sequence ( tg )
    (<> exprs
      (ast:visit reg3a exprs
        (expr _ 
          ((label (rreg3a_i.new l () () () () node))
           (else  (format (r3:mkgenkill node) (gen kill . _)
                          (rreg3a_i.new (tg) gen kill nil nil node)))))))))

;; Aux function: gets the next inst label
(function r3:getnext ( texpr defnx )
  (collector (add get)
   (<> (rreg3a_i.e texpr)
      (ast:iter reg3a expr
        (expr _ (
          (goto (add l) ) ; do not add default next, it's an explicit jump
          (gotocond (add l) (iter add defnx)) ; conditional jump
          (gotoconds (iter add l) (iter add defnx)) ; conditional jump
          (else (iter add defnx)))))) ; use default
    (get)
    ))

;; Build an order table
(function r3:ordergraph ( texprs )
  (with-hash (h)
   (when texprs
   (let loop ((prev (car texprs))
              (rest (cdr texprs)))
     (alet tag (rreg3a_i.lbl prev)
        (if (null? rest)
            (h! tag (r3:getnext  prev nil))
            (begin
              (h! tag (r3:getnext  prev
                                  (list (rreg3a_i.lbl (car rest)))))
              (loop (car rest) (cdr rest)))))))
   h))

;; Rebuild a quick access hash (to be called for each
;;   liveness propagation step)
(function r3:rehash ( ht texprs )
  (foreach (t texprs)
    (hashput ht (rreg3a_i.lbl t) t))
  ht)

;; Aux functions

; clean up the list of registers, leaving only one entry for each.
(function r3:unifiq ( lst )
  (collector (add get)
  (with-hash (h)
    (foreach (v lst)
      (alet cv (car v)
            (when (not (h> cv))
              (h! cv v)
              (add v)))))
  (get)))

; merge to sets of registers
(function r3:set-merge (ls)
  (collector (add get)
  (with-hash (h)
   (foreach (a ls)
    (foreach (i a)
      (alet ci (car i)
        (when (not (h> ci))
              (h! ci #t)
              (add i)))))
    (get))))

; substract b from a, where a and b are registers sets.
(function r3:set-substr (a b)
  (with-hash (h1)
      (foreach (bb b)
        (h1! (car bb) #t))
      (collector (add get)
        (foreach (aa a)
          (when (not (h1> (car aa)))
                (add aa)))
        (get))))

; return 'in' list for an instruction of a given label
(function r3:get-in (ht tag)
  (let* ((v (hashget ht tag)))
    (rreg3a_i.in v)))

; return a united 'in' list for all the instructions of given labels
(function r3:get-ins (ht tags)
  (r3:set-merge
    (foreach-map (tag tags)
      (r3:get-in ht tag))))

;; Solve one iteration towards a fixed point:
;;   propagating the register liveness information
;; TODO: replace with a single iteration solution!
(function r3:solve1 ( texprs nhsh ehsh )
  (let ((retp (mkref nil)))
    (foreach (t texprs)
      (let* ((lbl (rreg3a_i.lbl t))
             (sc (hashget nhsh lbl))
             (newout
              (r3:get-ins ehsh sc))
             (newin
              (r3:set-merge (list (rreg3a_i.gen t)
                                  (r3:set-substr newout 
                                                 (rreg3a_i.kill t))))))
        (if (not
             (and (= (length newout) (length (rreg3a_i.out t)))
                  (= (length newin) (length (rreg3a_i.in t)))))
            (r! retp #t))
        (rreg3a_i.in! t newin)
        (rreg3a_i.out! t newout))
      )
    (if (deref retp) texprs nil)))

;; Solve liveness equations against the given prepared instructions set
(function r3:solve ( texprs )
  ("Solves liveness equations against the given prepared instructions list.")
  (let* ((hseq (r3:ordergraph texprs))
         (whsh (r3:rehash (mkhash) texprs)))
    (let loop ((next (r3:solve1 (reverse texprs) hseq whsh)))
      (if (null? next) texprs
          (loop (r3:solve1 next hseq whsh))))))


;; Give a list of variables, table of fixed variables and a list of types
;; for a given instructions set.
(function r3:listvars ( texprs )
  (collector (add get) (collector (a1 g1) (collector (ta tg)
    (foreach (t texprs)
    (<> (rreg3a_i.e t)
        (ast:iter reg3a expr
            (expr DEEP
             ((fixvar (begin (a1 r) node))
              (else node)))
            (var _
              (begin (add node) (ta tp)))
           )))
    (list 
     (r3:unifiq (get))
     (let* ((h (mkhash)))
       (iter (cut hashput h <> #t)
             (unifiq (g1))) h)
     (unifiq (tg)))
     ))))

;; Aux function: adds an arc to graph
(function r3:graphadd (h k v)
  (let* ((vv (hashget h k))
         (kk (hashget h v)))
    (hashput h k (unifiq (cons v vv)))
    (hashput h v (unifiq (cons k kk)))
    ))

;; Build an interference graph
(function r3:mkgraph (  texprs vs type )
  (let* ((ll (mkhash))
        (tx (mkhash)))

  (foreach (v vs)
     (if (eqv? type (cadr v))
         (begin
           (hashput tx (car v) #t)
           (hashput ll (car v) nil)))) ;; prefill edges

  (foreach (t texprs)
    (let ((kill (rreg3a_i.kill t))
          (out  (rreg3a_i.out t)))
      (foreach (k kill)
        (foreach (o out)
          (if (not (eqv? (car k) (car o)))
              (if (and (hashget tx (car k))
                       (hashget tx (car o)))
                  (r3:graphadd ll (car k) (car o))
                  ))))))
  ll))

;; Build interference graphs for all the register types in the
;; instructions list
(function r3:mkgraphs ( texprs )
  (letf (( (vs vf tps) (r3:listvars texprs)))
    (let ((grphs
           (foreach-map (t tps)
             (list t (r3:mkgraph texprs vs t)))))
      (list vs vf grphs))))

; The only interface function here.
(function r3:lgraphs ( exprs )
  ("Builds variables interference graphs for a given list of instructions,"
   "for all the variables types found in the code."
   )
  (let* ((texprs0 (r3:intermediate1 exprs))
         (texprs (r3:solve texprs0)))
    (let* ((res
            (r3:mkgraphs texprs)))
      res)))

