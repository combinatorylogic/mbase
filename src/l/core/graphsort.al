;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;- A naive but fast linear sorting engine for graph colouring
;-
;- The idea is simple: range of numbers is typically quite small, so we can allocate an
;- array for a known range, fill it in a linear time and build a list out of it.
;- This must be significantly faster than quick sorting lists.
;-

(function __array_sort (rng)
  (if rng
  (let ((mn 65536)
        (mx 0))
    (foreach (r rng)
      (alet v (cadr r)
        (if (< v mn) (n.stloc! mn v))
        (if (> v mx) (n.stloc! mx v))))
    (let* ((nn (+ 1 (- mx mn)))
           (ar (anew t_object nn)))
      (foreach (r rng)
        (let* ((pos (- (cadr r) mn))
               (vlu ([ pos ] ar)))
          (aset ar pos (cons r vlu))))
      (alet res nil
        (for (i 0 nn)
          (alet vv ([ i ] ar)
                (if vv (foreach (v vv) (n.stloc! res (cons v res))))))
        res)))))


