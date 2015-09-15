;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \section{Stage $1^c$ --- building the interpreted compiler}
;-

(ctimex (println "Bootstrap stage: C"))
(include "../core/compiler.al")
(force-class-flush)
(gensym-counter-set (ctime (cdr *gensym-counter-storage*)))