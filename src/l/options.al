;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ctimex (begin
  (#define prefix-tail      #t)  ; use .tail call isntructions
  (#define debug-compiler   #t)  ; allow inspecting intermediate code
  (#define optimise-case    nil) ; legacy case optimisation

  (#define option-asserts  #t)   ; compile asserts

  (#define optimise-cache #t)    ; use code and const cache
  (#define optimise-cache-threshold 200)
  (#define optimise-cache-length-threshold 3000)

  (#define copyright    "(c) by Meta Alternative Ltd., 2005-2015")

  ))
