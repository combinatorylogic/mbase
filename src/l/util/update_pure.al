;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rebuilds Pure.cs

(define MAXARGS 20)

(iter println '(
"using System;"
"using System.Collections;"
"using System.IO;"
"using System.Reflection;"
""
""
"namespace Meta.Scripting"
"{"))

(for (i 0 MAXARGS)
 (println
  (S<< "public delegate Object AltFun" i "("
       (if (> i 0) (strinterleave  (formap (j 0 i) (S<< "Object a" j)) ",") "")
       ");"))
 (println
  (S<< "public interface AltClosure" i " {"))
 (println
  (S<< "  Object run("
       (if (> i 0) (strinterleave  (formap (j 0 i) (S<< "Object a" j)) ",") "")
       ");}"))
 )


(println "}")





