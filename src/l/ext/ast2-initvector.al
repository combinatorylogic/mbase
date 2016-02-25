(macro ast2-init-n-vector vlst
  (with-syms (fldnm arnm)
    `(straise
      (not.neth ()
         (lift-initfield ,fldnm
           ((public) (static))
           ,@(foreach-mappend (v vlst)
                              (int->ibytes v)))
         (lift-field (field (array int) ,arnm (public) (static)))
         (if (isnull (this # ,arnm))
             (begin
               (this # ,arnm <- (mkarr int ,(length vlst)))
               (System.Runtime.CompilerServices.RuntimeHelpers
                 @ InitializeArray
                   ((System.Array)(this # ,arnm))
                   ((System.RuntimeFieldHandle)(fieldtoken (this # ,fldnm)))
                   )))
         (leave ((object) (this # ,arnm)))))))

