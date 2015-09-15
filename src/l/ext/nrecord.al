;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(function __getnm (f)
  (p:match f (($tp $nm) nm) (else f)))

(function __gettp (f)
  (p:match f (($tp $nm) tp) (else 'object)))

(function nrec.makeacc (REC nm fld)
  (with-syms (nREC)
   `(let ((,nREC ,REC))
      (not.neth ((,nm ,nREC))
         (leave ((object)(,nREC # ,fld)))))))

(macro nrec:def (nm . fields)
  (
   "Defines a record type [nm] with a list of [fields]."
   "To create a new record instance, use  the constructor function"
   "([nm].new [<initial-value>]*)), to get field value, use ([nm].[field] [<instance>]),"
   "Or, alternatively: ([nm].make [:<fieldname> <initial-value>] ...)"
   "to set field value, use ([nm].[field]! [<instance>] [<value>])."
   )
  (with-syms (val)
  `(top-begin
     (not.class ,nm (extends System.Object)
       ,@(foreach-map (f fields)
           (p:match f
             (($tp $nm) `(field ,tp ,nm (public)))
             ($$M `(field object ,f (public)))
             (else (ccerror `(nrec:def-field ,f))))))

     (not.function ,(string->symbol (S<< nm ".new"))
       ,(foreach-map (f fields)
          (if (list? f) f `(object ,f)))
       (,val = (new ,nm))
       ,@(foreach-map (f fields)
           (alet n (__getnm f)
              `((,val # ,n) <- ,n)))
       (return ((object) ,val)))

     (macro ,(string->symbol (S<< nm ".make")) macroarg
           (list 'extra:with-optional-args macroarg
                 (quote ,(map __getnm fields))
                 (quote (,(Sm<< nm ".new") ,@(map __getnm fields)))))

     ,@(foreach-map (f fields)
         `(begin
            (function ,(string->symbol (S<< nm "." (__getnm f)))
              (REC)
              (not.neth ((,nm REC))
                        (leave ((object)(REC # ,(__getnm f))))))
            (macro ,(string->symbol (S<< nm "." (__getnm f) ".M"))
              (REC)
              (nrec.makeacc REC (quote ,nm) (quote ,(__getnm f))))))

     (function ,(Sm<< nm ".copy") (rc)
         (,(Sm<< nm ".new") ,@(foreach-map (f fields)
                                `(,(Sm<< nm "." (__getnm f)) rc))))

     ,@(foreach-map (f fields)
         `(function ,(string->symbol (S<< nm "." (__getnm f) "!"))
            (REC VAL)
            (not.neth ((,nm REC) (,(__gettp f) VAL))
                      (REC # ,(__getnm f) <- VAL)
                      (leave null))))
     ))
  )