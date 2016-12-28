;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(Section "Mutable records")

(macro rec:def (nm . fields)
  (
   "Defines a record type [nm] with a list of [fields]."
   "To create a new record instance, use  the constructor function"
   "([nm].new [<initial-value>]*)), to get field value, use ([nm].[field] [<instance>]),"
   "Or, alternatively: ([nm].make [:<fieldname> <initial-value>] ...)"
   "to set field value, use ([nm].[field]! [<instance>] [<value>])."
   )
   (let* ((ar (mapi (fun (i x) (list x i)) fields))
          (l  (length ar)))
     `(top-begin
        (function ,(string->symbol (S<< nm ".new"))
                  ,fields
            (mkovector (list ,@fields)))
        (macro ,(string->symbol (S<< nm ".make")) macroarg
           (list 'extra:with-optional-args macroarg
                 (quote ,fields)
                 (quote (mkovector (list ,@fields)))))
       ,@(foreach-map (f fields)
           `(function ,(string->symbol (S<< nm "." f))
                      (*REC*)
               (aget *REC* ,(lookup-env-car ar f))))
       (function ,(Sm<< nm ".copy") (rc)
                 (,(Sm<< nm ".new") ,@(foreach-map (f fields)
                                         `(,(Sm<< nm "." f) rc))))
       ,@(foreach-map (f fields)
           `(function ,(string->symbol (S<< nm "." f "!"))
                      (*REC* *VAL*)
               (asetx *REC* ,(lookup-env-car ar f) *VAL*))))))


;;;;;;

(function mkcollector ()
   (let* ((col (noconst (cons nil nil)))
          (sli (noconst (cons nil col)))
          (get (fun () (cdr col)))
          (add (fun (x)
                 (set-cdr! (cdr sli) (cons x nil))
                 (set-cdr! sli (cdr (cdr sli)))
                 )))
      (list add get)))

(macro collector (nms . body)
 ("Initialize a collector context, with given adder and getter names."
  "Usage: [(collector (<adder> <getter>) <body-expression>*)]"
  "Inside the body expressions you can use [(<adder> somevalue)] function to"
  "collect values in order, and then [(<getter>)] to return the collected list of values."
  ""
  "This macro is particularry useful with AST visitors."
  )

  `(format (mkcollector) (,(car nms) ,(cadr nms)) ,@body))


(macro collectors (defs . body)
  (if (null? defs) `(begin ,@body)
     `(collector ,(car defs) (collectors ,(cdr defs) ,@body))))

(macro mkref rest
  `(noconst (cons ,(if (null? rest) 'nil (car rest)) nil)))

(macro noconst (x) x)

(macro r! (tgt src)
  `(set-car! ,tgt ,src))

(macro deref (src)
  `(car ,src))


(macro with-sequence ( nam . body)
  "Creates a gensym sequence within the [body] context."
  (with-syms (stor)
  `(let* ((,stor (mkref 0))
          (,(car nam) (fun ()
                        (let* ((v (deref ,stor))
                               (nv (+ v 1)))
                          (r! ,stor nv)
                          (string->symbol
                           (S<< ,(S<< "sequence_" (car nam) "_")
                                v "_"))))))
     ,@body)))

; Unit tests for the functionality above
;
(unit-test-defn 1 (rec:def recabc a b c))

(unit-test 1 (alet x (recabc.new 1 2 3) (recabc.b! x 9) (recabc.b x)) 9)

(unit-test 1 (let ((r (mkref))) (r! r "xxx") (deref r)) "xxx")

(unit-test 1 (with-sequence (s) (list (s) (s) (s)))
           (sequence_s_0_
            sequence_s_1_
            sequence_s_2_))

(unit-test 1 (collector (a g) (a 1) (a 2) (a 3) (g)) (1 2 3))


