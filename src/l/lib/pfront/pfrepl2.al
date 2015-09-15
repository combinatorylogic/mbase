;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(n.module pfrepl2 exe)
(sysdll MBaseFront)

(function pf-very-lazy-reader (redr)
  (__peg:lst2stream
   (let loop ()
    (alet chr (not.neth ((System.IO.StreamReader redr))
                        (chr = (redr@Read))
                        (object ret = null)
                        (if (>= chr 0) (ret <- ((object)((char)chr))))
                        (leave ret))
          (writeline `(C: ,chr))
          (if (not chr) nil
              (cons chr loop)))) nil))

(function pf-read-eval-print (lst)
  (alet res (read-compile-eval (hlevel-compile lst))
    (println (S<< ":>> " (to-string res)))
    (print ">>")
    ))

(function peg-function-PFrepltoplev (s)
  (pf-read-eval-print s))

(pfront-expand-string
 "parser pftehrepl (pfront) {
    pftehrepl := { [pfeexpr]:e [Spaces]* [pftehrepl]:rst => $nil() }
             /   { ![otopexpr] [Spaces]* => $nil() };
    pfeexpr := { [otopexpr]:e => $PFrepltoplev(e) };
    otopexpr := [topexpr]:e ([CR]/\";\") => e;
  } ")

(force-class-flush)

(function main ()
  (shashput (getfuncenv) 'main nil)
  (corelib:set-lookup-path (not.neth () (leave
                                         (System.IO.Directory@GetCurrentDirectory))))
  (read-int-eval `(n.module DefaultPF))
  (print ">>")

  (peg:easyparse peg_pftehrepl
   (pf-very-lazy-reader
     (not.neth ()
       (leave ((object)(new System.IO.StreamReader
                            (System.Console@OpenStandardInput))))))))



