;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TeX pretty printer to be used with the vsparser thing


(define wwTabsX
  (strreplacers*
   (">" "$>$")
   ("<" "$<$")
   ("{" "{$\\{$}")
   ("}" "{$\\}$}")
   ("(" "\\lsplp{}")
   (")" "\\lsprp{}")
   ("&" "\\&")
   ("%" "\\%")
   ("_" "\\_")
   ("^" "{$\\hat{~}$}")
   ("$" "\\$")
   ("#" "\\#")
   (" " "\\lspspc{}")
   ("\n" "~\\\\\n")
   ("\\" "{$\\backslash$}")
   ("\t" "\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}\\lspspc{}")))


(function wwTabs (str)
  (strapply* wwTabsX str))


(function print-end ()
  (print "}")
  )

(function print-begin (prules c)
  (print (S<< "{" (prules c))))

(function print-endline ()
  (println "{}~\\\\"))

(function nstring=? (a b)
  (let ((na (null? a))
        (nb (null? b)))
  (cond
   ((and na nb) #t)
   ((and (not na) (not nb))
    (string=? a b))
   (else nil))))

(function state-comp (s1 s2)
  (cond
   ((null? s1) nil)
   ((null? s2) nil)
   (else
    (format s1 (_ _ fg1 bg1)
    (format s2 (_ _ fg2 bg2)
       (and (nstring=? fg1 fg2)
            (nstring=? bg1 bg2)))))))

(function pprint-buffer (lexer parser rules prules lines)
  (let* ((v (mkovector lines))
         (bpars (lex-and-parse lexer parser
                              (lazy_strs_reader
                               v)))
         (nl (alength v))
         (cac (rules bpars)))
    (let gloop ((l 0) (stt nil))
      (if (>= l nl) nil
         (let* ((ln ([ l ] v))
                (stn
                 (let loop ((i 0) (s (string->list ln)) (st stt))
                   (alet c (bnf-chkcache cac i l)
                    (if (null? s) (begin
                                   (if (not (null? st))
                                       (print-end))
                                   (print-endline)
                                   c)
                     (begin
                      (cond
                       ((and c
                             (not (state-comp st c)))
                          (begin
                            (if st (print-end))
                            (print-begin prules c)))
                       ((and st (not c))
                        (print-end)))
                      (print (wwTabs (S<< (car s))))
                      (loop (+ i 1) (cdr s) c)))))))

           (gloop (+ l 1) stn))))))





