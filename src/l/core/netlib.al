;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; More .net-specific functions.

(Section "\\NET{}-specific functionality")

(Par
 "In this section some essential \\NET{} CLI connectivity features are defined."
 )

(topblock

(macro asetx (a i b)
   "A fast array setter, in CLI--mode it is replaced with inline CLI code."
   `(aset ,a ,i ,b))

(function write (o)
  "Calls [System.Console.Write] for a given object."
  ((r_tsbind "System.Console" "Write" object) o))

(include "netlib_fields.al")

(macro current-class-search-path _
  '(quote ("System" "Meta.Scripting")))

(macro net.types args
   "Defines [t_<Typename>] variables for all the given [<Typename>]'s, with no validity checking."
   (let ((cr (read-int-eval '(r_current_using))))
    `(top-begin
       ,@(map
             (lambda (a)
               `(define ,(string->symbol (string-append "t_" (symbol->string a)))
                  (usdotnet ,cr ,(symbol->string a))))
           args)
       (force-class-flush)
       )))

(define _enum_to_object (r_sbind "System.Enum" "ToObject" t_type t_object))

(function to_enum_object (tp obj)
  "Converts a enum of a given type into an object ([System.Enum.ToObject] method wrapper)."
  (_enum_to_object tp obj))

(function enum-or (a b)
  "Calculates [a] OR [b], where [a] and [b] are enums."
  (to_enum_object (r_GetType b) (bitor a b)))

(function a->l (ar)
  "Converts an array into a list."
   (let ((l (alength ar)))
     (let loop ((i 0))
        (if (< i l) (cons (aget ar i) (loop (+ i 1)))))))

(function amap (f a)
  "Maps a given [Object] array into an array of the same size via a given function."
  (let* ((l (alength a))
         (a2 (anew t_object l)))
     (let loop ((i 0))
        (if (< i l) (begin (aset a2 i (f (aget a i))) (loop (+ i 1)))))))

(define t_Pair (r_typebyname "Meta.Scripting.Pair"))
(define _car_fld (r_getField t_Pair "car"))
(define _cdr_fld (r_getField t_Pair "cdr"))
)
(cli-only
 (function set-car! (p v)
   "Destructively sets a pair's head value."
   (ctime
    `(n.asm (p v)
            (expr p)
            (Castclass ,t_Pair)
            (expr v)
            (Stfld ,_car_fld)
            (Ldnull))))
 (function set-cdr! (p v)
   "Destructively sets a pair's tail value."
   (ctime
    `(n.asm (p v)
            (expr p)
            (Castclass ,t_Pair)
            (expr v)
            (Stfld ,_cdr_fld)
            (Ldnull))))
)

(topblock
(function append (a b)
   (if (null? a) b
      (let ((rslt (cons 1 nil)))
         (let loop ((r rslt) (aa a))
            (if (null? aa)
                (set-cdr! r b)
                (let ((nw (cons (car aa) nil)))
                   (set-cdr! r nw)
                   (loop nw (cdr aa)))))
         (cdr rslt))))

(expand-if (shashget (getfuncenv) 'compiled-environment)

(function map (f l) ;; constant stack space map function
  (if (null? l) l
    (let ((p0 (cons nil nil)))
      (let loop ((p p0)
                 (ll l))
         (if (null? ll) nil
            (begin
              (set-car! p (f (car ll)))
              (if (null? (cdr ll)) nil
                (let ((np (cons nil nil)))
                  (set-cdr! p np)
                  (loop np (cdr ll)))))))
       p0))))


(define a-lbound (r_tbind "System.Array" "GetLowerBound" int))
(define a-ubound (r_tbind "System.Array" "GetUpperBound" int))
(define a-get (r_tbind "System.Array" "GetValue" t_int))
(function ar->l (ar)
  "Converts an instance of [System.Array] into a list."
  (let ((lb (a-lbound ar 0))
        (ub (a-ubound ar 0)))
    (let loop ((i lb))
       (if (> i ub) nil
           (cons (a-get ar i) (loop (+ i 1)))
           ))))


(define r_getmethod1
   (let ((gtmtd (r_tbind t_type "GetMethod" string)))
      (fun (tp nm)
         (gtmtd tp nm))))

(define EnumHash (mkhash))


(define e_name (r_tsbind "System.Enum" "GetName" t_type t_object))
(define _enum_vals (r_tsbind "System.Enum" "GetValues" t_type))
(define _enum_nams (r_tsbind "System.Enum" "GetNames" t_type))

(function getEnum (tp nm)
  "Returns a enum of a given type."
  (let ((a (hashget EnumHash tp)))
     (if (null? a)
         (let ((bs (ar->l (_enum_vals tp)))
               (ns (ar->l (_enum_nams tp)))
               (hsh (mkhash)))
              (iter (lambda (ab)
                      (hashput hsh (car ab) (cdr ab)))

                    (czip ns bs))
              (hashput EnumHash tp hsh)
              (hashget hsh nm))
       ;; else
       (hashget a nm))))

(macro boolwrap1 (f)
  `(fun (xxxxx) (if (,f xxxxx) #t nil)))

(define _ie_getenum (r_tbind "System.Collections.IEnumerable" "GetEnumerator"))
(define _ie_cur (r_tbind "System.Collections.IEnumerator" "get_Current"))
(define _ie_mnx (r_tbind "System.Collections.IEnumerator" "MoveNext"))

(function net.map (fn en)
  (let ((enm (_ie_getenum en)))
    (let loop ()
       (if (_ie_mnx enm) (cons (fn (_ie_cur enm)) (loop))
                         nil))))

(function net.iter (fn en)
  (let ((enm (_ie_getenum en)))
    (let loop ()
       (if (_ie_mnx enm) (begin (fn (_ie_cur enm)) (loop))
                         nil))))


(define _gtkeys (r_tbind "System.Collections.Hashtable" "get_Keys"))

(function hashmap (fn ht)
 ("Applies the function [fn]([key] [value]) to all the key bindings in a given hashtable,"
  "returning a list of application results.")
  (let ((k (_gtkeys ht)))
     (net.map (lambda (x) (fn x (ohashget ht x))) k)))

(function hashiter (fn ht)
 ("Applies the function [fn]([key] [value]) to all the key bindings in a given hashtable.")
  (let ((k (_gtkeys ht)))
     (net.iter (lambda (x) (fn x (ohashget ht x))) k)))

(define hash-key?
  (let ((tt (r_tbind "System.Collections.Hashtable" "ContainsKey" object)))
    (fun (ht key) (tt ht (to-string key)))))

(function _dbg (str val)
  (println (buildstring str ": " (to-string val)))
  val)

)
(include "netlib_types.al")

(include "netlib_io.al")

;; an ad hoc "modules" support, in C preprocessor style.
(define _once_ht_ (mkhash))
(macro once (name . rest)
  (if (hashget _once_ht_ name) `(begin)
      (begin
        (hashput _once_ht_ name name)
        `(top-begin (hashput _once_ht_ ,name ,name) ,@rest))))

(function sleep (msec)
  "Waits for [msec] milliseconds."
  ((r_tsbind "System.Threading.Thread" "Sleep" t_int) msec))

(net.types MBaseException Exception)
(function mbaseerror (ex)
  "Gets a value bound to [MBaseException] instance [ex]."
  ((r_tbind t_MBaseException "val") ex))

; Character and string handling: core runtime does not provide us with this features.

(define t_char (r_typebyname "System.Char"))
(define t_char[] (r_typebyname "System.Char[]"))

;(define string->list
;  (let ((s2a (r_tbind t_string "ToCharArray")))
;     (@ ar->l s2a)))

(define symbol->list (@ string->list symbol->string))

;(function list->string (l)
;   (let* ((ll (length l))
;          (ar (anew t_char ll)))
;     (iteri (fun (i c) (aset ar i c)) l)
;     (new t_string (t_char[] ar))))

(define list->symbol (@ string->symbol list->string))

(int-only
 (function apply (fn ars)
    (let* ((ara (mkovector ars)))
       (int:apply fn ara))))

(cli-only
 (function apply (fn ars)
   "Applies [fn] to the list of arguments [ars]."
    (let* ((ara (mkovector ars)))
      (ctime
       `(n.asm (fn ara)
         (expr ara)
         (Castclass ,t_object_array)
         (expr fn)
         ,@(if ##prefix-tail `((Tailcall)) nil)
         (Call ,(car m_Call_Generic))
         )))))

(net.types DateTime)
(cli-only
 (function date-now ()
   "Returns the current date string."
   (ctime
    `(n.asm ()
       (local a ,t_DateTime)
       (Call ,(r_mtd "System.DateTime" "get_Now"))
       (Stloc (var a))
       (Ldloca (var a))
       (Ldstr "ddd, dd MMM yyyy")
       (Call ,(r_mtd "System.DateTime" "ToString" string))
     )))

)

(int-only
  (macro asetx rest `(aset ,@rest)))

;;;; Enums

(function enumOr (tp lst)
  "Apply the [bitwise or] to all the listed enum values of the type [tp]."
   (foldl enum-or 0 (map-over lst
                      (fun (s)
                          (getEnum tp (any->string s))))))


;;; Strings
(define stringCmp (r_tbind t_string "CompareTo" t_string))
(function string<? (s1 s2)
  "[#t] if [s1 < s2]."
  (< (stringCmp s1 s2) 0))

(function string>? (s1 s2)
  "[#t] if [s1 > s2]."
  (> (stringCmp s1 s2) 0))

(function string=? (s1 s2)
  "[#t] if [s1] equals to [s2]."
  (= (stringCmp s1 s2) 0))

(macro unquote (x)
  (writeline `(UNQUOTE:ERROR: ,x))
  (ccerror `(UNQUOTE:ERROR: ,x))
  )

(macro unquote-splicing (x)
  (writeline `(UNQUOTE-SP:ERROR: ,x))
  (ccerror `(UNQUOTE-SP:ERROR: ,x))
  )


(define t_void (dotnet "Void"))
(define RetType (r_tbind "System.Reflection.MethodInfo" "get_ReturnType"))
(define t_eq  (r_tbind "System.Type" "Equals" "System.Type"))
(define t_assignable? (r_tbind "System.Type" "IsAssignableFrom" "System.Type"))
(function void-p (m)
     (let ((v (RetType m)))
          (t_eq v t_void)))

(net.types Boolean Byte SByte Int16 UInt16 Int32 UInt32 Int64 UInt64
           Char Double Single Enum IntPtr)
