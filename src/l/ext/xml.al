;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(Section "XML and SXML support")

(Par
 "SXML format is used for internal representation of XML trees."
 "Detailed specification can be found at {\\tt http://ssax.sf.net/}."
 )

(add-assembly 'System.Xml)

(using ("System.Xml")

  (net.types XmlTextReader XmlNodeType)
  (define _ 0)

  (function new_Reader (nm)
    (new t_XmlTextReader (t_string nm)))

  (define xmlRead (r_tbind t_XmlTextReader "Read"))
  (define moveToAttr (r_tbind t_XmlTextReader "MoveToNextAttribute"))


  (define _xmlNodeType (r_tbind t_XmlTextReader "get_NodeType"))
  (function xmlNodeType (rdr)
    (string->symbol (e_name t_XmlNodeType (_xmlNodeType rdr))))

  (function sxml-stacker ()
    (let* ((top (cons '*TOP* nil))
           (stk (cons nil (list top)))
           (poplevel
             (fun ()
               (set-cdr! stk (cddr stk))) )
           (addsimple
             (fun (x)
               (let ((h (cadr stk))
                     (nw (list x)))
                 (set-cdr! h nw)
                 (set-cdr! stk (cons nw (cddr stk))))))
           (nodetail
             (fun (nd)
               (if (null? (cdr nd)) nd (cdr nd))))
           (addnode
             (fun (nm atr)
               (let ((h (cadr stk))
                     (nw (list (cons (string->symbol nm)
                                  (if (null? atr) nil
                                     `((@ ,@atr)))))))
                 (set-cdr! h nw)
                 (set-cdr! stk (cons nw (cddr stk)))
                 (set-cdr! stk (cons (nodetail (car nw)) (cdr stk)))
               ))))
       (list top poplevel addsimple nodetail addnode)))

  (function xmlReadG (rdr)
    (try
     (xmlRead rdr)
     t_Exception
     (fun (e) nil)))

  (function xml-read-stream (rdr)
     (format (sxml-stacker) (top poplevel addsimple nodetail addnode)
      (let loop ()
        (if (not (xmlReadG rdr)) nil
         (begin
          (case (xmlNodeType rdr)
           ((Element)
            (let ((ee (g-> rdr "IsEmptyElement")))
              (addnode
                (g-> rdr "Name")
                (if (g-> rdr "HasAttributes")
                 (let attr-loop ()
                   (if (moveToAttr rdr)
                    (cons
                      (list (string->symbol (g-> rdr "Name")) (g-> rdr "Value"))
                      (attr-loop))))))
              (if ee (poplevel))))
           ((Text)
            (addsimple (g-> rdr "Value")))
           ((CDATA)
            (addsimple (g-> rdr "Value")))
           ((ProcessingInstruction)
            (addsimple `(*PI*
                              ,(string->symbol (g-> rdr "Name")) ,(g-> rdr "Value"))))
           ((Comment)
            nil)
           ((XmlDeclaration)
            nil)
           ((DocumentType)
            nil)
           ((EntityReference)
            (addsimple (string->symbol (g-> rdr "Name"))))
           ((EndElement)
            (poplevel)))
          (loop))))
      top
   ))

(function xml-read (nm)
    ("Reads an XML stream from a file into an SXML tree.")
    (let ((rdr (new_Reader nm)))
      (xml-read-stream rdr)))

(define _rgxp_q (<r> "\""))
(function xmlquotestring (str)
   (strreplace* _rgxp_q "&quot;" str))

(function sindent (i)
   (list->string (formap (t 0 i) #\Space)))

(function pindent (f i)
   (fprint f (sindent i)))

(function pxmlargs-old (i a)
  (foldl string-append ""
         (map-over (cdr a)
                   (fmt (nm vl)
                      (buildstring "\n" (sindent (+ 1 i)) nm "=\"" (xmlquotestring vl) "\"")))))



(function pxmlargs (i a)
  (foldl string-append ""
         (map-over (cdr a)
                   (fmt (nm vl)
                      (buildstring " " nm "=\"" (xmlquotestring vl) "\"")))))




(function printxml (file sx)
  (let loop ((stak `((0 ,sx))))
    (if (null? stak) nil
      (format stak ((i l) . stk)
       (let
        ((newstk
          (cond
            ((string? l)
             (pindent file i)
             (fprintln file l) stk)
            ((symbol? l)
             (pindent file i)
             (fprintln file (buildstring "&" l ";")) stk)
            ((eqv? '//R (car l))
             (let ((res (cdr l)))
               (if (null? (cdr res))
                   stk
                   (begin
                     (p-lookahead-a-bit res)
                     (p-lookahead-a-bit (cdr res))
                     (cons (list i (cadr res)) (cons (list i (cons '//R (cdr res))) stk))))))
            ((eqv? '/ (car l))
             (pindent file i)
             (fprintln file (buildstring "</" (cdr l) ">")) stk)
            ((null? (cdr l))
             (pindent file i)
             (fprintln file (buildstring "<" (car l) "/>")) stk)
            (else ;; have members
              (pindent file i)
              (p-lookahead-a-bit l)
              (letf (((as . rs) (if (and (list? (cdr l)) (list? (cadr l)) (eqv? (car (cadr l)) '@) )
                                    (begin (p-lookahead-a-bit (cdr l)) (cons (cadr l) (cdr l)) )
                                    (cons nil l))))
                (if (not (null? rs)) (p-lookahead-a-bit rs))
                (fprint file (buildstring "<" (car l) (if as (pxmlargs i as) "") ))
                (if (null? (cdr rs) )
                  (begin (fprintln file "/>") stk)
                  (if (and (begin (p-lookahead-a-bit (cdr rs)) #t)
                           (null? (cddr rs))
                           (string? (cadr rs)))
                      (begin
                        (fprint file ">")
                        (fprint file (cadr rs))
                        (fprintln file (buildstring "</" (car l) ">"))
                        stk
                        )
                      (begin
                        (fprintln file ">")
                        (cons
                         (list (+ i 1) (cons '//R rs))
                         (cons (list i (cons '/ (car l))) stk))))))))))
        (loop newstk)
        )))))

(function dumpxml (filnm xml enc)
   ("Dumps a given SXML tree into an XML file.")
    (call-with-output-file filnm
         (fun (fil)
            (fprintln fil (S<< "<?xml version=\"1.0\" encoding=\"" (if enc enc "utf-8") "\"?>"))
            (printxml fil xml))))


(function sxml-attr (nm nd)
  (if (null? (cdr nd)) nil
    (let ((x (cadr nd)))
      (if (and (list? x) (eqv? '@ (car x)))
          (let ((y (find (fun (i) (eqv? (car i) nm)) (cdr x))))
           (if (null? y) nil
               (cadr y)))))))

(function mapahead (f l)
  (let ((top (cons nil nil)))
   (let loop ((x l) (c top))
      (if (null? x) nil
        (begin
         (let ((nw (list (f (car x)))))
           (p-lookahead-a-bit x)
           (set-cdr! c nw)
           (loop (cdr x) nw)))))
   (cdr top)))

(recfunction sxml-revisit (vfun nde)
  (if (list? nde)
     (begin
      (p-lookahead-a-bit nde)
      (format nde (ndnm . rest)
        (p-lookahead-a-bit rest)
        (if (null? rest) nde
            (letf (( (a . r) (if (and (list? (car rest)) (eqv? '@ (caar rest)))
                                 (cons (car rest) (cdr rest))
                                 (cons nil rest)) ))
              (if (null? r) nde
                  (let ((nxt
                          (mapahead
                            (fun (x)
                               (sxml-revisit vfun (if (list? x) (vfun x) x)))
                            r)))
                     (cons ndnm (if (null? a) nxt (cons a nxt)))))))))
      nde))

(macro sxml-path pth
  ("Creates a path extraction function."
   "The path element format is following:"
   "[["
   "<path>:"
   "  (<node>) - all matches of the node on this level"
   "  <node> - first match of the node on this level"
   "  * <path> - first match of the path somewhere deep."
   " (*) <path> - all matches of the path in the depth."
   " <path> <path> - match the first path, lookup the second"
   "               from that level."
   "]]"
   )
  (let* ((p1
          (let loop ((p pth))
           (cond
            ((null? p) nil)
            ((eqv? (car p) '*)
             (cons `(sxmlp:find-first (quote ,(cadr p))) (loop (cddr p))))
            ((and (list? (car p))
                  (eqv? '* (caar p)))
             (list `(sxmlp:all-down (sxml-path ,@(cdr p)))))
            ((list? (car p))
             (if (null? (cdr p))
               (list `(sxmlp:all-this-level   (quote ,(caar p))))
               (list `(sxmlp:all-this-level-c (quote ,(caar p)) (sxml-path ,@(cdr p))))))
            ((symbol? (car p))
             (cons `(sxmlp:first-this-level (quote ,(car p))) (loop (cdr p))))
            (else (ccerror `(sxml-path ,p))))))
           (r (gensym)))
   `(fun (,r)
       ,(foldl (fun (acc p)
                  `(,p ,acc)) `(list ,r) p1))))

(macro sxml-paths pth
  ("Creates an sxml-path function that is applied to a list of SXML trees"
   "Useful for stacking sxml-path chunks together."
   "Always returns a list of values."
   )
  `(fun (ts)
     (foreach-mappend (t ts)
        ((sxml-path ,@pth) t))))

(recfunction sxmlp:deepfetch (tree v)
  (cond
   ((null? tree) nil)
   ((and (list? tree) (symbol? (car tree)))
    (if (eqv? (car tree) '@) nil
     (if (eqv? (car tree) v) tree
      (sxmlp:deepfetch (cdr tree) v))))
   ((list? tree)
     (let ((r (sxmlp:deepfetch (car tree) v)))
       (if r r (sxmlp:deepfetch (cdr tree) v))))
   (else nil)))

(recfunction sxmlp:deepall (fn tree)
 (cond
  ((null? tree) nil)
  ((and (list? tree) (symbol? (car tree)))
   (if (eqv? '@ (car tree)) nil
    (foldl append (fn tree) (foreach-map (xx (cdr tree)) (sxmlp:deepall fn xx)))))
  (else nil)))

(recfunction sxmlp:fetchall (v tree)
 (filter (fun (a) a)
   (foreach-map (t tree)
     (cond
       ((and (list? t) (symbol? (car t)) (not (eqv? '@ (car t))))
        (if (eqv? v (car t)) t nil))
       (else nil)))))

(recfunction sxmlp:fetch (tree v)
  (cond
   ((null? tree) nil)
   ((and (list? tree) (list? (car tree)))
    (if (eqv? (caar tree) '@) nil
     (if (eqv? (caar tree) v) (car tree)
      (sxmlp:fetch (cdr tree) v))))
   ((list? tree) (sxmlp:fetch (cdr tree) v))
   (else nil)))


(function sxmlp:find-first (v)
  (fun (x)
    (filter (fun (a) a)
      (foreach-map (xx x)
        (sxmlp:deepfetch xx v)))))

(function sxmlp:first-this-level (v)
  (fun (x)
    (filter (fun (a) a)
      (foreach-map (xx x)
        (sxmlp:fetch xx v)))))

(function sxmlp:all-down (fn)
  (fun (x)
    (foldl append nil (foreach-map (xx x) (sxmlp:deepall fn xx)))))

(function sxmlp:all-this-level-c (v rz)
  (fun (x)
    (foldl append nil (foldl append nil (foreach-map (xx x) (map rz (sxmlp:fetchall v xx)))))))

(function sxmlp:all-this-level (v)
  (fun (x)
    (foldl append nil (foreach-map (xx x) (sxmlp:fetchall v xx)))))


)

