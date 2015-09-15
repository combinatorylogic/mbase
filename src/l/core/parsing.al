;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2015, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(Section "Simple parsing combinators")

;; Combinators:
; p<+>, pm<+> - sequence
; p<|>, pm<|> - selection
; p<!> - negation
; p<*> - none-or-many
; p<+*> - one-or-many
; p<?> - none-or-one
; p<0> - suppress the result
; p<R> - process the result

;; Aux functions and macros:
; :: - list->string, then wrap
; wrap - make a list of one element
; p.t - get the token value
; p[T] - make a token maker
; R> - format processor, wraps the result
; S->N - atoi function

;; <r> macro language:
; p1 p2 ... - sequence
; p1 | p2 ... - selection
; 'v - equality
; `v - unquoted equality
; (! ...) - negation
; (/ <tkn>) - token parser
; (p -> code) - result processing
; (_ p1 ...) - result suppression
; (= code...) - fallback
; (p *) - none-or-many
; (p +*) - one-or-many
; (% string) - one-of-string-chars
; string - parse the string
; char - parse the char
; ? - parse any
; <> - parse eof

;; Predefined parsers:
; p.alpha
; p.digit
; p.integer
; p.ident
; ...

;;; Parser is a function of 'a list -> (RES . REST),
;;; RES is (RESULT . data) or (FAIL . reason)
;;; REST is: nil or token list or CLOSURE (() -> REST)
(define ---p-fail--- '<***<FAIL>***>)
(macro p-fail? (r)
   "[#t] if parsing result [r] is a failure."
   `(eqv? ---p-fail--- (car ,r)))

(macro p-success? (r)
   "[#t] if parsing result [r] is a success."
   `(not (p-fail? ,r)))

(macro p-rest0 (r)
   `(cdr ,r))

(macro p-result (r)
   "Returns a contents of the successful parsing result [r]."
   `(car ,r))

(function p-lookahead-a-bit (cc)
   (let ((cd (cdr cc)))
      (if (list? cd) nil
         (if (null? cd) nil
           (set-cdr! cc (cd))))))

(function p-rest (r)
   "Returns a remaining stream for the parsing result [r] (either successful or unsuccessful)."
   (p-lookahead-a-bit r) ;; unroll the nearest lazy tail
   (cdr r))

(macro p-rest (r)
  "Returns a remaining stream for the parsing result [r], macro version."
  (with-syms (rr)
    `(let ((,rr ,r))
        (p-lookahead-a-bit ,rr) (cdr ,rr))))

(macro p-mkresult (d rest)
  "Makes a successful parsing result with given result value [d] and remaining stream [rest]."
   `(cons ,d ,rest))

(macro p-mkfail (res rest) ;; ignore the failure reason for now
   "Makes a parsing failure result with a given remaining stream [rest]."
   `(cons ---p-fail--- ,rest))

;; Generic characters
(include "./parsing_chars.al")

;; Combinators
(include "./parsing_0.al")

;; Auxillary stuff
(include "./parsing_1.al")

;; "regular expressions" language
(macro <r> body
  ("This is an easy to use frontend to recursive descent parsing combinators."
   ""
   "Body format is:"
   "[["
   "<body>:"
   "  <string> - parse a string"
   "  <char>   - parse a char"
   "  ?        - always successful parsing, not moving."
   "  '<>'     - parse EOF."
   "  <ident> - parser/recognizer reference"
   "  '<anything> - equality parser on <anything>"
   "  `<expr> - equality parser on <expr> value."
   "  ! <body>* - not a <body> parser"
   "  / <ident> - token parser"
   "  % <string> - recognises all the characters of a <string>"
   "  _ <body>* - discards <body>* parsing result."
;   "  < <body>* - dunno, something funny"
   "  (?? <body>) - <body> or nothing"
;   "  ?| <body>* - dunno, something funny"
   "  = <expr>* - fall back to literal substitution"
   "  T <body>* - if the <body>* parser is successful,"
   "              return the empty successful result at"
   "              the current input stream position."
   ""
   "  <body> + <body>"
   "  <body> <body>   - sequential parsing"
   "  <body>|<body>   - variant parsing, leftmost option first"
   "  <body> -> <expr> - applies <expr> function to <body> parsing result"
   "  <body> :-> <expr> - applies <expr> function to <body> parsing result,"
   "                      wraps the application result into a list."
   "  <body> *   - none-or-many occurences of <body>"
   "  <body> +*  - one-or-many occurences of <body>"
   "]]"
  )
  (cond
    ((list? body)
     (fccase body
       ((quote) (v) `(p>eq (quote ,v)))
       ((quasiquote) (v) `(p>eq ,v))
       ((!) ps `(p<!> (<r> ,@ps)))
       ((/) (t) `(p>token (quote ,t)))
       ((%) (str) `(pm>chars ,str))
       ((_) rest `(p<0> ,(cons '<r> rest)))
       ((<) rest `(p<drop> ,(cons '<r> rest)))
       ((??) rest `(p<?> ,(cons '<r> rest)))
       ((?|) (p def) `(p<?|> ,(cons '<r> p) ,def))
       ((=) rest rest)
       ((T) rest `(p>touch ,(cons '<r> rest)))
      ;; more predicates to go
       (else ;; any other list
         (cond
          ((null? (cdr body)) (cons '<r> (car body)))
          (else
           (let ((sknd (cadr body)))
             (case sknd
               ((+) `(<r> ,(car body) ,@(cddr body)))
               ((|) `(p<|> ,(cons '<r> (car body)) (<r> ,@(cddr body))))
               ((->) `(p<R> ,(cons '<r> (car body)) ,(caddr body)))
               ((X->) `(p<xR> ,(cons '<r> (car body)) ,(caddr body)))
               ((T>) `(p<xR> ,(cons '<r> (car body))
                             (p[xTap] (quote ,(caddr body)) ,(cadddr body))))
               ((D->) `(p<dR> ,(cons '<r> (car body)) ,(caddr body)))
               ((:->) `(p<R> ,(cons '<r> (car body))
                  (lambda (X) (wrap (,(caddr body) X)))))
               ((*) `(p<*> ,(cons '<r> (car body))))
               ((+*) `(p<+*> ,(cons '<r> (car body))))
              ;; more binary ops and postfix ops to go
               (else `(p<+> ,(cons '<r> (car body)) (<r> ,@(cdr body)))))))))))
     ((string? body)
      `(pm>string ,body))
     ((char? body)
      `(p>chareq ,body))
     ((eqv? '? body) 'p.any)
     ((eqv? '<> body) 'p.eof)
     (else body)))


;; the rest
(include "./parsing_2.al")
