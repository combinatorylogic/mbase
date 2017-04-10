;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   OpenMBase
;;
;; Copyright 2005-2017, Meta Alternative Ltd. All rights reserved.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-
;- \subsection{PEG syntax}
;-
;- Here we'll reuse for the last time
;- our old parsing infrastructure to implement the new PEG
;- syntax.
;-

(define px.whitespace (<r> ((#\Space | #\Tab | #\Newline))))

(define p.comment (<r> ("/*" ((! "*/") *) "*/") | ("//" ((! #\Newline) *))))

(make-simple-lexer peg-lexer
  (ident-or-keyword
   (p.alpha ((p.alpha | "_" | p.digit) *))
   ident)
  (keywords rule token term ignoring define parser extends dynhook check) ; ...
  (simple-tokens
   ".peg:" PFXTOKEN
   "<=" FROM
   "=>" TO "-" MINUS ":=" EQUALS "=" EQ "//" DSLASH "/" SLASH "&" AND
   "+" PLUS "?" QUEST "!" NOT
   "*" STAR ";" SCOL
   "(" LB ")" RB ":" COLON ".." DDOT "." DOT
   "{" LQB "}" RQB "<-" LARROW
   "," COMMA "$" MONEY "@" AT "`" BQUOTE
   "<" LAB ">" RAB
   "|" SB "|" SB

   "_" NOT "#" MONEY ;; TO AVOID SCREENING PROBLEMS IN SHELL
   )
  (regexp-tokens
   (((_ "[[") (p.alpha ((p.alpha | "_" | "-" | p.digit) *)) (_ "]]"))
    -> genlist->symbol) ident
   ((_ "'") p. (_ "'")) CHAR
   (((_ "'") ((((_ #\\) "'") | (! "'")) +*) (_ "'"))
    -> (fun (x)
         (if x
             (genlist->string x)
             ""
             ))) STRING
   (((_ "\"") ((((_ #\\) "\"") | (! "\"")) +*) (_ "\""))
    -> (fun (x)
         (if x
             (genlist->string x)
             ""
             ))) TCOMMENT
   (
    ((_ "[") (_ (px.whitespace *))
     (! px.whitespace) (_ (px.whitespace *))  (_ "-")
     (_ (px.whitespace *))
     (! px.whitespace)
     (_ (px.whitespace *))
     (_ "]"))

    -> (fmt (c1 c2)
            (list (ascii c1) (ascii c2))))
   RANGE
   (((_ "0x") ((p.alpha | p.digit) +*)) ->
    (fun (l) (HX->N (genlist->string l)))) number
   ((p.digit +*) -> (fun (l) (S->N (genlist->string l)))) number
   )
  (ignore p.whitespace p.comment))


(bnf-parser ((exprs parse-peg-top)
             (expr  parse-peg-expr)
             (tdecls parse-peg-decls)
             (clause parse-peg-clause)
             )

  (exprs
   ((expr exprs) (cons $0 $1))
   ((expr) (list $0)))

  (tdecls
   ((decl tdecls) (cons $0 $1))
   ((decl) (list $0)))

  (maybepfx
   ((PFXTOKEN) nil)
   (() nil))

  (decl
   ((maybepfx parser ident:nm extends declargs:a LB exprs:es RB)
    `(packrat-ast ,nm ,a ,@es))
   ((maybepfx parser ident:nm LB exprs:es RB)
    `(packrat-ast ,nm () ,@es))
   )

  (declargs
   ((ident:a COMMA declargs:rs) (cons a rs))
   ((ident:a) (list a)))

  (expr
   ((token ident:n report:r EQUALS peg:p TO annots:a SCOL)
    `(terminal () token ,n ,p (,a ()) ,@r))
   ((token ident:n report:r EQUALS peg:p SCOL)
    `(terminal () token ,n ,p (() ()) ,@r))
   ((term ident:n report:r EQUALS peg:p TO constrtop:ctr SCOL)
    `(terminal () term ,n ,p ,ctr ,@r))
   ((term ident:n report:r EQUALS peg:p SCOL)
    `(terminal () term ,n ,p (() ()) ,@r))
   ((ident:n report:r EQUALS peg:p TO annots:a SCOL)
    `(terminal () normal ,n ,p (,a ()) ,@r))
   ((ident:n report:r EQUALS peg:p SCOL)
    `(terminal () normal ,n ,p (() ()) ,@r))
   ((ignoring ident:igname SCOL)
    `(with-ignore ,igname))
   ((define ident:defname LAB defargs:as RAB report:r EQUALS peg:p TO constrtop:ctr SCOL)
    `(define ,defname ,as ,p ,ctr ,@r)) ;; macro definition
   ((define ident:defname LAB defargs:as RAB report:r EQUALS peg:p SCOL)
    `(define ,defname ,as ,p (() ()) ,@r)) ;; macro definition
   ((rule ident:name EQUALS peg:p TO constrtop:ctr SCOL)
    `(rule ,name ,p ,ctr))
   ((rule ident:name EQUALS peg:p SCOL)
    `(rule ,name ,p ( () () )))
   ((dynhook ident:name SCOL)
    `(dynahook ,name))
   )

  (report
   ((TCOMMENT:s) (list s))
   (() nil))

  (defargs
    ((ident:a COMMA defargs:b) (cons a b))
    ((ident:a) (list a)))

  (constrtop
   ((annots:a constr:b) `(,a ,b))
   ((annots:a) `(,a ()))
   ((constr:b) `(() ,b)))

  (constr
   ((BQUOTE ident:id) `(const ,id))
   ((MONEY ident:id LB fconsargs:as RB) `(fcall ,id ,@as))
   ((MONEY ident:id LB RB) `(fcall ,id))
   ((ident:id LB constrargs:al RB) `(constr ,id ,@al))
   ((ident:id LB RB) `(constr ,id))
   ((ident:id) `(var ,id)))

  (fconsargs
   ((constr:a COMMA fconsargs:b)
    `(,a ,@b))
   ((constr:a) (list a)))

  (constrargs
   ((constrarg:a COMMA constrargs:b) `(,a ,@b))
   ((constrarg:a) `(,a))
   )

  (constrarg
   ((ident:field LARROW constr:a) `(set ,field ,a))
   ((MONEY ident:id LB fconsargs:as RB)
    `(set ,(gensym) (fcall ,id ,@as)))
   ((BQUOTE ident:id) `(set ,(gensym) (const ,id)))
   ((MONEY ident:id LB RB) `(set ,(gensym) (fcall ,id)))
   ((ident:id LB constrargs:al RB) `(set ,(gensym) (constr ,id ,@al)))
   ((ident:id LB RB) `(set ,(gensym) (constr ,id)))
   ((ident:field) `(set ,field (var ,field)))
   ((AT constrarg:a) `(append ,(cadr a) ,(caddr a)))
   )

  (pslashlist
   ((patom:a SLASH pslashlist:ls) (cons a ls))
   ((patom:a) (list a)))

  (pdslashlist
   ((patom:a DSLASH pdslashlist:ls) (cons a ls))
   ((patom:a) (list a)))

  (atomslist
   ((patom:a atomslist:b) (cons a b))
   ((patom:a) (list a)))

  (peg
   ((patom:a SLASH pslashlist:ls)
    `(palt ,a ,@ls))
   ((patom:a DSLASH pdslashlist:ls)
    `(pdalt ,a ,@ls))
   ((patom:a atomslist:b) `(seq ,a ,@b))
   ((patom:a) a))

  (patom
   ((patomx:a COLON ident:n) `(bind ,n ,a))
   ((patomx:a) a))

  (patomx
   ((AND psimple:a) `(andp ,a))
   ((NOT psimple:a) `(notp ,a))
   ((psimple:a PLUS) `(plus ,a))
   ((psimple:a STAR) `(star ,a))
   ((psimple:a QUEST) `(maybe ,a))
   ((psimple:a) a))

  (psimple
   ((LQB peg:a TO constrtop:ctr report:r RQB) `(lift () ,a ,ctr ,@r))
   ((LQB peg:a report:r RQB) `(lift () ,a (() ()) ,@r))
   ((LB peg:a RB) a)
   ((SB ident:nm COLON STRING:s SB) `(rule ,nm ,s))
   ((SB STRING:s SB) `(rule keyword ,s))
   ((check LB ident:nm RB) `(check ,nm))
   ((STRING:s) `(trivial (string ,@(map ascii (string->list s)))))
   ((RANGE:a) `(trivial (range ,@a)))
   ((CHAR:c) `(trivial (char ,(ascii (car c)))))
   ((DDOT) `(trivial (fail)))
   ((DOT) `(trivial (anychar)))
   ((number:n) `(trivial (char ,n)))
   ((ident:t LAB comalist:args RAB) `(macroapp ,t ,@args))
   ((ident:t) `(terminal ,t))
   )

  (clause
   ((peg:a TO constrtop:ctr report:r) `(,a ,ctr ,@r))
   ((peg:a report:r) `(,a (() ()) ,@r))
   )

  (comalist
   ((peg:l COMMA comalist:r) (cons l r))
   ((peg:l) (list l)))

  (annots
   ((annot:a annots:b) (cons a b))
   ((annot:a) (list a)))

  (annot
   ((LQB ident:nm EQ literal:v RQB) `(,nm ,v)))

  (literal
   ((ident:i) i)
   ; ... numbers?
   )


  )


