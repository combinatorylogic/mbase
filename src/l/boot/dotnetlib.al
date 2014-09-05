;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; .NET-Specific library (using reflection)

(macro vector args
  ("Creates a vector of values, an element type is derived from"
   "the first value type.")
   `(mkvector (list ,@args)))
   

(macro ovector args
   "Creates an Object vector of given values."
   `(mkovector (list ,@args)))

;; the very basic .NET types
(define t_int  (r_typebyname "System.Int32"))
(define t_string (r_typebyname "System.String"))
(define t_object (r_typebyname "System.Object"))
(define t_type (r_typebyname "System.Type"))
(define e_type (r_typebyname "System.Exception"))

(topblock
(define writeline0 println)
        
(macro writeline args
  ("Prints a string of arguments into a standard output,"
   "using the [to-string] function to print each value.")
   (cond
    ((> (length args) 1)
       `(writeline0 (buildstring ,@(map (lambda (x) `(to-string ,x)) args))))
    ((null? (cdr args))
       `(writeline0 (to-string ,(car args))))
    (else
       '(writeline0 nil))))

(function writeline (arg)
  ("Function counterpart for the [writeline] macro."
   "Prints a string to standard output using"
   "[to-string] conversion function.")
   (println (to-string arg)))

(function r_typer0 (tp)
   (if (string? tp) (r_typebyname tp)
      (cond
        ((eqv? tp 'int) t_int)
        ((eqv? tp 'string) t_string)
        ((eqv? tp 'object) t_object)
        (else tp))))

(function r_getmethod01 (a b c)
   (let ((res (r_getmethod0 a b c)))
     (if (null? res)
          (begin
             (println (buildstring "Method undefined " a ", " b))
             nil)
          res)))
        
(function r_typerx (tp)
  "Evaluates the [Type] object for a given symbolic type name."
  (cond
        ((eqv? tp 'int) (r_typer0 'int))
        ((eqv? tp 'string) (r_typer0 'string))
        ((eqv? tp 'object) (r_typer0 'object))
        ((or (symbol? tp) (list? tp)) (read-int-eval tp))
        ((string? tp) (r_typebyname tp))
        (else tp)))

(macro r_typer (tp)
  ("Expands into the [Type] object"
   "evaluation for the given symbolic \\NET{} type name.")
   `(r_typer0 ,(cond 
        ((eqv? tp 'int) '(quote int))
        ((eqv? tp 'string) '(quote string))
        ((eqv? tp 'object) '(quote object))
        (else tp))))

)
(macro r_mtd (class method . args) 
   "Expands into a [MethodInfo] for the specified class's method."
    `(r_getmethod01
       ,(if (string? class) `(r_typebyname ,class) `(r_typer ,class))
       ,method
       ,(if (> (length args) 0) `(vector ,@(map (lambda (x) `(r_typer ,x)) args))
                           'nil)))

(function r_mtdf (class method args) 
  ("A function version of the [r_mtd] macro, evaluates"
   "a [MethodInfo] object for a given method signature.")
  (r_getmethod01 
   (r_typerx class)
   method
   (if (> (length args) 0) (mkvector (map (lambda (x) (r_typerx x)) args))
       nil)))
                          
                           
(macro r_bind0 (tp class method . args)
   (let* ((l (length args))
          (alist (let loop ((i 0)) 
                    (if (< i l) 
                     (cons 
                      (string->symbol (string-append "arg-" (number->string i)))
                      (loop (+ i 1)))
                     nil))))           
    `(let*
      ((typ ,(if (string? class) `(r_typebyname ,class) class))
       (argst ,(if (> l 0) `(vector ,@(map (lambda (x) `(r_typer ,x)) args))
                           'nil))
       (mtd (r_getmethod01 typ ,method argst))
       (isitbool (r_boolmethodp mtd))
       (wrapper (if isitbool
		    r_debool
		    (fun (l) l)))
       )
      ,(if (> tp 0)
        `(lambda (*O* ,@alist) (wrapper (r_invoke mtd *O* ,(if (> l 0) `(ovector ,@alist) 'nil))))
        `(lambda (,@alist) (wrapper (r_invoke mtd nil ,(if (> l 0) `(ovector ,@alist) 'nil))))))))

;; redefined as cmacros later.        
(macro r_bind args
  ("Binds a method via reflection, expands into the"
   "wrapper function for that method.")
  `(r_bind0 1 ,@args))
(macro r_sbind args 
  ("Binds a {\\sl static} method via reflection, expands"
   "into a wrapper function for that method.")
   `(r_bind0 0 ,@args))
(macro r_bindx args 
  `(r_bind0 1 ,@args))
(macro r_tbind args
  ("Expands into a wrapper function for a given \\NET{}"
   "method, for interpteded mode is the same as [r_bind].")
 `(r_bind ,@args))
(macro r_tsbind args
  ("Expands into a wrapper function for a given \\NET{} static"
   "method, for interpteded mode is the same as [r_sbind].")
 `(r_sbind ,@args))

(function exit (code)
  "Exit with a given termination code."
  ((r_tsbind  "System.Environment" "Exit" int) code))

(function quit () 
  "Exit with a code '0'."
  (exit 0))

(macro new (classname . args)
  (
   "Expands into the code creating an instance of a"
   "given class, with a given constructor arguments values and types."
   "Here [args] is a list of [(type value)] lists."
   )
   (if (null? args) `(r_new (r_typer ,classname) (anew t_type 0) nil)
     `(r_new (r_typer ,classname) (vector ,@(map (lambda (x) `(r_typer ,x)) (map car args)))
         (ovector ,@(map cadr args)))))

(define Encoding.UTF8 ((r_tsbind "System.Text.Encoding" "get_UTF8")))
(define t_Encoding (r_typer "System.Text.Encoding"))

(force-class-flush)

(function io-open-write (filename)
  "Opens a file stream for writing."
 (new "System.IO.StreamWriter" ("System.String" filename)))

(function io-open-read (filename)
  "Opens a file stream for reading."
   (new "System.IO.StreamReader" ("System.String" filename) ("System.Text.Encoding" Encoding.UTF8)))

(function io-open-string (str)
   "Opens a string stream for reading."
   (new "System.IO.StringReader" ("System.String" str)))

(define default-path r_basepath)
 
(define io-read
   "Reads an S--expression from the given stream."
   (let ((aread (r_tsbind "Meta.Scripting.Reader" "aread" 
			  "System.IO.TextReader"))
         (proc (r_tsbind "Meta.Scripting.Reader" "process" object)))
         
     (@ proc aread)))

(define xio-read
   (let ((aread (r_tsbind "Meta.Scripting.Reader" "aread" 
			  "Meta.Scripting.ExtendedReader"))
         (proc (r_tsbind "Meta.Scripting.Reader" "process" object)))
         
     (@ proc aread)))

(define readline 
  "Reads a line from the given stream."
  (r_tbind "System.IO.StreamReader" "ReadLine"))
        

(define io-wclose 
  "Closes an output stream."
  (r_tbind "System.IO.StreamWriter" "Close"))

(define io-close
  "Closes an input stream."
  (r_tbind "System.IO.TextReader" "Close"))

(define xio-close
  (r_tbind "Meta.Scripting.ExtendedReader" "Close"))


(define path_fullpath (r_tsbind "System.IO.Path" "GetFullPath" string))
(define dirinfo-fullpath (r_tbind "System.IO.DirectoryInfo"
				  "get_FullName"))

(function new_dirinfo_path (src)
  (let ((di (new "System.IO.DirectoryInfo" (string src))))
    (path_fullpath (dirinfo-fullpath di))))


(define _getpath (r_tsbind "System.IO.Path" "GetDirectoryName" string))
(define mkreader 
  (r_tsbind "Meta.Scripting.ExtendedReader" "mkreader" "System.IO.TextReader"))

(function bootlib:push-module-env () 
  (shashput (getfuncenv) '*MODULE-SEARCH-PATH-STACK*
            (cons (shashget (getfuncenv) '*MODULE-SEARCH-PATH*)
                  (shashget (getfuncenv) '*MODULE-SEARCH-PATH-STACK*))))

(function bootlib:pop-module-env () 
  (let ((stack (shashget (getfuncenv) '*MODULE-SEARCH-PATH-STACK*)))
    (shashput (getfuncenv) '*MODULE-SEARCH-PATH-STACK* (cdr stack))
    (shashput (getfuncenv) '*MODULE-SEARCH-PATH* (car stack))))

(function bootlib:add-module-env (path) 
  (shashput (getfuncenv) '*MODULE-SEARCH-PATH*
            (append path
                    (shashget (getfuncenv) '*MODULE-SEARCH-PATH*))))

(macro using-modules (path . body)
  `(top-begin
     (ctimex (begin (bootlib:push-module-env)
                    (bootlib:add-module-env (quote ,path))))
     ,@body
     (force-class-flush)
     (ctimex (bootlib:pop-module-env))
     ))

(macro include (fnm)
  ("Expands into the list of values from a given file,"
   "enclosed into [(top-begin ...)] statement.")
  (let* 
      ((oxpath (corelib:get-lookup-path))
       (fn  (new_dirinfo_path (buildstring oxpath "/" fnm)))
       (fi0 (io-open-read fn))
       (fi  (mkreader fi0))
       (res `(top-begin
               (ctimex (bootlib:push-module-env))
	       (ctimex (corelib:set-lookup-path ,(_getpath fn)))
	       ,@(let loop () (let ((r (xio-read fi)))
				(if (null? r) nil
				    (cons r (loop)))))
               (force-class-flush)
	       (ctimex (corelib:set-lookup-path ,oxpath))
               (ctimex (bootlib:pop-module-env))
	       )))
    (xio-close fi)
    (if (shashget (getfuncenv) 'debug-display-include-paths)
	(println (buildstring "include file: " fn)))
    res))

(function generic-filepath (fnm)
  (let* 
      ((oxpath (corelib:get-lookup-path))
       (fn  (buildstring oxpath "/" fnm)))
    (new_dirinfo_path fn)))

(macro generic-include (fnm . code)
  (let* 
      ((oxpath (corelib:get-lookup-path))
       (fn  (buildstring oxpath "/" fnm))
       (res `(top-begin
	       (ctimex (corelib:set-lookup-path ,(_getpath fn)))
               ,@code
	       (ctimex (corelib:set-lookup-path ,oxpath))
	       )))
    res))

(function include-alc (fnm)
  (let* ((oxpath (corelib:get-lookup-path))
	 (fn (buildstring oxpath "/" fnm)))
    (load-alc fn)
    (corelib:set-lookup-path oxpath)))

(function read-str (str)
  "Reads an S--expression from a given string."
  (let* ((rr (io-open-string str))
         (rv (io-read rr)))
     (io-close rr)
     rv))

;;;;;;;;;;;;;;;;;;;
; 

(Par
 "From now on, since [include] macro is now defined, the rest of "
 "the code appears in a more ordered way."
 )

(include "./initlib2.al")