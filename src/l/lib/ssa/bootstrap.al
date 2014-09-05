
(with-macros ((hlevl-file (fun (x) `(begin )))
	      (top-begin (fun (x) `(begin ,@(cdr x))))
	      (pfront.debugpoint (fun (x) `(inner.debugpoint ,@(cdr x))))
	      )
  (include "./ssa.al"))

