;; TODO!!!!
;; derived AST new tags must be *appended* to the original AST tags.
;; When deriving from more than one sources, "newtags" flag must be added
;; which should force a tag rewrite (i.g., marking all the variant nodes for a
;; visitor attention.


;; TODO!!!!
;; Check if source and destination definitions match sanely

;; TODO!!!!
;; Use the destination format for explicit and implicit builders, not the source one


;; All the high-level ASTs
(include "./ast2-ast.al")
;; Translating the frontend macro language into AST
(include "./ast2-frontend.al")
;; Maintaining the AST hierarchy
(include "./ast2-hier.al")
;; Fusing AST definition into a visitor
(include "./ast2-fuse.al")
;; Lowering a visitor into an executable code
(include "./ast2-lowering.al")
;; Platform-specific backend
(include "./ast2-backend.al")
;; Integration
(include "./ast2-macros.al")

;; Translation sequence:
; ast-front-lower + ast-merge-inherited for each def:ast macro
; ast-variant-hash
;                 \
;                  ----> for each ast:visit:new macro
;                  /
; ast-visitor-lower
;  ->
;    ast-visitor-fuse (with the ast source, not hash)
;       -> visitor-inject-listnodes
;           -> visitor-backend-prepare
;               -> visitor-refine-else
;                  -> visitor-backend-lowering
;                     -> visitor-populate-varids
;                        -> visitor-lower-further
;
; For list-formatted input, after visitor-refine-else:
;    ast-listform-lowering
;
; For each AST, list->ast and ast->list transformers are generated.
; 