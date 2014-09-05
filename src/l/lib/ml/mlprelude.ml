(* Lisp part - it is really easy to mix different languages! *)

lisp "mlprelude.al";;

(* Core data types *)

type 'alpha list = Nil | Cons of 'alpha * 'alpha list;;
type 'alpha option = Some of 'alpha | None;;
type fclass = FNull | FList | FSymbol | FNumber | FString | FChar | FOther;;

type 'a deref = Deref of 'a | Ref of 'a;;

(* Foreign (Lisp) functions curried annotations *)

foreign mlplus ( number -> number -> number ) -> "+";;
foreign mlminus ( number -> number -> number ) -> "-";;
foreign mlmult ( number -> number -> number ) -> "*";;
foreign mldiv ( number -> number -> number ) -> "/";;

foreign itoa ( number -> string ) -> "to-string";;
foreign atoi ( string -> number ) -> "S->N";;
foreign eq ( number -> number -> bool ) -> "eq?";;

foreign mkhash ( unit -> 'alpha 'beta hashmap ) -> mlmkhash;;
foreign hashget ( 'alpha 'beta hashmap -> 'alpha -> 'beta) -> hashget;;
foreign hashput ( 'alpha 'beta hashmap -> 'alpha -> 'beta -> unit) -> hashput;;

foreign pr ( 'alpha -> unit) -> print;;
foreign genpr ( 'alpha -> unit) -> mlprint;;
foreign terpri ( unit -> unit) -> terpri;;

foreign strtolist ( string -> char list ) -> mlstringtolist;;
foreign listtostr ( char list -> string ) -> mllisttostring;;

foreign stringappend ( string -> string -> string ) -> "string-append";;

foreign tolisp ( 'a -> lisplist ) -> "ml-to-lisp";;
foreign lispcar ( lisplist -> 'a ) -> "car";;
foreign lispcdr ( lisplist -> 'a ) -> "cdr";;
foreign isnull ( lisplist -> 'a ) -> "null?";;
foreign lispclass ( lisplist -> fclass ) -> "lispclass";;
foreign lwrap ( 'a -> lisplist ) -> "I";;
foreign lispcons ( lisplist -> lisplist -> lisplist ) -> "cons";;
foreign lispnull ( unit -> lisplist) -> "ml-lispnull";;

foreign sethead ( 'a list -> 'a -> 'a list ) -> "ml-setcar!";;
foreign settail ( 'a list -> 'a -> 'a list ) -> "ml-setcar!";;

foreign ref ( 'a -> 'a refh ) -> "ml-mkref";;
foreign deref ( 'a refh -> 'a ) -> "ml-deref";;
foreign set ( 'a refh -> 'a -> a refh ) -> "ml-setref";;

let (<s>) = stringappend;;

foreign charEq (char -> char -> bool) -> "eq?";;

foreign force ('a deref -> 'a) -> "ml-deref-force";;

(* Core functions *)

let prln x = begin pr x; terpri () end;;
let genprln x = begin genpr x; terpri () end;;

let rec map f l =
   match l with 
      [] -> []
    | hd : tl -> (f hd) : (map f tl);;

let rec iter f l =
   match l with
     [] -> ()
   | hd : tl ->
      begin f hd; iter f tl end;;

let rec foldl f i l =
    match l with
      [] -> i
    | hd : tl ->
        foldl f (f i hd) tl;;

(* This definition for append is used by '::' syntax sugar. *)
let rec append a b =
    match a with 
       [] -> b
     | hd : tl -> hd : (append tl b);;

let rec invert =
     function
        [] -> []
      | hd : tl -> 
         (invert tl) :: (hd : []);;

let o a b f = a (b f);;

(* Infix versions *)
let (<o>) a b f = a (b f);;
let (<map>) = map;;
let (<iter>) = iter;;

(* Parsing combinators *)

type 'a 'b result = PError of string * 'b list | PResult of 'a list * 'b list;; 

let pplus p1 p2 src = 
    let r1 = p1 src in
    match r1 with
      PError(_,_) -> r1
    | PResult(res,rst) -> 
      (let r2 = p2 rst in
       match r2 with
          PError(msg,_) -> PError msg src
        | PResult(res2, rst2) -> PResult (res::res2) rst2);;


let por p1 p2 src = 
    let r1 = p1 src in
    match r1 with
      PError(_,_) -> p2 src
    | PResult(_,_) -> r1;;

let rec pmany p src =
    let r = p src in
    match r with
       PError(_,_) -> PResult [] src
     | PResult(res, rst) -> 
       (let r1 = pmany p rst in
        match r1 with
           PError(_, _) -> r
         | PResult(res1, rst1) -> PResult (res::res1) rst1);;

let (<p+>) = pplus;;
let (<p|>) = por;;

let poneormany p = 
    pplus p (pmany p);;

let rapply p f src =
    match p src with
       PError(msg, rst)  -> PError msg src
     | PResult(res, rst) -> PResult (f res) rst;;

let (<p@>) = rapply;;
    
let psingle f error = 
    function hd : tl -> if (f hd) then PResult [hd] tl else PError error tl
           | []      -> PError "EOF" [];;

let palways l = PResult [] l;;

let pany = psingle (fun x -> true) "";;

let pnever l = PError "" l;;

let pforget p = p <p@> (fun l -> []);;

(* Parsing for character streams *)

let pchar ch =
    psingle (charEq ch) "Wrong character";;

let pstring = 
    let rec loop = 
        function
             hd : [] -> pchar hd
         |   hd : tl -> (pchar hd) <p+> (loop tl)
         |   [] -> palways
    in loop <o> strtolist;;

let pstringor = 
    let rec loop = 
        function
              hd : [] -> pchar hd
          |   hd : tl -> (pchar hd) <p|> (loop tl)
          |   [] -> palways
    in loop <o> strtolist;;

let pcollect p = p <p@> (fun l -> [listtostr l]);;

