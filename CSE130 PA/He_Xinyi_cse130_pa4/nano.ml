exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     
| NativeFunc of string

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"
  | NativeFunc s ->
      Printf.sprintf "Native %s" s

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | NilExpr -> 
        "[]"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x  (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

(* lookup : string * env -> value
   *****  This function takes a string x and returns the value associates with it that appears first in the environment.
          If x is not in the environment, then raise MLFailure.   *****
*) 
let lookup (x,evn) = match listAssoc(x,evn) with
					| None -> raise (MLFailure ("variable not bound: "^x))
					| Some i -> i 

(*  eval: env * expr -> value that when called with the pair (evn,e) n the environment evn.
   *****  This function takes a pair environment and expression e and evaluates the NanoML expression.
   Evaluate Const to Int, True/False to Bool.
   Evaluate Basic Binary Operations, Plus to +, Minus to -, Mul to *,Div to /, Eq to = ,Ne to !=, Lt to <, Lt to <=,
   And to &&, Or to || . If there is any operation doesn't match, then raise Failure.
   Evaluates Cons to :: in list operation. *****
*) 	 						  
let rec eval (evn,e) = 
	match e with
	| Const i -> Int(i) 
	| True -> Bool(true)
	| False -> Bool(false)
	| Var s -> if s = "hd" then NativeFunc "hd" 
				else if s = "tl" then NativeFunc "tl"
			    else if s = "null" then NativeFunc "null"
			    else if s = "map" then NativeFunc "map"
				else if s = "foldl" then NativeFunc "foldl"
				else lookup (s,evn)	
	| Bin (e1,op,e2) ->	(
	let l = eval(evn,e1) in 
	let r = eval(evn,e2) in
		match (l,op,r) with
		|(Int x,Plus,Int y) -> Int((+) x y)
		|(Int x,Minus,Int y) -> Int((-) x y)
		|(Int x,Mul,Int y) -> Int(x*y)
		|(Int x,Div,Int y) -> Int(x/y)
		|(Int x,Eq,Int y) -> Bool(x = y)
		|(Bool x,Eq,Bool y) -> Bool(x = y)
		|(Int x,Ne,Int y) -> Bool(x != y )
		|(Bool x,Ne,Bool y) -> Bool(x != y )	
		|(Int x,Lt,Int y) -> Bool(x < y )	
		|(Int x,Le,Int y) -> Bool(x <= y )	
		|(Bool x,And,Bool y) -> Bool(x && y )	
		|(Bool x,Or,Bool y) -> Bool(x || y )	
		|(x,Cons,y) -> Pair (x,y)
		|(_,_,_) -> raise (MLFailure ("Operation doesn't match: " ^ valueToString l ^ binopToString op ^ valueToString r))
	)

(*  If (e1,e2,e3): is to check whether e1 is true. If it is, then evaluate expression e2, otherwise evaluate expression e3.
    If the type of the value of e1 is not Bool ,then raise failure. 
*) 	 
	| If (e1,e2,e3) -> (
		match eval(evn,e1) with
		| Bool p -> if p then eval(evn,e2) else eval(evn,e3)
		| _ -> raise (MLFailure ("This expression was expected of type Bool"))
	)

(*  Let (s,e2,e3): s is the Var string and save the eval of e1 and save as s then push into the environment.
	Then evaluate the value of e2 in the updated environment.
*)
	| Let (s,e1,e2) -> let evn' = (s,eval(evn,e1))::evn in eval(evn',e2)

(*  Fun (x,e): x is a Var string and is the name of the function, evaluate the function as Closure.
	evn is the environemt, and n,x,e are the name, formal parameter, and body expression of the function. 
	If the function is anonymous or declared in a let statement, the name should be None. 
If the function is declared in a let rec statement, then the name of the function should be Some f. 
*)
	| Fun (x,e) -> Closure(evn,None,x,e)

(*	App(e1,e2) corresponds to applying e2 to the function e1 via the expression Fun (x,e).
	First evaluate e1 and e2, then do the next steps based on different cases.
*)
	| App (e1,e2) -> (
		let l = eval(evn,e1) in
		let r = eval(evn,e2) in
		match l with 

(*	NativeFunc "hd" returns the head element of a list if it is not empty. 
	If the list is empty then raise Failure.
	If the the passed in argument is not a list, the also raise Failure.
 *)
		| NativeFunc "hd" ->
			(match r with
			| Pair (head,_) -> head
			| Nil -> raise (MLFailure ("The list is empty")) 
			| _ -> raise (MLFailure ("The argument is not a list"))) 

(*	NativeFunc "tl" returns the rest list except the head element of a list if it is not empty. 
	If the list is empty then raise Failure.
	If the the passed in argument is not a list, the also raise Failure.
*)
		| NativeFunc "tl" ->
			(match r with
			| Pair (_, tail) -> tail
			| Nil -> raise (MLFailure ("The list is empty")) 
			| _ -> raise (MLFailure ("The argument is not a list")))

(*	NativeFunc "null" checks whether a list empty or not. If it is, then returns  true.
	If the the passed in argument is not a list, the also raise Failure.
*)
		| NativeFunc "null" -> 
			(match r with
			| Nil -> Bool(true)
			| Pair (_,_) -> Bool(false)
			| _ -> raise (MLFailure ("The argument is not a list"))) 	

(*	NativeFunc "map" applies the function on each element of the list and returns the list.
	Otherwise, the function call is in ill-typed manner.
*)
		| NativeFunc "map" ->
			(match r with
			| Closure(evn1,n,x,e) -> Closure(evn1,Some "map",x,e)
			| _ ->  raise (MLFailure ("This is an ill-typed manner"))) 

(*	NativeFunc "flodl" applies the function on the base case and then each accumulator.
	Otherwise, the function call is in ill-typed manner.
*)
		| NativeFunc "foldl" ->
			(match r with
			| Closure(evn1,n,x,e) -> Closure(evn1,Some "foldl",x,e)
			| _ ->  raise (MLFailure ("This is an ill-typed manner"))) 	
		
		| Closure(evn1,n,x,e) -> 
			(match n with
			|None -> let new_evn = (x,r)::evn1 in eval(new_evn,e)

(*  Use a recursive helper function to everytime map the function on the head element 
	and then recursively map it on the rest of the list.
*)
		    |Some "map" -> (let rec map_helper (Closure(evn,n, x,e),l) = 
						     match l with
							 |Nil -> Nil		   	  
	  		     			 |Pair (h,t) ->	let head = eval((x,h)::evn,e) in 
											Pair(head,(map_helper (Closure(evn, n, x,e),t)))
	  		     							in map_helper (Closure(evn1, n, x,e),r))

(*  First match to check the type of r. 
	If r is a list then recursively call function on each element and update
	the accumulator each time.
	If r is not a list, which means it is the base case, so save the base case in the environment.
*)
	        |Some "foldl" ->  (match r with 
	    					  | Pair(a,b) -> 
	    					   (let rec helper(Closure(evn,n, x, e),l) = 
									let Closure(evn_f,n_f,x_f,e_f) = eval(evn,e) in 
										match l with 
										| Nil -> lookup(x, evn)
										| Pair(h,t) ->
					 			 			let evn2 = (x_f,h)::evn in
					 			 			let acc = eval(evn2,e_f) in 
					 			 			let new_evn = (x,acc) ::evn in 
					 			 			helper(Closure(new_evn,n,x,e),t)
										| _ -> raise (MLFailure("This is ill-typed."))
									  in helper (Closure(evn1,n,x,e),r))
	        				  | _ -> Closure((x, r)::evn1, n, x, e))

		    |Some f -> let new_evn = (f, Closure (evn1,Some f,x,e))::(x,r)::evn1 in eval(new_evn,e))
		)

(*	Letrec (s,e1,e2): s is a Var string and first evaluate e1 and save as evn1.
	Check the type of e1, if it is a closure then return a closure with string option Some and save it to the environemnt.
	Otherwise, do the same thing as let(s,e1,e2).
  *)
	
	| Letrec (s,e1,e2) ->  
			(let v = eval (evn, e1) in
    		let evn1 = match v with 
    			| Closure (evn',None,x,e) -> Closure (evn',Some s,x,e)
    			| _ -> v
			in eval((s,evn1)::evn,e2))
	| NilExpr -> Nil
	


(**********************     Testing Code  ******************************)
