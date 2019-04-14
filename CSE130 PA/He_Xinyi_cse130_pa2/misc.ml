(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)

(* assoc : int * string * (string * int) list -> int 
   *****  This function takes a triple (d,k,l) where l is a list of key-valued pairs and finds the first ki that equals k.
          If such a ki is found, then vi is returned. Otherwise, the default value d is returned.
          If l is empty then return the default value d.
          Otherwise recursively search the tail of the list until a ki = k appears ot the list becomes empty. *****
*) 
let rec assoc (d,k,l) = 
	match l with
	| [] -> d
	| h::t -> let (a,b) = h in if a = k then b 
							   else (assoc[@tailcall]) (d,k,t) ;;

(* fill in the code wherever it says : failwith "to be written" *)

(* removeDuplicates: int list -> int list
   *****  This function takes a list l and returns the list of elements of l with the (2,3...)duplicates removed 
   		  and where the remaining elements appear in the same order as in l .
          If the rest of the list is empty, then return all the elements that have been seen.
          Otherwise, recursively search the rest of the list and add elements that have not been seen to the seen list. *****
*) 
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = if List.mem h seen then seen else h::seen in
        let rest' = t in 
	  (helper[@tailcall]) (seen',rest') 
  in
      List.rev (helper ([],l));;


(* Small hint: see how ffor is implemented below *)
(* wwhile: (int -> int * bool) * int -> int
   *****  This function takes a pair (f,b) and calls the function f on input b to get a pair (b',c').
          It keeps calling f on b' to update the pair as long as c' is true. Once f returns a c' that is false, wwhile should return b'.
		  It recursively calls f on b when c' is true and update c' everytime.
		  When c' is false, it returns b'. *****
*) 
let rec wwhile (f,b) = let (x,y) = f b in if y then (wwhile[@tailcall])(f, x) else x;;


(* fill in the code wherever it says : failwith "to be written" *)

(* fixpoint:(int -> int) * int -> int 
   *****  This function takes a pair (f,b) and repeatedly updates b with f(b) until b=f(b) and then returns b.
          It calls function wwhile on the pair (g,b) until f(b) = b. *****
*) 
let fixpoint (f,b) = wwhile (let g x= (f x, x!=f x) in g ,b);;


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
