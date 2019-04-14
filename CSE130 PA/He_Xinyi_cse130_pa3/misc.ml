(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(* sqsum : int list -> int 
   *****  This function takes a list l and returns the sum of the square of each element.
          The base case is to add 0 and the square of the first element of l. 
          Everytime add a and the square of x until there is no more element. 
          a is the sum of square of the elements we have seen so far and x is the next element *****
*) 
let sqsum xs = 
  let f a x = a+x*x in
  let base = 0 in
    List.fold_left f base xs


(* pipe :  ('a -> 'a) list -> ('a -> 'a) 
   *****  This function takes a list of functions and returns a function f such that for any x, 
          the application f x returns the result fn(...(f2(f1 x))). 
          The base case is to call f1 on the function which returns its input x. 
          Everytime call the function x on the result of call a on x' until there is no more element in the function list. *****
*) 
let pipe fs = 
  let f a x = fun x' -> x (a x') in
  let base = fun x->x in
    List.fold_left f base fs


(* sepConcat : string -> string list -> -> string
   *****  This function takes a input string sep and a list of strings and return the string separated by the sep.
          If there is 0 strings in the list sl, then return "". 
          Otherwise everytime concatenate a which is the string we get so far with the sep and the next string until there is no more 
          string element in the list. *****
*) 
let rec sepConcat sep sl = 
      let f a x = a^sep^x in
      let base = "" in
        List.fold_left f base sl


(* stringOfList : ('a -> string) -> 'a list -> string
   *****  This function takes a input function f and a list l and returns a string representation of the list l.
          Call the function sepConcat on sep "; " and apply f on each element of l, then wrap the string with "[" and "]". *****
*) 
let stringOfList f l = "["^(sepConcat "; " (List.map f l))^"]"

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(* clone :'a -> int -> 'a list 
   *****  This curried function takes a input x and an integer n and returns a list of length n , where each element is x.
          If n is less than or equal to 0, then return empty list.
          Recursively append x to the list we've already cloned where each element is x until the list has length n. *****
*)
let rec clone x n = if n <=0 then [] else x::(clone x (n-1))

(* padZero :int list -> int list -> int list * int list
   *****  This curried function takes two lists and adds zerons in front to make the lists equal.
          If l1 has a smaller length, then clone corresponding number of 0s and append at the front of l1.
          Otherwise, clone corresponding number of 0s and append at the front of l2. *****
*)
let rec padZero l1 l2 = let len = List.length l1 - List.length l2 in (clone 0 (-len) @ l1, clone 0 len @ l2)

(* removeZero :int list -> int list
   *****  This function takes a list and removes a prefix of trailing zeros.
          If l is empty list then just return a empty list.
          Otherwise, recursively remove the head element if is a zero. *****
*)
let rec removeZero l = match l with 
                      | [] -> []
                      | h::t -> if h = 0 then removeZero t
                                else h::t

(* bigAdd :int list -> int list -> int list
   *****  This function takes two integer lists and returns the list corresponding to the addition of the two big integers. 
          First reverse l1 and l2 and combine them together because we calculate from the least significant digit
          For each pair in the combined list, add them together. 
          And if the result is larger or equal to 10, then have a carry = 1, and the digit should be x1+x2-10 and append it to the list we already have.
          If the result is less than 10, then have no carry which is 0, and the digit should be x1+x2 and append it to the list we already have.
          After we got the result, remove trailing zeros.*****
*)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = let (a1,a_list) = a in 
                let (x1,x2) = x in 
                      if x1+x2+a1 >= 10 then (1,(x1+x2+a1-10)::a_list) 
                                        else (0,(x1+x2+a1)::a_list)  in
    let base = (0,[]) in
    let args = (List.combine (List.rev l1) (List.rev l2))@[(0,0)]  in
    let (_, res) = List.fold_left f base args in
      res 
  in 
    removeZero (add (padZero l1 l2))

(* mulByDigit :int -> int list -> int list
   *****  This function takes an integer digit and a big integer, and returns the big integer list which is the result of multiplying the big integer with the digit.
          If the integer is less or equal to 0, then return empty list.
          Otherwise, recursively add l to the multiple of i-1 and l until we add i times in total.*****
*)
let rec mulByDigit i l = if i <= 0 then [] else bigAdd l (mulByDigit (i-1) l)

(* bigMul :int list -> int list -> int list
   *****  This function takes two integer lists and returns the list corresponding to the multiplication of the two big integers. 
          Evertime, multiply each digit of l2 and l1 list by calling the function mulByDigit.
          Then append a 0 after the result we now get to represent the corresponsing digit, and then add the two list together.*****
*)
let bigMul l1 l2 = 
  let f a x = let (a1,a_list) = a in 
              (1,bigAdd (mulByDigit x l1) (a_list@[0])) in
  let base = (1,[]) in
  let args = l2 in
  let (_, res) = List.fold_left f base args in
    res
