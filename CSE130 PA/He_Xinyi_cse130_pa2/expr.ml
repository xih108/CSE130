(*
 * expr.ml
 * cse130
 * based on code by Chris Stone
 *)

(* Please do not modify the names or types of any of the following
 * type constructors, or predefined functions, unless EXPLICITLY
 * asked to. You will loose points if you do.
 *)


(* REMEMBER TO DOCUMENT ALL FUNCTIONS THAT YOU WRITE OR COMPLETE *)

(* This is how we represent expressions of the grammar using the datatype.*) 
type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr	
  | Cube   of expr
  | Positive  of expr * expr * expr

(* exprToString : expr -> string
   *****  This function takes an expression and enable the printing of it.
          Recursively match each part of the expression with its corresponding string until every expression has been converted to string. *****
*) 
let rec exprToString e = 
            match e with 
            | VarX -> "x"
            | VarY -> "y"
            | Sine e -> "sin(pi*"^exprToString(e)^")"
            | Cosine e -> "cos(pi*"^exprToString(e)^")"
            | Average (e1,e2) -> "(("^exprToString(e1)^"+"^exprToString(e2)^")/2)"
            | Times (e1,e2) -> exprToString(e1)^"*"^exprToString(e2)
            | Thresh (e1,e2,e3,e4) -> "("^exprToString(e1)^"<"^exprToString(e2)^"?"^exprToString(e3)^":"^exprToString(e4)^")"
            | Cube (e) -> exprToString(e)^"^3"
            | Positive (e,e1,e2) -> "("^exprToString(e)^">0 ?"^exprToString(e1)^":"^exprToString(e2)^")"


(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
let buildCube(e) = Cube(e)
let buildPositive(e,e1,e2) = Positive (e,e1,e2)

let pi = 4.0 *. atan 1.0

(* eval: expr * float * float -> float
   *****  This function takes an triple (e,x,y) and evaluates the expression e at the point x,y
          Recursively match each part of the expression with its corresponding function and evaluates at the point (x,y) 
          until every expression has been evaluated. *****
*) 
let rec eval (e,x,y) = 
              match e with
              | VarX -> x
              | VarY -> y
              | Sine(e)  -> sin(pi*.eval (e,x,y))
              | Cosine(e) -> cos(pi*.eval (e,x,y))
              | Average(e1,e2) -> ((eval (e1,x,y)+.eval (e2,x,y))/.(2.0))
              | Times(e1,e2) -> eval (e1,x,y)*.eval (e2,x,y)
              | Thresh(a,b,a_less,b_less) -> (if eval (a,x,y) < eval (b,x,y) then eval (a_less,x,y) else eval (b_less,x,y))
              | Cube(e) -> eval (e,x,y)*.eval (e,x,y)*.eval (e,x,y)
              | Positive(e,e1,e2) -> (if eval(e,x,y) > 0.0 then eval (e1,x,y) else eval (e2,x,y))
          

(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here ***************)
