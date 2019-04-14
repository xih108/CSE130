% envtype(Env,X,T) is true if [X,T] belongs to Env and [X,T] is the first occurrence of a binding for variable X.
envtype([[Var,Ty]|_],Var,Ty).
envtype([[K|_]|T],Var,Ty) :- not(K=Var),envtype(T,Var,Ty).

int_op(plus).
int_op(minus).
int_op(mul).
int_op(div).

comp_op(eq).
comp_op(neq).
comp_op(lt).
comp_op(leq).

bool_op(and).
bool_op(or).

% typeof for constant just match T with int
typeof(_,const(_),T):- T = int.

% typeof for bool just match T with bool
typeof(_,boolean(_),T):- T= bool.

% typeof for nil just match T with list()
typeof(_,nil,T):- T=list(_).

% typeof for an variable by implementing the envtype just wrote
typeof(Env,var(X),T) :- envtype(Env,X,T).

% typeof for plus,minus,mul,div by match each expression with int
typeof(Env,bin(E1,Bop,E2),int) :- int_op(Bop), typeof(Env,E1,int), typeof(Env,E2,int).

% typeof for eq,neq,lt,leq by match each expression has the same type
typeof(Env,bin(E1,Bop,E2),bool) :- comp_op(Bop), typeof(Env,E1,T), typeof(Env,E2,T).

% typeof for and,or by match each expression with bool
typeof(Env,bin(E1,Bop,E2),bool) :- bool_op(Bop), typeof(Env,E1,bool), typeof(Env,E2,bool).

% typeof for cons by match E1 with type T and E2 with tyoe list(T)
typeof(Env,bin(E1,cons,E2),list(T)) :- typeof(Env,E1,T), typeof(Env,E2,list(T)).

% typeof for ite: match E1 with bool, and E2, E3 should have same type
typeof(Env,ite(E1,E2,E3),T) :- typeof(Env,E1,bool), typeof(Env,E2,T), typeof(Env,E3,T).

% typeof for let, store the type of E1 as the type of X into Env and get the type of E2
typeof(Env,let(X,E1,E2),T) :- typeof(Env,E1,T1), append([[X,T1]],Env,Env1), typeof(Env1,E2,T).

% typeof for letrec, store the type of X into Env and get the type of E1, then E2
typeof(Env,letrec(X,E1,E2),T) :-  append([[X,T1]],Env,Env1),typeof(Env1,E1,T1), typeof(Env1,E2,T).

% typeof for fun, store the type of X into Env and get the type of E
typeof(Env,fun(X,E),arrow(T1,T2)) :- append([[X,T1]],Env,Env1),typeof(Env1,E,T2).

% typeof for app, apply E2 on function E1, first get the type of E1 and then evaluate E2.
typeof(Env,app(E1,E2),T) :- typeof(Env,E1,arrow(T1,T)),typeof(Env,E2,T1).
