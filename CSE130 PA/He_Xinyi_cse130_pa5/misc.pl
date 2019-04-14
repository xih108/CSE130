%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helpers

%isin(X,L) is true if X appears in L
isin(X,[X|_]).
isin(X,[_|T]) :- isin(X,T).

% zip(L1,L2,L3) is true if L3 is the result of interleaving L1 and L2
% e.g. zip([1,2],[3,4],[1,3,2,4])   is true
zip([],[],[]).
zip([H1|T1],[H2|T2],[H1,H2|T]) :- zip(T1,T2,T).

% assoc(L,K,V) is true if L is a list of 2-element lists and one of them is [K,V]
% e.g. assoc([[key1,value1],['a',1],[3,4]], 3, 4) is true
assoc([[X,Y]|_],X,Y).
assoc([_|T],X,Y) :- assoc(T,X,Y).

% remove_duplicates(L1,L2) is true if L2 is the result of removing all duplicate elements from L1.
% The remaining elements should be in the original order.
% e.g. remove_duplicates([1,1,2,2,3,3,4,4],[1,2,3,4]) is true
clean([],Soln,Y) :- reverse(Y,Soln).
clean([H|T],Soln,Y) :- isin(H,Y),!,clean(T,Soln,Y).
clean([H|T],Soln,Y) :- clean(T,Soln,[H|Y]).
remove_duplicates(L1,L2) :- clean(L1,L2,[]). 

% union(L1,L2,L3) is true if L3 is the set union of L1 and L2. 
% There should be no duplicates in L3.
% e.g. union([1,2,3],[2,3,4],[1,2,3,4]) is true
union(L1,L2,L3) :- append(L1,L2,L),remove_duplicates(L,L3). 

% intersection(L1,L2,L3) is true if L3 is the set intersection of L1 and L2.
% There should be no duplicates in L3.
% e.g. intersection([1,2,3],[2,3,4],[2,3]) is true
its([],_,X,Y) :- reverse(X,Y).
its([H|T],L,X,Y) :- isin(H,L),!,its(T,L,[H|X],Y).
its([_|T],L,X,Y) :- its(T,L,X,Y).
intersection(L1,L2,L3) :- its(L1,L2,[],L3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 1: Facts

cost(carne_asada,3).
cost(lengua,2).
cost(birria,2).
cost(carnitas,2).
cost(adobado,2).
cost(al_pastor,2).
cost(guacamole,1).
cost(rice,1).
cost(beans,1).
cost(salsa,1).
cost(cheese,1).
cost(sour_cream,1).
cost(taco,1).
cost(tortilla,1).
cost(sopa,1).


ingredients(carnitas_taco, [taco,carnitas, salsa, guacamole]).
ingredients(birria_taco, [taco,birria, salsa, guacamole]).
ingredients(al_pastor_taco, [taco,al_pastor, salsa, guacamole, cheese]).
ingredients(guacamole_taco, [taco,guacamole, salsa,sour_cream]).
ingredients(al_pastor_burrito, [tortilla,al_pastor, salsa]).
ingredients(carne_asada_burrito, [tortilla,carne_asada, guacamole, rice, beans]).
ingredients(adobado_burrito, [tortilla,adobado, guacamole, rice, beans]).
ingredients(carnitas_sopa, [sopa,carnitas, guacamole, salsa,sour_cream]).
ingredients(lengua_sopa, [sopa,lengua,beans,sour_cream]).
ingredients(combo_plate, [al_pastor, carne_asada,rice, tortilla, beans, salsa, guacamole, cheese]).
ingredients(adobado_plate, [adobado, guacamole, rice, tortilla, beans, cheese]).

taqueria(el_cuervo, [ana,juan,maria], 
        [carnitas_taco, combo_plate, al_pastor_taco, carne_asada_burrito]).

taqueria(la_posta, 
        [victor,maria,carla], [birria_taco, adobado_burrito, carnitas_sopa, combo_plate, adobado_plate]).

taqueria(robertos, [hector,carlos,miguel],
        [adobado_plate, guacamole_taco, al_pastor_burrito, carnitas_taco, carne_asada_burrito]).

taqueria(la_milpas_quatros, [jiminez, martin, antonio, miguel],  
        [lengua_sopa, adobado_plate, combo_plate]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 1: Rules

% available_at(X,Y): It is true if the item X is available at the taqueria Y. 
% Just check whether X is in the list of items avalible at taqueria Y.
available_at(X,Y) :- taqueria(Y,E,L), isin(X,L).

% multi_helper(X): It is true if X is available at two different taquerias Y and Z.
multi_helper(X) :- available_at(X,Y),available_at(X,Z),not(Y=Z).

% multi_available(X): It is true if the item X is available in more than one place. 
% Just check whether X is in the list of items that are available in more than one place without duplicates.
multi_available(X) :- bagof(X,multi_helper(X),Bag), remove_duplicates(Bag,L2), isin(X,L2).

% overworked_helper(X): It is true if the person X works at two different taquerias Y1 and Y2.
overworked_helper(X) :- taqueria(Y1,E1,L1), isin(X,E1), taqueria(Y2,E2,L2), isin(X,E2), not(Y1=Y2).

% overworked(X): It is true if the person X works at more than one taqueria. 
% Just check whether X is in the list of employees that worked at more than one taqueria without duplicates.
overworked(X) :- bagof(X,overworked_helper(X),Bag), remove_duplicates(Bag,L), isin(X,L). 

% cost_helper(L,N): It is true if N is the total cost of ingredients in list L. 
% If L is empty, the total cost should be 0. 
% Otherwise, recursively add the cost of the head ingredient and the total cost of the rest ingredients.
cost_helper([],0).
cost_helper([H|T],Cost):- cost_helper(T,T_cost), cost(H,H_cost),Cost is T_cost + H_cost.
% total_cost(X,K): It is true if the sum of the costs of the ingredients of item X is equal to K.
% Just check whether the total cost of the ingredients of X equals K.
total_cost(X,K) :- ingredients(X,L),cost_helper(L,N),K=N. 

% has_ingredients(X,L): It is true if the item X has all the ingredients listed in L.
% Just check whether all elements of L are contained in the list of ingredients of X.
has_ingredients(X,L) :- ingredients(X,L1), intersection(L,L1,L).

% avoids_ingredients(X,L): It is true if the item X does not have any of the ingredients listed in L.
% Just check whether L does not have any intersection with the list of ingredients of X.
avoids_ingredients(X,L) :- ingredients(X,L1), intersection(L,L1,[]).

% p1(L,X): It is true if L is the list of items that contains all ingredients in X.
p1(L,X) :- bagof(G,has_ingredients(G,X),L). 

% p2(L,Y): It is true if L is the list of items that do not contain any of the ingredients in Y.
p2(L,Y) :- bagof(G,avoids_ingredients(G,Y),L).

% find_items(L,X,Y): It is true that L is the list of all items that contain all the ingredients in X and do not contain any of the ingredients in Y
find_items(L,X,Y) :- p1(L1,X),p2(L2,Y),intersection(L1,L2,L).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
