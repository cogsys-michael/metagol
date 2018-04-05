:- use_module('../metagol').
:- use_module(library(apply)).

%% background knowledge
divisible(X,Y) :- nonvar(X), nonvar(Y), !, X mod Y =:= 0.
divisible(X,Y) :- nonvar(X), var(Y), 
  SQRT is sqrt(X), 
  ( (SQRT=:=floor(SQRT), Y is floor(SQRT)) ;
    Max is ceiling(SQRT)-1,
    between(1,Max,I), X mod I =:= 0,
    (Y=I ; Y is X//I)  
  ).

  
%% tell metagol to use the BK
prim(divisible/2).


%% metarules
metarule([P,Q,B],([P,A]:-[[Q,A,B]])).
metarule([P,Q,R],([P,A]:-[[Q,A],[R,A]])).

metagol:min_clauses(1).
metagol:max_clauses(4).

%% 
a(Progs) :-
  examples(4,400,Pos,Neg),
  metagol_sn(Pos,Neg,0.10,3,Progs).

a :- a(Progs), pprint_snt(Progs).

  
leap_year(Y) :- divisible(Y,4), 
  (divisible(Y,100) -> divisible(Y,400) ; true).

examples(From,To,Pos,Neg) :-
  findall(leapyear(N),(between(From,To,N), leap_year(N)), Pos),
  findall(leapyear(N),(between(From,To,N), \+(leap_year(N))), Neg).
