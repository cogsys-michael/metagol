:- use_module('../metagol').

%% tell metagol to use the BK
prim(s/2).

%% metarules
metarule(base,[P,A],([P,A]:-[])).
metarule(mutual,[P,Q,R],([P,A]:-[[Q,A,B],[R,B]]),PS):-
  member(sym(R,1,_),PS). %% explicitly bind R to a symbol on the predicate signature

%% background knowledge
s(10,9).
s(9,8).
s(8,7).
s(7,6).
s(6,5).
s(5,4).
s(4,3).
s(3,2).
s(2,1).
s(1,0).

%% learn the definition of even by inventing odd
a :-
  Pos = [even(10),even(8),even(6),even(4),even(2)],
  Neg = [even(3)],
  learn(Pos,Neg).