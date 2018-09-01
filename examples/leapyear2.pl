:- use_module('../metagol').

%% background knowledge
% This is background knowledge B_div
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

%% parameters
metagol:max_fp_frac(0.10).
metagol:max_clauses(5).

%% 
% target predicate
leapyear(Y) :- divisible(Y,4), 
  (divisible(Y,100) -> divisible(Y,400) ; true).

% separate years from From to To into positive and 
% negative examples for leap_year/1 
examples(From,To,Pos,Neg) :-
  findall(leapyear(N),(between(From,To,N), leapyear(N)), Pos),
  findall(leapyear(N),(between(From,To,N), \+(leapyear(N))), Neg).

% use Metagol_SN
a:-
  examples(4,400,Pos,Neg),
  metagol_sn(Pos,Neg,3,_Depth,Progs),
  pprint(Progs).
  
% Metagol_AI will fail on this task
b:-
  examples(4,400,Pos,Neg),
  learn(Pos,Neg).
