:- use_module('../metagol').

%% background knowledge
even(X) :- X mod 2 =:= 0.

%% tell metagol to use the BK
prim(even/1).

%% metarules
metarule([P, Q], ([P, A, _B]:-[[Q, A]])).
metarule([P, Q], ([P, _A, B]:-[[Q, B]])).
metarule([P, Q, R], ([P, A, B]:-[[Q, A], [R, B]])).


%% parameters
metagol:max_fp_frac(0.5).

%%
a :-
  % positive examples
  Pos = [
    one_even(1,2),
    one_even(1,4),
    one_even(2,3),
    one_even(2,5),
    one_even(3,2),
    one_even(3,4),
    one_even(3,18)
  ],
  % negative examples
  Neg = [
    one_even(1,1),
    one_even(1,3),
    one_even(1,5),
    one_even(2,2),
    one_even(2,4),
    one_even(4,8),
    one_even(3,7)
  ],
  metagol_sn(Pos,Neg,2,_Depth,Progs),
  pprint(Progs).
