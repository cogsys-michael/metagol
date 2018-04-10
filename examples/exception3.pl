:- use_module('../metagol').


%% tell metagol to use the BK
prim(even/1).

%% metarules
metarule([P, Q], ([P, A, _B]:-[[Q, A]])).
metarule([P, Q], ([P, _A, B]:-[[Q, B]])).
metarule([P, Q, R], ([P, A, B]:-[[Q, A], [R, B]])).

%% background knowledge
even(X) :- X mod 2 =:= 0.

metagol:max_fp_frac(0.50).

a :-
  Pos = [
    xor(1,2),
    xor(1,4),
    xor(2,3),
    xor(2,5),
    xor(3,2),
    xor(3,4),
    xor(3,18)
    ],
  Neg = [
    xor(1,1),
    xor(1,3),
    xor(1,5),
    xor(2,2),
    xor(2,4),
    xor(4,8),
    xor(3,7)
    ],
  metagol_sn(Pos,Neg,3,_,Progs),
  pprint(Progs).
