
:- use_module('../ilp.pl').


odd(1) :- true.
odd(2) :- false.


even(X) :- \odd(X). % this is not to be learnt (it's already general)


divisible_by_2(X) :- 0 is X mod 2. 

% ?- learn(odd/1,[divisible_by_2/1],[clause(Clause)]).
/*
odd(A) :-
    \divisible_by_2(A).
*/

has_remainder(X) :- 1 is X mod 2.

% ?- learn(odd/1,[has_remainder/1],[clause(Clause)]).
/*
odd(A) :-
    has_remainder(A).
*/


parity(1,odd) :- true.
parity(2,odd) :- false.

% ?- learn(parity/2,[divisible_by_2/1],[clause(Clause)]).
%       false