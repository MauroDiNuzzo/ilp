
% from: https://arxiv.org/abs/2008.07912

:- use_module('../ilp.pl').

% Note: declare the Knowledge predicates as dynamic if Knoweldge is going to change.
:- dynamic lego_builder/1.
:- dynamic estate_agent/1.
:- dynamic enjoys_lego/1.


% Knowledge
lego_builder(alice).
lego_builder(bob).

estate_agent(claire).
estate_agent(dave).

enjoys_lego(alice).
enjoys_lego(claire).


% Positive examples
happy(alice) :- true.

% Negative examples
happy(bob) :- false.
happy(claire) :- false.
happy(dave) :- false.


% ?- learn(happy/1,[lego_builder/1,estate_agent/1,enjoys_lego/1]).
/*
happy(A) :-
    lego_builder(A),
    no(estate_agent(A)),
    enjoys_lego(A).
*/
