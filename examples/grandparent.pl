

:- use_module('../ilp.pl').


% knowledge
mother(ann,amy).
mother(ann,andy).
mother(amy,amelia).
mother(linda,gavin).
father(steve,amy).
father(steve,andy).
father(gavin,amelia).
father(andy,spongebob).

% positive examples
grandparent(ann,amelia) :- true.
grandparent(steve,amelia) :- true.
grandparent(ann,spongebob) :- true.
grandparent(steve,spongebob) :- true.
%grandparent(linda,amelia) :- true.
    
% negative examples
grandparent(amy,amelia) :- false.


%:-  learn(grandparent/2,[mother/2,father/2],[clause(Clause),statistics(Statistics)]),
%    portray_clause(Clause),
%    writeln(Statistics).
/*
grandparent(A, B) :-
    (   (   (   father(C, B),
                father(A, C)
            ;   mother(A, D),
                father(D, B)
            )
        ;   mother(E, B),
            father(A, E)
        )
    ;   mother(F, B),
        mother(A, F)
    ).
*/
% Statistics: [precision(1),recall(1),specificity(1),accuracy(1),tp(4),tn(1),fp(0),fn(0),terms(1/2),negations(0),variables(6/2),singletons(0)]