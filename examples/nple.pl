
:- use_module('../ilp.pl').


% Knowledge
:- use_module(library(clpfd)).
mult(X,Y,R) :- R #= X*Y.


% Positive examples:
nple(3,1) :- true.
nple(60,20) :- true.

% Negative examples:
nple(10,5) :- false.


% Query
%   ?- nple(6,2).
%       false
%   ?- learn(nple/2,[mult/3]).
%   ?- nple(6,2).
%       true   



