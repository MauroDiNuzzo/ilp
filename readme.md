# Inductive Logic Programming (ILP)

This lightweight library is currently just an excercise for experimenting in simplicity/readability of ILP while strictly remaining in standard Prolog.

If you're interested in contributing, please feel free to reach out on Linkedin (<https://www.linkedin.com/in/mauro-dinuzzo/>).

**DISCLAIMER**: USE FOR EDUCATIONAL PURPOSES ONLY.

## Features

The library includes:
- `learn/[2,3]` predicates that implement learning through the generate-and-test search method (both top-down and bottom-up).
- `unlearn/1` predicate that reverts to the state of the Prolog database as it was before learning.
- `(\)/1` predicate that implements "delayed" negation-by-failure using coroutining (i.e., attributed variables).

## Planned improvements

The currently implemented brute-force solution pruning should be enhanced with a more sophisticated strategy. The scoring system for the generated hypothesis is fixed and very empirical at this stage and should be dynamically determined.

## Limitations

The library has not been developed with performance in mind (everything is written in Prolog). There are many points where better choices could have been made to reduce CPU time. Furthermore, the present ILP implementation does not make use of mode declarations to prune the search space, and as a result the number of required inferences rapidly increases with problem complexity (number of examples, arity of the knewledge predicates, and so on). Finallt, auxiliary predicate invention is not implemented. The use of the library should therefore be restricted to small examples.

Please also note that the library has been developed and tested on SWI Prolog version 9.2.1 (<https://www.swi-prolog.org/>).
Compatibility with other Prolog systems/versions is not warranted.
Finally, this ILP implementation is substantially different from other more robust and comprehensive systems (for example, see the classical [Aleph](https://www.cs.ox.ac.uk/activities/programinduction/Aleph/) or the newer [Popper/Metagol](https://github.com/logic-and-learning-lab/Popper)). 

## Known issues

All variables appearing in the head of the predicate to learn must appear in the examples. For example, the examples:

```prolog
parity(1,odd) :- true.
parity(2,odd) :- false.
```

won't work as opposed to the examples:

```prolog
odd(1) :- true.
odd(2) :- false.
```

within the problem:

```prolog
divisible_by_2(X) :- 0 is X mod 2. 

% this works as expected (notice that \ means negation)
:- learn(odd/1,[divisible_by_2/1],[clause(Clause)]). 

   Clause =  (
      odd(A) :- 
         \divisible_by_2(A)
   ).

% this doesn't work as expected
:- learn(parity/2,[divisible_by_2/1],[clause(Clause)]). 
   false
```

Therefore, in the current ILP implementation it is important to carefully check how the problem is defined in terms of the variables of the predicate to be generalized. 

## Installation

Download the ILP files and unpack the archive in your favourite folder. From the SWI Prolog toplevel, import the `ilp` module predicates in the current namespace:

```prolog
:- use_module('path_to_folder/ilp.pl').
```

## Learning

ILP follows the following general approach:
- Select examples to be generalised
- Construct clauses that entails the selected examples (saturation)
- Find the most general clause with the best score (reduction)

Knowledge is declared using facts or rules. For example, let's consider the following facts for `mother/2` and `father/2` predicates:

```prolog
% Known facts
% mother(X,Y) means that X is the mother of Y
% father(X,Y) means that X is the father of Y
mother(ann,amy).
mother(ann,andy).
mother(amy,amelia).
mother(linda,gavin).
father(steve,amy).
father(steve,andy).
father(gavin,amelia).
father(andy,spongebob).
```

Now, suppose we have a `grandparent/2` predicate that we don't know how to formalize, but we do have positive and negative examples. These examples can be provided using `true/0` and `false/0` as follows:

```prolog
% grandparent(X,Y) means that X is the grandparent of Y

% Positive examples
grandparent(ann,amelia) :- true.
grandparent(steve,amelia) :- true.
grandparent(ann,spongebob) :- true.
grandparent(steve,spongebob) :- true.
    
% Negative examples
grandparent(amy,amelia) :- false.
```

We can now learn a generalization of the `grandparent/2` predicate, which is done by indicating the predicate to learn as well as the prior knowledge upon which to construct the generalization. We can also retrieve the generalized clause and the associated statistics using the corresponding options, as follows:

```prolog
:- learn(grandparent/2,[mother/2,father/2],
      [clause(Clause),statistics(Statistics)]).

   Clause = (
      grandparent(A,B) :- 
         mother(A,C),mother(C,B) ;
         mother(C,B),father(A,C) ;
         father(C,B),mother(A,C) ;
         father(A,C),father(C,B)
   ),

   Statistics = [
      precision(1),
      recall(1),
      specificity(1),
      accuracy(1),
      tp(4),tn(1),fp(0),fn(0),
      terms(1/2),
      negations(0),
      variables(6/2),
      singletons(0)
   ].
```

The ILP system correctly generalize the provided examples. At this point, we can obtain correct answers to questions that were not explicitly included in the examples, such as:

```prolog
:- grandparent(linda,amelia). % this would been false without learning
   true
```

Finally, notice that an error is raised if you attempt to learn the same predicate twice.

```prolog
:- learn(grandparent/2,[whatever]).
%ERROR: No permission to learn predicate `grandparent/2'
```

However, you can use the unlearn predicate to cancel the effects of learning:

```prolog
:- unlearn(grandparent/2).
:- grandparent(linda,amelia).
   false
```

Please see the Prolog source code of the SLP library for more detailed information on the usage of the relevant predicates.

## Other examples

Here I provide few examples with problems expressed both in natural language and in ILP. I have crafted problems for which several large language models (LLMs) fail to provide consistently correct responses.

### Example 1

Natural language:

> *Consider a relation named "nple". It is true that 3 is "nple" of 1. It is true that 60 is "nple" of 20. It is false that 10 is "nple" of 5. What is a definition of "nple" that only uses multiplication?*

ILP:

```prolog
% Knowledge
:- use_module(library(clpfd)).
mult(X,Y,R) :- R #= X*Y.

% Positive examples:
nple(3,1) :- true.
nple(60,20) :- true.

% Negative examples:
nple(10,5) :- false.
```

```prolog
% Queries:
:- nple(6,2). % before learning
   false

:- learn(nple/2,[mult/3]).
:- nple(6,2). % after learning
   true   
```

This is the generalized `nple/2` predicate after learning (notice that all the examples are abolished and replaced by the generalized predicate):

```prolog
nple(A,B) :-
    mult(3,B,A).
```

Since we used the CLP(FD) library to define the `mult/3` function, we can even do something like the following:

```prolog
:- nple(6,B).
   B = 2
:- nple(A,2).
   A = 6
```

### Example 2

Natural language:

> *Let's only consider the following 10 facts. (1) Alice is a Lego builder. (2) Bob is a Lego builder. (3) Claire is an estate agent. (4) Dave is an estate agent. (5) Alice enjoys Lego. (6) Claire enjoys Lego. (7) Alice is happy. (8) Bob is not happy. (9) Claire is not happy. (10) Dave is not happy. According to the 10 previous facts, identify a general rule that predicts what makes a person happy in terms of a combination of the following conditions: (a) being a Lego builder, (b) being an estate agent, and (c) enjoying Lego.*

ILP:

```prolog
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
```

```prolog
% Learn and report stats
:- learn(happy/1,
      [lego_builder/1,estate_agent/1,enjoys_lego/1],
      [clause(Clause),statistics(Statistics)]).

   Clause = (
      happy(A) :-
         lego_builder(A),
         \estate_agent(A),
         enjoys_lego(A)
   ),
   Statistics = [
      precision(1),
      recall(1),
      specificity(1),
      accuracy(1),
      tp(1),tn(3),fp(0),fn(0),
      terms(3/3),
      negations(1),
      variables(1/1),
      singletons(0)
   ]
```

Now, let's suppose that information about a new person is added to the database:

```prolog
:- assert(lego_builder(tom)).
:- assert(estate_agent(tom)).
:- assert(enjoys_lego(tom)).
```

Of course we have:

```prolog
:- happy(tom).
   false
```

However, a new situation (e.g., the person changes job) might affect things:

```prolog
:- retract(estate_agent(tom)).
:- happy(tom).
   true
```




## Further reading

Muggleton, S. (1991). Inductive logic programming. New Generation Computing, 8(4), 295-318.

Shapiro, E. Y. (1983). Algorithmic program debugging. Cambridge, MA: MIT Press.
