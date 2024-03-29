% ilp.pl
% Last modified: 3/29/2024 by Mauro DiNuzzo


:- module(ilp, [
        learn/2,    
        learn/3,
        unlearn/1,
        (\)/1 % note: this is already an operator (200,fy) in Prolog
    ]).


:- use_module(library(debug)).
%:- debug(ilp).

:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(dialect/hprolog)).

:- use_module(library(listing)).
:- use_module(library(ansi_term)).

:- use_module(library(error)).

:- dynamic error:has_type/2.
:- multifile error:has_type/2.

error:has_type(predicate_indicator,Functor/Arity) :-
	atom(Functor),
	integer(Arity),
	Arity >= 0.


:- dynamic '$Lpe'/2. % Predicate,Examples    


%% unlearn/1
% unlearn(+Predicate)
%
:- module_transparent unlearn/1.

unlearn(Predicate) :-
    must_be(predicate_indicator,Predicate),
    (   ilp:'$Lpe'(Predicate,Examples)
    ->  dynamic(Predicate),
        Predicate = Functor/Arity,
        functor(Head,Functor,Arity),
        retractall(Head),
        prolog_load_context(module,Module),
        unlearn_(Module,Examples),
        retractall(ilp:'$Lpe'(Predicate,_))
    ;   throw(error(existence_error(predicate,Predicate),_))
    ).

unlearn_(_,[]) :- !.
unlearn_(Module,[Head|Tail]) :-
    assertz(Module:Head),
    unlearn_(Module,Tail).


%% (\)/1
% \(+Goal)
%
% Delay the execution of not(Goal) until all variables in Goal are bound.
% We need a negation predicate that behaves as not/1, i.e., (\+)/1, yet allows for variable bindings during saturation.
%
% example:
% |?- assert(p(a)).
% |?- not(p(a)).
%       false
% |?- not(p(X)).
%       false
% But in the latter case we are asking if there's any X such that p(X) fail, so we want success here.
% |?- \p(X).
%       true
:- module_transparent (\)/1.

\(Goal) :- when(ground(Goal),not(Goal)).


%% learn/[2,3]
% learn(+Predicate,+Knowledge)
% learn(+Predicate,+Knowledge,+Options)
%
% Options:
%   - assert(+bool)
%   - clause(-Clause)
%   - statistics(-Statistics)
:- module_transparent learn/2, learn/3.

learn(Predicate,Knowledge) :-
    learn(Predicate,Knowledge,[]).
learn(Predicate,Knowledge,Options) :-
    must_be(predicate_indicator,Predicate),
    must_be(list(predicate_indicator),Knowledge),
    must_be(list(compound),Options),  
    option(assert(Assert),Options,true),
    must_be(boolean,Assert),
    (   ilp:'$Lpe'(Predicate,_)
    ->  throw(error(permission_error(learn,predicate,Predicate),_))
    ;   true
    ),
    Predicate = Functor/Arity,
    functor(Head,Functor,Arity),
    findall((Head :- Body),clause(Head,Body),Examples),
    learn(Predicate,Knowledge,Examples,Clause,Statistics),
    ignore(memberchk(statistics(Statistics),Options)), % bind output Statistics if requested
    ignore(memberchk(clause(Clause),Options)), % bind output Clause if requested  
    (   debugging(ilp) 
    ->  with_output_to(atom(Atom),portray_clause(Clause)),
        ansi_format(fg(blue),'/*\n',[]),
        ansi_format(fg(blue),Atom,[]),
        ansi_format(fg(blue),'*/',[])
    ;   true
    ),
    debug(ilp,'Statistics: ~q',[Statistics]),
    (   Assert
    ->  dynamic(Predicate),
        retractall(Head),
        assert(Clause),
        assert(ilp:'$Lpe'(Predicate,Examples))
    ;   true
    ).


%% combine_clauses/5
% combine_clauses(+ListOfClauses,+Knowledge,+Examples,-Clause,-Statistics)
%
combine_clauses([InitialCandidate|Tail],Knowledge,Examples,Clause,Statistics) :-
    stats(Knowledge,Examples,InitialCandidate,InitialStatistics), 
    score(disjunction,InitialStatistics,InitialScore),    
    combine_clauses_(Tail,InitialCandidate,InitialScore,InitialStatistics,Knowledge,Examples,Clause,Statistics).

combine_clauses_([],Clause,_,Statistics,_,_,Clause,Statistics) :- !.
combine_clauses_([Candidate|Candidates],PreviousCandidate,PreviousScore,PreviousStatistics,Knowledge,Examples,Clause,Statistics) :-
    PreviousCandidate = (Head :- PreviousBody),
    Candidate = (Head :- Body), % unify head
    CurrentCandidate = (Head :- (PreviousBody) ; (Body)), % disjunction
    %debug(ilp,'Current candidate: ~q',[CurrentCandidate]),    
    stats(Knowledge,Examples,CurrentCandidate,CurrentStatistics),  
    score(disjunction,CurrentStatistics,CurrentScore),       
    (   CurrentScore > PreviousScore
    ->  combine_clauses_(Candidates,CurrentCandidate,CurrentScore,CurrentStatistics,Knowledge,Examples,Clause,Statistics) 
    ;   CurrentScore = PreviousScore
    ->  combine_clauses_(Candidates,PreviousCandidate,PreviousScore,PreviousStatistics,Knowledge,Examples,Clause,Statistics)
    ;   Clause = PreviousCandidate,
        Statistics = PreviousStatistics
    ).


%% learn/5
% learn(+Predicate,+Knowledge,+Examples,-Clause,-Statistics)
%
learn(Predicate,Knowledge,Examples,Clause,Statistics) :-
    member(Method,[bh,hb]),
    findall(Instance,learning(Method,Predicate,Knowledge,Examples,Instance),KeyValues),    
    KeyValues \= [],
    keysort(KeyValues,Sorted),
    reverse(Sorted,Ordered),
    %debug(ilp,'Clauses (sorted/desc): ~q',[Ordered]),    
    pairs_values(Ordered,List),
    list_to_set(List,Set),
    combine_clauses(Set,Knowledge,Examples,Clause,Statistics), 
    !.


%% learning/5
% learning(+Method,+Predicate,+Knowledge,+Examples,-Instance)
%
learning(Method,Predicate,Knowledge,Examples,Instance) :-
    generate_clause(Method,Predicate,Knowledge,Examples,Generated,_Order),  
    remove_duplicate_terms(Generated,Clause),  
    stats(Knowledge,Examples,Clause,Statistics),
    score(clause,Statistics,Score),
    %debug(ilp,'Clause (score ~w): ~q',[Score,Clause]),
    Score > 0,
    %debug(ilp,'Clause PASSED (score ~w): ~q',[Score,Clause]),
    %debug(ilp,'Statistics: ~q\n',[Statistics]),
    Instance = (Score-Clause). % keyvalue


%% score/3
% score(+Level,+Statistics,-Score)
%
score(clause,Statistics,Score) :-
    memberchk(precision(Precision),Statistics),
    memberchk(recall(Recall),Statistics),
    (Precision == 0, Recall == 0 -> F is 0 ; F is 2*Precision*Recall/(Precision+Recall)),  
    memberchk(terms(Terms/Knowledge),Statistics),
    Distance1 is (Terms/Knowledge-1)^2, % do we use too many or too few terms (wrt knowledge)?
    memberchk(variables(Variables/Arity),Statistics),
    Distance2 is (Variables/Arity-1)^2, % do we use too many or too few variables (wrt arity)?
    memberchk(negations(Negations),Statistics),
    memberchk(singletons(Singletons),Statistics),  
    % ATTENTION: Weighting is empirical (TODO: estimate dynamically)
    Score is F/(1+Distance1)^10.0/(1+Distance2)^1.0/(1+Negations)^1.5/(1+Singletons). % F-score weighted with penalty
score(disjunction,Statistics,Score) :-
    memberchk(precision(Precision),Statistics),
    memberchk(recall(Recall),Statistics),
    (Precision == 0, Recall == 0 -> F is 0 ; F is 2*Precision*Recall/(Precision+Recall)),  
    memberchk(singletons(Singletons),Statistics),    
    Score is F/(1+Singletons). % F-score with penalty on singletons    


%% stats/4
% stats(+Knowledge,+Examples,+Clause,-Stats)
%
stats(Knowledge,Examples,(Head :- Body),[
        precision(Precision), 
        recall(Recall), % Sensitivity
        specificity(Specificity),
        accuracy(Accuracy),
        tp(TP), 
        tn(TN), 
        fp(FP), 
        fn(FN),
        terms(NumTerms/Length),
        negations(NumNegations),
        variables(NumVariables/Arity),
        singletons(NumSingletons)
    ]) :-
    length(Knowledge,Length),
    functor(Head,_,Arity),
    term_variables([Head,Body],Variables),
    length(Variables,NumVariables),    
    term_singletons([Head,Body],Singletons),
    length(Singletons,NumSingletons),    
    terms_count(Body,NumTerms),
    negations_count(Body,NumNegations),
    stats_(Examples,Head,Body,0,0,0,0,TP,TN,FP,FN),
    Accuracy is (TP+TN)/(TP+TN+FP+FN),
    (DSP is TN+FP, DSP > 0 -> Specificity is TN/DSP ; Specificity is 0),
    (DP is TP+FP, DP > 0 -> Precision is TP/DP ; Precision is 0),
    (DR is TP+FN, DR > 0 -> Recall is TP/DR ; Recall is 0).
stats_([],_,_,TP,TN,FP,FN,TP,TN,FP,FN) :- !.
stats_([(Candidate :- Example)|Examples],Head,Body,CTP,CTN,CFP,CFN,TP,TN,FP,FN) :-
    copy_term([Head,Body],[HeadCopy,BodyCopy]),
    (   HeadCopy = Candidate, 
        call(BodyCopy)
    ->  (   call(Example)
        ->  (NewCTP is CTP+1, NewCTN is CTN, NewCFP is CFP, NewCFN is CFN) % TP
        ;   (NewCTP is CTP, NewCTN is CTN, NewCFP is CFP+1, NewCFN is CFN) % FP
        )
    ;   (   call(Example)
        ->  (NewCTP is CTP, NewCTN is CTN, NewCFP is CFP, NewCFN is CFN+1) % FN
        ;   (NewCTP is CTP, NewCTN is CTN+1, NewCFP is CFP, NewCFN is CFN) % TN
        )        
    ),
    stats_(Examples,Head,Body,NewCTP,NewCTN,NewCFP,NewCFN,TP,TN,FP,FN).


%% negations_count/2
% negations_count(+Terms,-Count)
%
negations_count(Terms,Count ):-
    terms_to_list(Terms,List),
    negations_count_(List,0,Count).

negations_count_([],Count,Count) :- !.
negations_count_([\(_)|Tail],Current,Count) :- !,
    Next is Current+1,
    negations_count_(Tail,Next,Count).
negations_count_([_|Tail],Current,Count) :- 
    negations_count_(Tail,Current,Count).    


%% terms_count/2
% terms_count(+Terms,-Count)
%
% Count unique terms.
terms_count(Terms,Count) :-
    terms_to_list(Terms,List),
    terms_count_(List,0,Count).

terms_count_([],Count,Count) :- !.
terms_count_([Head|Tail],Current,Count) :-
    \+ umemberchk(Head,Tail), !,
    Next is Current+1,
    terms_count_(Tail,Next,Count).
terms_count_([_|Tail],Current,Count) :-
    terms_count_(Tail,Current,Count).

terms_to_list((Term,Terms),[Term|List]) :- !,
    terms_to_list(Terms,List).
terms_to_list((Term),[Term]).

umemberchk(Element,[Head|_]) :-
    Element == Head, !.
umemberchk(Element,[_|Tail]) :-
    umemberchk(Element,Tail).


%% remove_duplicate_terms/2
% remove_duplicate_terms(+Terms,-Clause)
remove_duplicate_terms((Head :- Terms),(Head :- Clause)) :-
    terms_to_list(Terms,List),
    uset(List,Set),
    list_to_terms(Set,Clause).

uset([],[]) :- !.
uset([Head|Tail],[Head|Set]) :-
    \+ umemberchk(Head,Tail), !,
    uset(Tail,Set).
uset([_|Tail],Set) :-
    uset(Tail,Set).

list_to_terms([Head],(Head)) :- !.
list_to_terms([Head|Tail],(Head,Terms)) :-
    list_to_terms(Tail,Terms).


%% generate_clause/6
% generate_clause(+Method,+Predicate,+Knowledge,+Examples,-Clause,-Order)
%
generate_clause(bh,Functor/Arity,Knowledge,_,Clause,Order) :-
    % Bottom-up
    % Generate hypothesis (body), prove/unify it, and then bind head variables.
    generate_hypothesis(Knowledge,Body,Order),
    copy_term(Body,Copy), % store for later        
    term_variables(Copy,Grounds), % not yet grounds but soon          
    catch(Copy,_,fail), % hopefully now grounds       
    term_variables(Body,Variables),       
    reduce_variables(Variables,Grounds), % now we have body variables to arrange in head        
    % time to bind
    functor(Head,Functor,Arity),          
    permutation_length(Variables,Combination,Arity),       
    term_variables(Head,Combination), % bindings here     
    handle_singletons_bh(Head,Body),  
    Clause = (Head :- Body).
generate_clause(hb,Functor/Arity,Knowledge,Examples,Clause,Order) :-
    % Top-down
    % Generate hypothesis (body), link to head variables, and then prove/unify body.
    generate_hypothesis(Knowledge,Body,Order),
    copy_term(Body,Copy), % store for later  
    term_variables(Copy,Grounds), % not grounds yet
    functor(Head,Functor,Arity),    
    copy_term(Head,HCopy),         
    permutation_length(Grounds,Combination1,Arity),          
    term_variables(Head,Combination1),              
    select((Head :- _),Examples,_), % link    
    catch(Copy,_,fail), % hopefully now grounds    
    term_variables(Body,Variables),       
    reduce_variables(Variables,Grounds),        
    handle_singletons_hb(Variables,Grounds),     
    term_variables(Variables,Unbound), 
    permutation_length(Unbound,Combination2,Arity),       
    term_variables(HCopy,Combination2), % binding     
    Clause = (HCopy :- Body).    
%generate_clause(...) % TODO: add other more-sophisticated/exhaustive methods.


%% handle_singletons_bh/2
% handle_singletons_bh(?Head,?Body)
%
handle_singletons_bh(Head,Body) :-
    term_variables(Head,Variables),
    term_singletons([Head,Body],Singletons),
    %debug(ilp,'Singletons: ~q',[Singletons]),
    length(Singletons,Length),
    combination_length(Variables,Combination,Length),
    Combination = Singletons.
handle_singletons_bh(_,_).


%% handle_singletons_hb/2
% handle_singletons_hb(+Variables,+Grounds)
%
handle_singletons_hb(Variables,Grounds) :-
    pairs_keys_values(Pairs,Grounds,Variables),
    term_singletons(Variables,Singletons),
    handle_singletons_hb_(Pairs,Singletons).

handle_singletons_hb_([],_) :- !.
handle_singletons_hb_([Ground-Variable|Tail],Singletons) :-
    var(Variable),
    umemberchk(Variable,Singletons), 
    Variable = Ground,
    handle_singletons_hb_(Tail,Singletons).
handle_singletons_hb_([_|Tail],Singletons) :-
    handle_singletons_hb_(Tail,Singletons).


%% reduce_variables/2
% reduce_variables(+Variables,+Grounds)
%
% example:
% |?- reduce_variables([A,B,C,D,E,F],[1,2,2,3,2,1]).
%       A = F,
%       B = C, C = E
reduce_variables(Variables,Grounds) :-
    pairs_keys_values(Pairs,Grounds,Variables),
    reduce_variables_(Pairs).

reduce_variables_([]) :- !.
reduce_variables_([Head|Tail]) :-
    member(Head,Tail), !,
    reduce_variables_(Tail).
reduce_variables_([_|Tail]) :-
    reduce_variables_(Tail).


%% generate_hypothesis/3
% generate_hypothesis(+Knowledge,-Hypothesis,-Order)
%
generate_hypothesis(Knowledge,Hypothesis,Order) :- % good compromise
    length(Knowledge,Order),
    combination_length(Knowledge,Combination,Order),
    knowledge_hypothesis(Combination,Hypothesis).  
/*
generate_hypothesis(Knowledge,Hypothesis,Order) :- % fast but limited
    length(Knowledge,Order),
    knowledge_hypothesis(Knowledge,Hypothesis).  
*/
/*
generate_hypothesis(Knowledge,Hypothesis,Order) :- % exhaustive but very slow
    var(Order),
    between(1,inf,Order),
    combination_length(Knowledge,Combination,Order),
    knowledge_hypothesis(Combination,Hypothesis).
*/


%% combination_length/3
% combination_length(+List,-Combination,+Length)
%
% |?- combination_length([a,b],C,3).
%       C = [a, a, a] ;
%       C = [a, a, b] ;
%       C = [a, b, b] ;
%       C = [b, b, b] ;
%
combination_length(_,[],0).
combination_length([Head|Tail],[Head|List],Current) :- 
    Current > 0,
    Next is Current - 1,
    combination_length([Head|Tail],List,Next).
combination_length([_|Tail],List,Length) :- 
    Length > 0,
    combination_length(Tail,List,Length).


%% permutation_length/3
% permutation_length(+List,-Combination,+Length)
%
% |?- permutation_length([a,b],C,3).
%       false
% |?- permutation_length([a,b,c],C,2).
%       C = [a, b] ;
%       C = [a, c] ;
%       C = [b, a] ;
%       C = [b, c] ;
%       C = [c, a] ;
%       C = [c, b] ;
%
permutation_length(List,Combination,Length) :-
    permutation(List,Permutation),
    take(Length,Permutation,Combination). % truncate the permutation


%% disposition_length/3
% disposition_length(+List,-Combination,+Length)
%
% example:
% |?- disposition_length([a,b],C,3).
%       C = [a, a, a] ;
%       C = [a, a, b] ;
%       C = [a, b, a] ;
%       C = [a, b, b] ;
%       C = [b, a, a] ;
%       C = [b, a, b] ;
%       C = [b, b, a] ;
%       C = [b, b, b] ;
disposition_length(_,[],0) :- !.
disposition_length(L,[V|R],N) :-
    N > 0,
    N1 is N-1,
    unknown(V,L,_),
    disposition_length(L,R,N1).

unknown(X,[X|L],L).
unknown(X,[_|L],R) :- 
    unknown(X,L,R).


%% knowledge_hypothesis/2
% knowledge_hypothesis(+Knowledge,-Hypothesis)
%
% |?- knowledge_hypothesis([f/1,g/2],Hypothesis).
%       Hypothesis = (f(A),g(B,C)) ;
%       Hypothesis = (f(A),\(g(B,C))) ;
%       Hypothesis = (\(f(A)),g(B,C)) ;
%       Hypothesis = (\(f(A)),\(g(B,C))) ;
knowledge_hypothesis(Knowledge,Hypothesis) :-
    knowledge_list(Knowledge,ListOfLists),
    select_multiple(ListOfLists,List),
    list_to_term(List,Hypothesis).


%% list_to_term/2
% list_to_term(+List,-Term)
%
% |?- list_to_term([f(A),\(g(B,C))],Term).
%       Term = (f(A),\(g(B,C)))
list_to_term([Head],(Head)) :- !.
list_to_term([Head|Tail],(Head,Term)) :-
    list_to_term(Tail,Term).


%% knowledge_list/2
% knowledge_list(+Predicates,-ListOfLists)
%
% |?- knowledge_list([f/1,g/2],List).
%       List = [[f(A),\(f(A))],[g(B,C),\(g(B,C))]]
/*
knowledge_list([],[]) :- !.
knowledge_list([Functor/Arity|Predicates],[[Head]|List]) :-
    functor(Head,Functor,Arity),
    knowledge_list(Predicates,List).
*/
knowledge_list([],[]) :- !.
knowledge_list([Functor/Arity|Predicates],[[Head,\(Head)]|List]) :-
    functor(Head,Functor,Arity),
    knowledge_list(Predicates,List).


%% select_multiple/2
% select_multiple(+ListOfLists,-List)
%
% |?- select_multiple([[f(A),\(f(A))],[g(B,C),\(g(B,C))]],List).
%       List = [f(A),g(B,C)] ;
%       List = [f(A),\(g(B,C))] ;
%       List = [\(f(A)),g(B,C)] ;
%       List = [\(f(A)),\(g(B,C))] ;
%
% Note: also works with lists of different length.
select_multiple([],[]) :- !.
select_multiple([List|Lists],[Element|Elements]) :-
    select(Element,List,_),
    select_multiple(Lists,Elements).


