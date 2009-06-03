:- use_module(library(chr)).
:- chr_constraint state/2, trans/3, reachable/2.

tst :- 
       trans(start, s1, 0.5),  trans(start, s2, 0.49),  trans(start, end, 0.01),
       trans(   s1, s1, 0.6),  trans(   s1, s2, 0.3 ),  trans(   s1, end, 0.1),
       trans(   s2, s1, 0.3),  trans(   s2, s2, 0.6 ),  trans(   s2, end, 0.1).

state(X,Y) \ state(X,Y) <=> true. % dup. elim.

% Determine start state: A state with no transition to it.
trans(X,_,_) ==> state(X, start). % X is a potential end state
trans(_,X,_) \ state(X, start) <=> true. % X cannot be a start state
state(_X,start), state(_Y,start) <=> false. % Multiple start-states not permitted

% Determine end state: A state with no transition from it. Mulitple end states is allowed.
trans(_,X, _) ==> state(X,end). % X is a potential end state
trans(X,_,_) \ state(X,end) <=> true. % X cannot be an end state

state(State,start) ==> reachable(0,State). % Start state is always reachable at time 0