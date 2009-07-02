:- use_module(library(chr)).

% Null constraint:

:- chr_constraint
	sequence/1, sequence/2, sequence_length/1,
	trans/3, emit/3,
	time/1, state/2, reachable/3, input/2, count/4, total/3,
	countphase/1, expandphase/1, normphase/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialize time constraints from sequence
sequence(S) <=> length(S,L) | sequence_length(L), sequence(1,S).
sequence(_,[]) <=> true. % Recursion termination
sequence(Time,[Elem|R]) <=> % Add an input constraint for each symbol
    NextTime is Time + 1 | input(Time,Elem), sequence(NextTime,R).

% Determine start state
state(X,Y) \ state(X,Y) <=> true. % dup. elim.
trans(X,_,_) ==> state(X, start). % X is a potential end state
trans(_,X,_) \ state(X, start) <=> true. % X cannot be a start state
state(_X,start), state(_Y,start) <=> false. % No multiple start-states

% init counts
trans(S1,S2,_P) ==> count(trans,S1,S2,0), count(transtotal,S1,0).
emit(S1,Symbol,_P) ==> count(emit,S1,Symbol,0), count(emittotal,S1,0).

% Start state is always reachable at time 0
state(State,start) ==> reachable(0,State,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expand phase:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Know when we are done:
expandphase(T), sequence_length(T) <=> normphase.

% Find all reachable next states (preserve duplicates).
expandphase(Time), reachable(Time, State, History), trans(State,NextState,_) ==>
    NextTime is Time + 1 | reachable(NextTime,NextState,[State|History]).

% Change to countphase
expandphase(Time) <=> countphase(Time).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The count phase:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

countphase(Time), input(Time,Symbol), reachable(Time,State,[PreviousState|Rest]) ==>
    count(trans,[PreviousState,State],1),
    count(emit, State, Symbol, 1).

countphase(Time) <=> NewTime is Time + 1 | expandphase(NewTime).

% Collect counts
count(T,S1,S2,1), count(T,S1,S2,PrevSum), total(T,S1,PrevTotal) <=>
    Sum is PrevSum + 1,
    Total is PrevTotal + 1
    |
    count(T,S1,S2,Sum),
    total(T,S2,Total).