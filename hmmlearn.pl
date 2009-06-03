:- use_module(library(chr)).

:- chr_constraint
	sequence/1, sequence/2, sequence_length/1,
	init_counts/0, init_done/0,
	trans_count/3, trans_total/2,
	emit_count/3, emit_total/2,
	normalize/0, time/1, state/2, reachable/2, input/2,
	trans/3, emit/3.

% Initialize time constraints from sequence
sequence(S) <=> length(S,L) | sequence_length(L), sequence(1,S). % Add Time counter
sequence(_,[]) <=> true. % Recursion termination
sequence(Time,[Elem|R]) <=> % Add an input constraint for each symbol
    NextTime is Time + 1
    |
    input(Time,Elem),
    sequence(NextTime,R).

%%%% Initialization of transition and emission counter constraints
trans_total(St,0) \ trans_total(St,0) <=> true. % dup. elim.
emit_total(St,0) \ emit_total(St,0) <=> true. % dup. elim.
init_counts, trans(S1,S2,_P) ==> trans_count(S1,S2,0), trans_total(S1,0).
init_counts, emit(S1,Symbol,_P) ==> emit_count(S1,Symbol,0), emit_total(S1,0).
init_counts <=> time(0). % Initialization done -> begin simulation...

%%%% Determine start and end states %%%%
state(X,Y) \ state(X,Y) <=> true. % dup. elim.
trans(X,_,_) ==> state(X, start). % X is a potential end state
trans(_,X,_) \ state(X, start) <=> true. % X cannot be a start state
state(_X,start), state(_Y,start) <=> false. % Multiple start-states not permitted
trans(_,X, _) ==> state(X,end). % X is a potential end state
trans(X,_,_) \ state(X,end) <=> true. % X cannot be an end state


elim_dup_time @ time(X) \ time(X) <=> true. % dup. elim.

state(State,start) ==> reachable(0,State). % Start state is always reachable at time 0

% Find all reachable next states (there may be duplicates, which is intensional).
time(Time), reachable(Time, State), trans(State,NextState,_) ==>
     NextTime is Time + 1 | reachable(NextTime,NextState).

% For each state reachable from the start state, add a transition count (but no emit count!).
start_state_transitions @
state(State,start), trans(State,NextState,_) \
reachable(1, NextState), trans_count(State,NextState,TransCount), trans_total(State,TotalTransCount) <=>
     NextTransCount is TransCount + 1,
     NextTotalTransCount is TotalTransCount + 1
     |
     trans_count(State,NextState,NextTransCount),
     trans_total(State, NextTotalTransCount).

% The main counting rule. Each possible state transition that will result in emission 
% corresponding to input at given time, as well as the emission it self are counted.
% Note, this rule does not follow transitions to the end state
count_transitions_and_emissions @
time(Time), input(Time,Symbol),
trans(State,NextState,_), trans(NextState, _,_) \ % Make sure it not a dead-end (end-state)
reachable(Time,State), % Eat one reachable constraint
trans_count(State,NextState,TransCount), trans_total(State,TotalTransCount),
emit_count(State, Symbol,EmitCount), emit_total(State,TotalEmitCount) <=>
     NextTime is Time + 1,
     NextTransCount is TransCount + 1,
     NextEmitCount is EmitCount + 1,
     NextTotalTransCount is TotalTransCount + 1,
     NextTotalEmitCount is TotalEmitCount + 1,
     write('recursion'), nl
     |
     state(NextTime,NextState),
     emit_count(State,Symbol,NextEmitCount),
     emit_total(State, NextTotalEmitCount),
     trans_count(State,NextState,NextTransCount),
     trans_total(State, NextTotalTransCount).

% Transition to end states or accepting states (no emission)
% If it is not handled by previous rule, it must be end-state or unreachable by constraint:
% This rules handles the first case.
end_state_transitions @
time(Time), state(Time,State), trans(State,EndState,_), sequence_length(Time), state(EndState,end) \
reachable(Time, State), trans_count(State, EndState, _), trans_total(State,TotalTransCount) <=>
     NextTotalTransCount is TotalTransCount + 1
     |
     trans_count(State, EndState, 1), trans_total(State, NextTotalTransCount).

% If a constraint prohibits a certain transition, then we have too many reachable constraints
remove_unreachable @ time(Time) \ reachable(Time,_) <=> true.

increase_time_step @
time(T), input(T,_) <=> T1 is T + 1 | write('increase time'),nl,time(T1).

enter_normalization_phase @
time(T), sequence_length(T) <=> normalize.

% Remove zero probability transitions
normalize \ trans_count(_,_,0) <=> true.
normalize \ trans_total(_,0) <=> true.
% Remove zero probability emission
normalize \ emit_count(_,_,0) <=> true.
normalize \ emit_total(_,0) <=> true.

normalize, trans_total(S1,CT) \ trans(S1,S2,_OldProb), trans_count(S1,S2,C) <=>
    Probability is C / CT
    |
    trans(S1,S2,Probability).

normalize, emit_total(State,CT) \ emit(State,Symbol,_OldProb), emit_count(State,Symbol,C) <=>
    Probability is C / CT
    |
    emit(State,Symbol,Probability).