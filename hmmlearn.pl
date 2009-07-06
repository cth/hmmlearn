:- use_module(library(chr)).

%:- set_prolog_flag(chr_toplevel_show_store,false).

:- chr_constraint
	sequence/1, sequence/2, sequence_length/1,
	trans/3, emit/3,
	history_size/1, cleanup/1, 
	time/1, nexttime/1, state/2, reachable/4, input/2, count/4, total/3,
	countphase/0, expandphase/0, normphase/0,
	init/0.

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
trans(_,X,_) ==> state(X, end).	 % X is an potential end state
trans(X,_,_) \ state(X, end) <=> true. % X cannot be an end state

% init counts
trans(S1,S2,_P) ==> count(trans,S1,S2,0), total(trans,S1,0).
emit(S1,Symbol,_P) ==> count(emit,S1,Symbol,0), total(emit,S1,0).

% Start state is always reachable at time 0
init,state(State,start) <=>
    reachable(0,State,[],1),
    time(0),
    nexttime(0),
    expandphase.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Store management and incremental cleanup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prune_history(_,0,[]).
prune_history([],_,[]).
prune_history([H1|R1],MaxSize,[H1|R2]) :-
	NewMaxSize is MaxSize - 1,
	prune_history(R1,NewMaxSize,R2).

time(Time), history_size(HS) ==>
    Old is Time - HS,
%    write(cleanup(Old)),nl,    
    HS > 0
    |
    cleanup(Old).

cleanup(Time) \ reachable(Time,_State,_History,_Count) <=>
%    write(reachable(Time,State,History,Count)),nl,
    true.
cleanup(_) <=> true.

% Remove premature end transitions
sequence_length(EndTime), state(S,end) \ reachable(Time,S,_,_) <=>
    Time \= EndTime
    |
    true.

history_size(HS) \ reachable(Time,State,History,Count) <=>
    length(History,HistLen),
    HistLen > HS
    |
    prune_history(History,HS,PrunedHist),
    reachable(Time,State,PrunedHist,Count).

reachable(Time,State,History,C1), reachable(Time,State,History,C2) <=>
    C is C1 + C2,
    reachable(Time,State,History,C).

time(T) \ nexttime(T) <=>
    NextTime is T + 1,
    nexttime(NextTime).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expand phase:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Find all reachable next states (preserve duplicates).
expandphase, time(Time), nexttime(NextTime),
reachable(Time, State, History,Count),
trans(State,NextState,_), emit(NextState,Symbol,_),
input(NextTime,Symbol) ==>
    write(reachable(NextTime,NextState,[State|History],Count)),nl
    |
    reachable(NextTime,NextState,[State|History],Count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The count phase:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Collect counts
/*
count(T,S1,S2,1), count(T,S1,S2,PrevSum), total(T,S1,PrevTotal) <=>
    Sum is PrevSum + 1,
    Total is PrevTotal + 1
    |
    count(T,S1,S2,Sum),
    total(T,S2,Total).
*/
count(T,S1,S2,C1), count(T,S1,S2,C2) <=>
    C is C1 + C2
    |
    count(T,S1,S2,C).

%countphase, time(T), input(T,_) ==> write('countphase: '), write(time(T)), nl, true.

countphase, time(T), input(T,Symbol), reachable(T,State,[PrevState|R],Count) ==>
    write('counting: '),write(reachable(T,State,[PrevState|R],Count)),nl,
    count(trans,PrevState,State,Count),
    count(emit, State, Symbol, Count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Phase control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Know when we are done:
time(T), countphase, sequence_length(SL) <=>
    T >= SL |
    write(normphase(T)),nl,
    normphase.

sequence_length(SL) \ time(T), expandphase <=>
%    sleep(1),
    T < SL,
    NewT is T + 1
    |
    time(NewT),
    write(countphase(NewT)),nl,
    countphase.

time(T), sequence_length(SL) \ countphase <=>
%    sleep(1),
    T < SL
    |
%    time(NewT),
    write(expandphase(T)),nl,
    expandphase.