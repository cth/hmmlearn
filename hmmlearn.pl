:- use_module(library(chr)).

%:- set_prolog_flag(chr_toplevel_show_store,false).

:- chr_constraint
	sequence/1, sequence/2, sequence_length/1,
	trans/3, emit/3,
	derived_trans/3, derived_emit/3,
	history_size/1, emission_history/2, cleanup/1, 
	time/1, nexttime/1,
	state/2, state_order/2,
	path/5, input/2, count/4, total/3,
	countphase/0, expandphase/0, normphase/0, totalphase/0,
	init/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Side constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_path_constraints(NextState,
		       [State|History],
		       EmissionHistory,
		       Constraints,
		       Constraints) :-
	write(check_path_constraints(NextState,
	    [State|History],EmissionHistory,Constraints)),
	nl.

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

% Start state is always path at time 0
init,state(State,start) <=>
    path(0,State,[],[],1),
    emission_history(0,[]),
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
    HS > 0
    |
    cleanup(Old).

cleanup(Time) \ path(Time,_State,_History,_Constraints,_Count) <=> true.
cleanup(_) <=> true.

% Remove premature end transitions
sequence_length(EndTime), state(S,end) \ path(Time,S,_,_,_) <=>
    Time \= EndTime
    |
    true.

% Keep State history of paths to a windows of history size
history_size(HS) \ path(Time,State,StateHist,Constraints,Count) <=>
    length(StateHist,StateHistLen),
    StateHistLen > HS
    |
    prune_history(StateHist,HS,PrunedStateHist),
    path(Time,State,PrunedStateHist,Constraints,Count).

% Maintain a current window of the emission history
history_size(HS) \ emission_history(T,EHS) <=>
    length(EHS,L),
    L > HS
    |
    prune_history(EHS,HS,PEHS),
    emission_history(T,PEHS).

% Sum multiple paths in the case they have identical history
path(Time,State,History,Constraints,C1), path(Time,State,History,Constraints,C2) <=>
    C is C1 + C2,
    path(Time,State,History,Constraints,C).

% Sum multiple paths in the case the state has limited (lower than usual) order
/*
state_order(State,Order),
   path(Time,State,Hist1,C1), path(Time,State,Hist2,C2) <=>
   prune_history(Hist1,Order,SharedHistory),
   prune_history(Hist2,Order,SharedHistory),
   C is C1 + C2,
   path(Time,State,SharedHistory,C).
*/

time(T) \ nexttime(T) <=> NextTime is T + 1, nexttime(NextTime).

time(T), input(T,S) \ emission_history(OldT,EHS) <=>
    OldT < T | emission_history(T, [S|EHS]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expand phase:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Find all path next states (preserve duplicates).
expandphase, time(Time), nexttime(NextTime),
emission_history(Time,EmissionHistory),
path(Time,State,History,Constraints,Count),
trans(State,NextState,_), emit(NextState,Symbol,_),
input(NextTime,Symbol) ==>
    check_path_constraints(NextState,[State|History],EmissionHistory,Constraints,UpdConstraints)
    |
    path(NextTime,NextState,[State|History],UpdConstraints,Count).


% Special case for end-state
/*
expandphase, time(Time), nexttime(NextTime),
emission_history(Time,EmissionHistory),
path(Time,State,History,Constraints,Count),
trans(State,NextState,_), state(NextState,End),
sequence_length(NextTime) ==>
    check_path_constraints(NextState,[State|History],EmissionHistory,Constraints,UpdConstraints)
    |
    path(NextTime,NextState,[State|History],UpdConstraints,Count).
  */

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
    C is C1 + C2,
    count(T,S1,S2,C).

countphase, time(T), input(T,Symbol), path(T,State,[PrevState|R],Constraints,Count) ==>
    write('counting: '),write(path(T,State,[PrevState|R],Constraints,Count)),nl,
    count(trans,PrevState,State,Count),
    count(emit, State, Symbol, Count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Normalization phase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Count totals
totalphase, trans(S,_,_) ==> total(trans,S,0).
totalphase, emit(S,_,_) ==> total(emit,S,0).

totalphase, count(Type,Origin,_,Count) ==>
    total(Type,Origin,Count).

totalphase \ total(Type,Origin,C1), total(Type,Origin,C2) <=>
    Count is C1 + C2,
    total(Type,Origin,Count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Normalization phase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


normphase, total(trans,S1,Total), trans(S1,S2,_), count(trans,S1,S2,Count) ==>
    write('calc trans probs'),nl,
    Count > 0,
    Total > 0
    |
    Probability is Count / Total,
    derived_trans(S1,S2,Probability).


normphase, total(emit,State,Total), emit(State,Sym,_), count(emit,State,Sym,Count) ==>
    Count > 0,
    Total > 0
    |
    Probability is Count / Total,
    derived_emit(State,Sym,Probability).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Phase control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


totalphase <=> write('entering norm phase'), nl, normphase.

% Know when we are done:
time(T), countphase, sequence_length(SL) <=>
    T >= SL |
    write(normphase(T)),nl,
    totalphase.

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