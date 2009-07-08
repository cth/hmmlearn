:- use_module(library(chr)).

%:- set_prolog_flag(chr_toplevel_show_store,false).

:- chr_constraint
	trans/3, emit/3, % Defining the structure of the HMM
	sequence/1,  % The training sequence
	sequence/2,sequence_length/1,input/2, % Derived from traing sequence
	outputfile/1, progress_indication/1,
	history_size/1, future_size/1,
	emission_history/2, emission_future/3,
	cleanup/1, 
	time/1, nexttime/1, 
	% derived from trans relations	
	state/2,
	% annotations about limited order of a state	
	state_order/2, 
	% Used to record partial paths to reachable states	
	path/5, reach_end/2, iteration_expansions/2,
        % Counters	
	count/4, total/3, 
	% Program phases:
	countphase/0, expandphase/0, normphase/0, totalphase/0, backwardsphase/0, extractionphase/0,
	% Final derived probabilities
	derived_trans/3, derived_emit/3,
	option/1, start_training/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Side constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Finds and checks all constraints for a path
check_path_constraints(NextTime, NextState,[State|History],
		       NextEmission,EmissionHistory,EmissionFuture,
		       Constraints,UpdConstraints) :-
%	write('check_constraints'),nl,
	user_constraints(ConstraintGoals),
	check_user_constraint(ConstraintGoals,NextTime,
			      NextState,[State|History],
			      NextEmission,EmissionHistory,EmissionFuture,
			      Constraints,UpdConstraints).

% Check individual constraint goals specified by user
check_user_constraint([],_,_,_,_,_,_,C,C).
check_user_constraint([GoalFunctor|R],NextTime, NextState,[State|History],
		      NextEmission,EmissionHistory,EmissionFuture,
		      Constraints,UpdConstraints) :-
	Goal =.. [GoalFunctor|[NextTime, NextState,[State|History],
		      NextEmission,EmissionHistory,EmissionFuture,
		      Constraints,ConstraintsOut]],
	call(Goal),
	check_user_constraint(R,
		      NextTime, NextState,[State|History],
		      NextEmission,EmissionHistory,EmissionFuture,
		      ConstraintsOut,UpdConstraints).

% Sample constraint that just prints the path
print_path(NextTime, NextState,[State|History],
	   NextEmission,EmissionHistory,EmissionFuture,
	   Constraints,Constraints) :-
	write(path(NextTime,NextState,[State|History],
		   NextEmission,EmissionHistory,EmissionFuture,
		   Constraints,Constraints)),
	nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pre-processing of input sequence and HMM 
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
trans(_,X,_) ==> state(X, end).	 % X is an potential end state
trans(X,_,_) \ state(X, end) <=> true. % X cannot be an end state

state(_X,start), state(_Y,start) <=>
   write('multiple start states are not allowed.'),
   nl,
   false.

state(X,end), emit(X,_,_) <=>
   write('explicit end states are not allowed to have emissions.'),
   nl,
   false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% start_training counts
trans(S1,S2,_P) ==> count(trans,S1,S2,0), total(trans,S1,0).
emit(S1,Symbol,_P) ==> count(emit,S1,Symbol,0), total(emit,S1,0).

% We have implicit end-states unless they are explicit:
start_training, state(_,end) ==> option(explicit_end_states).
start_training ==> option(implicit_end_states).
option(explicit_end_states) \ option(implicit_end_states) <=> true.

% Start state is always path at time 0
start_training, state(State,start) ==>
    path(0,State,[],[],1),
    emission_history(0,[]),
    time(0),
    nexttime(0),
%    write('backwardsphase'),nl,
    backwardsphase,
    write('done'),nl,
    expandphase.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Backwards tracing to see if states can reach end state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is sort of expensive (a lot of rule matches prevented
% by the guard. TODO: Optimize.

reach_end(Time,State) \ reach_end(Time,State) <=> true.

reach_end(Time,State), trans(PrevState,State,_),
emit(PrevState,Symbol,_), input(PrevTime,Symbol) ==>
   PrevTime is Time - 1,
   PrevTime > 0
   |
   reach_end(PrevTime,PrevState).

option(explicit_end_states),
state(S,end), trans(PS,S,_), emit(PS,Symbol,_),
sequence_length(SL), input(SL,Symbol) \ backwardsphase <=>
   reach_end(SL,PS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Store management and incremental cleanup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

time(T), progress_indication(PI) ==> 0 is T mod PI | write('.'), flush.

time(T), sequence_length(SL) ==>
	OnePercent is SL // 100,
	0 is T mod OnePercent,
	T > 0,
	PercentDone is T // OnePercent
	|
	write(PercentDone),write('%'),nl.

% Some tracking of the size of the constraint store
/*
time(T), sequence_length(SL), input(_,_) ==>
	OnePercent is SL // 100, 0 is T mod OnePercent, T > 0 | write('+').
	
time(T), sequence_length(SL), path(_,_,_,_,_) ==>
	OnePercent is SL // 100, 0 is T mod OnePercent, T > 0 | write('-').
	
time(T), sequence_length(SL), emission_history(_,_) ==>
	OnePercent is SL // 100, 0 is T mod OnePercent, T > 0 | write('*').

time(T), sequence_length(SL), emission_future(_,_,_) ==>
	OnePercent is SL // 100, 0 is T mod OnePercent, T > 0 | write('#').
	
time(T), sequence_length(SL), count(_,_,_,_) ==>
	OnePercent is SL // 100, 0 is T mod OnePercent, T > 0 | write('!').
*/
		
prune_history(_,0,[]).
prune_history([],_,[]).
prune_history([H1|R1],MaxSize,[H1|R2]) :-
	NewMaxSize is MaxSize - 1,
	prune_history(R1,NewMaxSize,R2).

time(Time), history_size(HS) ==> Old is Time - HS, HS > 0 | cleanup(Old).

cleanup(Time) \ path(Time,_State,_History,_Constraints,_Count) <=> true.
cleanup(_) <=> true.

% Remove premature end transitions
sequence_length(EndTime), state(S,end) \ path(Time,S,_,_,_) <=> Time =< EndTime | true.

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

% Maintain emission future window:
nexttime(T) ==> emission_future(T,T,[]).


nexttime(T) ==>
%	write('getting input symbol for time: '),
%	write(T),
%	nl,
	input_sequence(T,Symbol)
	|
	input(T,Symbol).
	
time(T) \ input(X,_) <=> X < T | true.

% Eliminate when it becomes past
time(T) \ emission_future(T,_,_) <=> true. 
/*

nexttime(Begin),input(End,Symbol),future_size(FS) \
emission_future(Begin,End,Sequence) <=>
%	write('emission_future attempted applied'),nl,
    FutureSize is  End - Begin,
    FutureSize < FS,
    NewEnd is End + 1
    |
    emission_future(Begin,NewEnd,[Symbol|Sequence]).
*/

% Sum multiple paths in the case they have identical history and constraints
% Note that constraints must occur canonical!
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

expandphase,time(T) ==> iteration_expansions(T,0).

% Count how many paths we extend in this iteration
iteration_expansions(T,A), iteration_expansions(T,B) <=>
    C is A + B,
	iteration_expansions(T,C).

% Find all path next states (preserve duplicates).
expand_non_endstate @
expandphase,
option(explicit_end_states),
time(Time),
nexttime(NextTime),
emission_history(Time,EmissionHistory), 
emission_future(NextTime,_,EmissionFuture),
path(Time,State,History,Constraints,Count),
trans(State,NextState,_),
emit(NextState,Symbol,_),
input(NextTime,Symbol),
sequence_length(SL),
reach_end(NextTime,NextState) ==>
    Time < SL,
    check_path_constraints(NextTime,NextState,[State|History],
			   Symbol,EmissionHistory,EmissionFuture,
			   Constraints,UpdConstraints)
    |
	iteration_expansions(Time,1),
    path(NextTime,NextState,[State|History],UpdConstraints,Count).

expand_implicit_endstate @
expandphase,
option(implicit_end_states),
time(Time), 
nexttime(NextTime),
input(NextTime,Symbol),
emission_history(Time,EmissionHistory),
emission_future(NextTime,_,EmissionFuture),
path(Time,State,History,Constraints,Count),
trans(State,NextState,_),
emit(NextState,Symbol,_) ==>
    check_path_constraints(NextTime,NextState,[State|History],
			   Symbol,EmissionHistory,EmissionFuture,
			   Constraints,UpdConstraints)
    |
	iteration_expansions(Time,1),
    path(NextTime,NextState,[State|History],UpdConstraints,Count).


% Special case for special end-state transitions when we have explicit end states:
expandphase, time(Time), nexttime(NextTime),
sequence_length(Time), state(NextState,end), trans(State,NextState,_),
path(Time,State,History,Constraints,Count) ==>
    write('extending path to end state'),
    write(path(NextTime,NextState,[State|History],Constraints,Count)),nl,
	iteration_expansions(Time,1),
    path(NextTime,NextState,[State|History],Constraints,Count).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The count phase:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

count(T,S1,S2,C1), count(T,S1,S2,C2) <=>
    C is C1 + C2,
    count(T,S1,S2,C).

count_emission @
countphase, time(T), input(T,Symbol), path(T,State,_,_,Count) ==>
    count(emit, State, Symbol, Count).

count_transition @
countphase, time(T), path(T,State,[PrevState|_],_,Count) ==>
    count(trans,PrevState,State,Count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sum totals phase
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
% Model extraction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extractionphase, outputfile(File) ==>
	nl,
	write('Writing derived probabilities to file: '),
	write(File),
	nl,
	tell(File).

extractionphase \ derived_trans(S1,S2,Probability) <=>
	write(trans(S1,S2,Probability)), write('.'), nl.
	
extractionphase \ derived_emit(State,Symbol,Probability) <=>
	write(emit(State,Symbol,Probability)), write('.'), nl.

extractionphase <=>	told.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Phase control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

totalphase <=> normphase.

normphase <=> extractionphase.

% Know when we are done:
time(T), countphase, sequence_length(SL) <=>
    T > SL |
%    write(normphase(T)),nl,
    totalphase.

% Stop if no extensions where made in last expand-phase

sequence_length(SL) \ time(T), expandphase <=>
    T =< SL,
    NewT is T + 1
    |
    time(NewT),
%    write(countphase(NewT)),nl,
    countphase.

countphase, time(T) \ iteration_expansions(T1,0) <=> 
	T1 < T
	|
	write('No possible expansions in iteration '),
	write(T),
	nl,
	halt.

time(T), sequence_length(SL) \ countphase <=>
    T =< SL
    |
%    write(expandphase(T)),nl,
    expandphase.
