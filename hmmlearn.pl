:- use_module(library(chr)).

:- chr_constraint
	sequence/1, sequence/2, init_counts/0,
	trans_count/3, trans_total/2,
	emit_count/3, emit_total/2,
	normalize/0, time/1, state/2, input/2,
	trans/3, emit/3.

% Initialize time constraints from sequence
sequence(X) <=> sequence(0,X). % Add Time counter
sequence(_,[]) <=> true. % Recursion termination
sequence(Time,[Elem|R]) <=> % Add an input constraint for each symbol
    NextTime is Time + 1
    |
    input(Time,Elem),
    sequence(NextTime,R).

% Initialization of transition and emission counter constraints
trans_total(St,0) \ trans_total(St,0) <=> true. % dup. elim.
emit_total(St,0) \ emit_total(St,0) <=> true. % dup. elim.
init_counts, trans(S1,S2,_P) <=> trans_count(S1,S2,0), trans_total(S1,0).
init_counts, emit(S1,Symbol,_P) <=> emit_count(S1,Symbol,0), emit_total(S1,0).
init_counts <=> true. % done

state(Time,State), input(Time,Symbol), time(Time) \
trans_count(State,NextState,TransCount), emit_count(State, Symbol,EmitCount),
trans_total(State,TotalTransCount), emit_total(State,TotalEmitCount) <=>
     NextTime is Time + 1,
     NextTransCount is TransCount + 1,
     NextEmitCount is EmitCount + 1,
     NextTotalTransCount is TotalTransCount + 1,
     NextTotalEmitCount is TotalEmitCount + 1
     |
     state(NextTime,NextState),
     emit_count(State,Symbol,NextEmitCount),
     emit_total(State, NextTotalEmitCount),
     trans_count(State,NextState,NextTransCount),
     trans_total(State, NextTotalTransCount).

time(T), input(T,_) <=> T1 is T + 1 | time(T1).

time(Y) <=> normalize.

normalize, trans_total(S1,CT) \ trans_count(S1,S2,C) <=>
    Probability is C / CT
    |
    trans(S1,S2,Probability).

normalize, emit_total(State,CT) \ emit_count(State,Symbol,C) <=>
    Probability is C / CT
    |
    emit(State,Symbol,Probability).