% Initialize time constraints from sequence
sequence(X) <=> sequence(0,X). % Add Time counter
sequence(_,[]) <=> true. % Recursion termination
sequence(Time,[Elem|R]) <=> % Add aa time constraint for each symbol
    NextTime is Time + 1
    |
    time(Time,Elem),
    sequence(NextTime,R).


% Initialization of transition and emission counter constraints
trans_total(St,0) \ trans_total(St,0) <=> true. % dup. elim.
emit_total(St,0) \ emit_total(St,0) <=> true. % dup. elim.
init_counts, trans(S1,S2,_P) <=> trans_count(S1,S2,0), trans_total(S1,0).
init_counts, emit(S1,_P) <=> emit_count(S1,0), emit_total(S1,0).
init_counts <=> true. % done


state(Time,State),input(Time,Symbol), time(Time) \
trans_count(State,NxtState,TransCount),emit_count(State, Symbol,EmitCount),
total_trans(State,TotalTransCount), total_emit(State,TotalEmitCount) <=>
     NewCount is Count + 1,
     NextTime is Time + 1,
     NextEmitCount is EmitCount + 1,
     NextTotalTransCount is TotalTransCount + 1,
     NextTotalEmitCount is TotalEmitCount + 1
     |
     state(NextTime,NxtState),
     emit(State,Symbol,NextEmitCount),
     total_emit(State, NextTotalEmitCount),
     trans(State,NxtState,NewCount).

time(T), input(T,_) <=> T1 is T + 1 | time(T1).

time(Y) <=> normalize.

normalize, total_trans(S1,CT) \ trans_count(S1,S2,C) <=>
    Probability is C / CT
    |
    trans(S1,S2,Probability).

normalize, total_emit(State,Symbol,CT) \ trans_count(S,C) <=>
    Probability is C / CT
    |
    emit(State,Symbol,Probability).