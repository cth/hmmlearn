% Initialization
reset_all_counts, trans(S1,S2,C) | C \= 0 <=> trans(S1,S2,0).
reset_all_counts, emit(St,Sym,C) | C \= 0 <=> emit(St,Sym,0).

state(Time,State) \
input(Time,Symbol), time(Time),
trans(State,NxtState,TransCount),emit(State, Symbol,EmitCount) <=>
     NewCount is Count + 1,
     NextTime is Time + 1,
     NextEmitCount is EmitCount + 1
     |
     state(NextTime,NxtState),
     emit(State,Symbol,NextEmitCount),
     trans(State,NxtState,NewCount).

% When to increase time-step
% - All transitions have been updated