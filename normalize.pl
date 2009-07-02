enter_normalization_phase @
time(T), sequence_length(T) <=> normalize.

% Remove zero probability transitions
normalize \ trans_count(_,_,0) <=> true.
normalize \ trans_total(_,0) <=> true.
% Remove zero probability emission
normalize \ emit_count(_,_,0) <=> true.
normalize \ emit_total(_,0) <=> true.

normalize, trans_total(S1,CT) \
trans(S1,S2,_OldProb), trans_count(S1,S2,C) <=>
    Probability is C / CT
    |
    trans(S1,S2,Probability).

normalize, emit_total(State,CT) \
emit(State,Symbol,_OldProb), emit_count(State,Symbol,C) <=>
    Probability is C / CT
    |
    emit(State,Symbol,Probability).