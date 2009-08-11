% Constraint hooks,
% Constraints will be tested in the order they occur in the list supplied to 
% to user_constraints
user_constraints([
        nucleotide_constraint
]).

% hmmlearn expects inputs as input_sequence/2
% Map input_sequence/2 as indirection to nt/3
input_sequence(T,Symbol) :- nt(T,Symbol,_).

nucleotide_constraint(Position,coding,_,NextEmission,_,_,C,C) :-
        nt(Position,NextEmission,gene).

nucleotide_constraint(Position,noncoding,_,NextEmission,_,_,C,C) :-
        nt(Position,NextEmission,nongene).
