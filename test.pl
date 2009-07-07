:- [hmmlearn].

% Specification of constraints
user_constraints([print_path]).

t :-
   trans(start, s1, 0.5),  trans(start, s2, 0.49), % trans(start, end, 0.01),
   trans(   s1, s1, 0.6),  trans(   s1, s2, 0.3 ), % trans(   s1, end, 0.1),
   trans(   s2, s1, 0.3),  trans(   s2, s2, 0.6 ),
%   trans(   s2, end, 0.1),

   emit(s1, a, 0.8),  emit(s1, b, 0.2),
   emit(s2, a, 0.2),  emit(s2, b, 0.7),emit(s2, c, 0.1),
   history_size(2),
   future_size(2),
   sequence([
	     a,b,a,a,b,c,b,a,a,b,a,a,b,c,b,a
	    ]),
   init.

