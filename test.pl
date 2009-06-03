:- [hmmlearn].

tst1 :- 
       trans(start, s1, 0.5),  trans(start, s2, 0.49),  trans(start, end, 0.01),
       trans(   s1, s1, 0.6),  trans(   s1, s2, 0.3 ),  trans(   s1, end, 0.1),
       trans(   s2, s1, 0.3),  trans(   s2, s2, 0.6 ),  trans(   s2, end, 0.1),

       emit(s1, a, 0.8),  emit(s1, b, 0.2),
       emit(s2, a, 0.2),  emit(s2, b, 0.8), 
       sequence([a,b,a,b,a,b,a,a,a,b,b,b,b,a,a,b,a,b,a]),
       init_counts.

