:- dynamic drs/2.

assert_drs(D, C) :-
  assertz(drs(D,C)).