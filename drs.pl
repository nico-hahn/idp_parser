:- dynamic drs/2.
:- dynamic ndrs/3. % Named DRS

% initialize empty NDRSs for the structure and theory.
:- assertz(ndrs(structure, [], [])).
:- assertz(ndrs(theory, [], [])).
:- assertz(ndrs(knowledge, [], [])).

replace_drs_fact(OLD, NEW) :-
  call(OLD),
  retract(OLD),
  assertz(NEW).

assert_drs(ndrs(_, D, C)) :-
  assertz(drs(D,C)).

add_discourse_refs(N, D_REFS) :-
  extend_ndrs(N, D_REFS, []).

add_conditions(N, CONDS) :-
  extend_ndrs(N, [], CONDS).

extend_ndrs(N, D_REFS, CONDS) :-
  ndrs(N, D, C),
  append(D_REFS, D, D_NEW),
  append(CONDS, C, C_NEW),
  replace_drs_fact(ndrs(N, D, C), ndrs(N, D_NEW, C_NEW)).

% this predicate is used to build the complete DRS
% resulting from the entire user input.
get_knowledge() :-
  ndrs(structure, SD, SC),
  ndrs(theory, TD, TC),
  append(SC, [drs(TD, TC)], CONDITIONS),
  assertz(drs(SD, CONDITIONS)),
  write(drs(SD, CONDITIONS)).