:- use_module(library(lists)).

% Takes a list [a, b, c, ...] and a functor func
% and returns a list [func(a), func(b), func(c), ...] 
encapsuleListInFunctor([], _, []).
encapsuleListInFunctor(InputList, Functor, OutputList) :-
  InputList = [In|InRest],
  Term =.. [Functor, In],
  OutputList = [Term|OutRest],
  encapsuleListInFunctor(InRest, Functor, OutRest).

% Takes a list [a, b, c, ...] and a predicate name 'pred'
% puts it into a predicate pred(a, b, c, ...)
buildDrsPredicate(PName, Args, Predicate) :-
  Predicate =.. [PName|Args].

% Takes a list [a, b, c, ..., x] and a predicate name 'pred'
% and returns a list [pred(a, x), pred(b, x), pred(c, x), ...]
buildDrsBinaryPredicates(PName, Args, Predicates) :-
  last(Args, Last),
  append(Arguments, [Last], Args),
  buildBinaryPreds(PName, Arguments, Last, Predicates).

buildBinaryPreds(_, [], _, []).
buildBinaryPreds(Name, Args, Last, Predicates) :-
  Args = [Arg|ArgsRest],
  Predicate =.. [Name, Arg, Last],
  Predicates = [Predicate|PredicatesRest],
  buildBinaryPreds(Name, ArgsRest, Last, PredicatesRest).

% Takes a main drs, an antecedent drc and a consequence drs
% and builds that into the main drs
buildDrsImplication(DrsIn, DrsAntecedent, DrsConsequent, DrsOut) :-
  DrsAntecedent = drs(RefsAnt, _),
  DrsConsequent = drs(RefsCons, CondsCons),
  remove_intersection(RefsCons, RefsAnt, RefsConsOut),
  DrsIn = drs(RefsIn, CondsIn),
  DrsOut = drs(RefsIn, [drsImpl(DrsAntecedent, drs(RefsConsOut, CondsCons))|CondsIn]).

% Removes every element of list2 in list1.
% Result is in list3.
remove_intersection([], _, []).
remove_intersection([X|Tail], L2, Result) :- 
  member(X, L2), !, remove_intersection(Tail, L2, Result). 
remove_intersection([X|Tail], L2, [X|Result]) :-
  remove_intersection(Tail, L2, Result).

% Write something to the user_error stream (log/1)
% or to another stream (log/2)
log(Message) :-
  log(user_error, Message).
log(Stream, Message) :-
  is_list(Message),
  is_atom_list(Message),
  atomics_to_string(Message, ' ', String),
  writeln(Stream, String),
  !.
log(Stream, Message) :-
  writeln(Stream, Message).
is_atom_list([]).
is_atom_list([H|T]) :-
  atom(H),
  is_atom_list(T),
  !.
is_atom_list([H|T]) :-
  number(H),
  is_atom_list(T).