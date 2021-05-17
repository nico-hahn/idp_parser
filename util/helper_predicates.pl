:- use_module(library(lists)).

% Takes a list [a, b, c, ...] and a functor func
% and returns a list [func(a), func(b), func(c), ...] 
encapsuleListInFunctor([], _, []).
encapsuleListInFunctor(InputList, Functor, OutputList) :-
  InputList = [In|InRest],
  OutputList = [call(Functor, In)|OutRest],
  encapsuleListInFunctor(InRest, Functor, OutRest).

% Takes a list [a, b, c, ...] and a predicate name 'pred'
% puts it into a predicate pred(a, b, c, ...)
buildDrsPredicate(PName, Args, Predicate) :-
  Predicate = apply(PName, Args)
  
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
  member(X, L2), !, remove_list(Tail, L2, Result). 
remove_intersection([X|Tail], L2, [X|Result]) :-
  remove_list(Tail, L2, Result).