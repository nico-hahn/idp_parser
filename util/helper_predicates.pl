encapsuleListInFunctor([], _, []).
encapsuleListInFunctor(InputList, Functor, OutputList) :-
  InputList = [In|InRest],
  OutputList = [call(Functor, In)|OutRest],
  encapsuleListInFunctor(InRest, Functor, OutRest).

% Takes a list [a, b, c, ...] and a predicate name 'pred'
% puts it into a predicate pred(a, b, c, ...)
buildDrsPredicate(PName, Args, Predicate) :-
  Predicate = apply(PName, Args)
  
