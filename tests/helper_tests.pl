:- begin_tests(helperTests).

test(encapsuleListInFunctor) :-
  encapsuleListInFunctor([a, b, c], func, [func(a), func(b), func(c)]).
test(encapsuleListInFunctor_Fail, [fail]) :-
  encapsuleListInFunctor([a, b, c], func, [a, fun(b), fun(c)]).

test(buildDrsPredicate) :-
  buildDrsPredicate(pred, [b, c], pred(b, c)).

:- end_tests(helperTests).