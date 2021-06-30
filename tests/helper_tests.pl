:- begin_tests(helperTests).

test(encapsuleListInFunctor) :-
  encapsuleListInFunctor([a, b, c], func, [func(a), func(b), func(c)]).
test(encapsuleListInFunctor_Fail, [fail]) :-
  encapsuleListInFunctor([a, b, c], func, [a, fun(b), fun(c)]).

test(buildDrsPredicate) :-
  buildDrsPredicate(pred, [b, c], pred(b, c)).

test(binaryPredicate) :-
  buildDrsBinaryPredicates(
    pred,
    [a, b, c, d, x],
    [pred(a, x), pred(b, x), pred(c, x), pred(d, x)]
  ).

test(removeIntersetion) :-
  remove_intersection([a, b, c, d], [b, d], [a, c]).

:- end_tests(helperTests).