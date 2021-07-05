:- begin_tests(drsTests).

test(implications) :-
  assertion(
    sentenceComponent(
      drs([], [drsImpl(drs([t1, t2], [friends(t1, t2)]), drs([], [tall(t1)]))]),
      [if, a, tree, t1, and, t2, are, friends, ',', then, the, tree, t1, is, tall],
      []
    )
  ),
  assertion(
    sentenceComponent(
      drs([], [drsImpl(drs([t1, t2], [friends(t1, t2)]), drs([], [tall(t1)]))]),
      [a, tree, t1, is, tall, ',', if, a, tree, t1, and, the, tree, t2, are, friends],
      []
    )
  ).

test(definition) :-
  assertion(
    sentenceComponent(
      [drs([], [definition, drs([], [drsImpl(drs([t1, t2, t3], [friends(t1, t2), friends(t2, t3)]), drs([], [friends(t1, t3)]))])])],
      [
        we, define, the, following, ':',
        if, a, tree, t1, and, t2, are, friends,
        and, t2, and, t3, are, friends, ',',
        then, t1, and, t3, are, friends, '.',
        end, of, definition, '.'
      ],
      []
    )
  ).

test(quant_imp) :-
  assertion(
    sentenceComponent(
      drs([t1, t2], [drsImpl(drs([t1, t2], [friends(t1, t2)]), drs([], [friends(t2, t1)]))]),
      [for, all, t1, and, t2, ',', if, t1, and, t2, are, friends, ',', then, t2, and, t1, are, friends],
      []
    )
  ).

:- end_tests(drsTests).