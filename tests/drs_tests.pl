:- begin_tests(drsTests).

test(implications) :-
  assertion(
    sentenceComponent(
      drs([], [drsImpl(drs([t1, t2], [friends(t1, t2)]), drs([], [tall(t1)])])),
      [if, tree, t1, and, t2, are, friends, ',', then, tree, t1, is, tall],
      []
    )
  ),
  assertion(
    sentenceComponent(
      drs([], drsImpl(drs([t1, t2], [friends(t1, t2)]), drs([], [tall(t1)]))),
      [tree, t1, is, tall, ',', if, tree, t1, and, tree, t2, are, friends],
      []
    )
  ).

test(definition) :-
  assertion(
    sentenceComponent(
      [drs([], [definition, drs([], [drsImpl(drs([t1, t2, t3], [friends(t1, t2), friends(t2, t3)]), drs([], [friends(t1, t3)]))])])],
      [
        we, define, the, following, ':',
        if, tree, t1, and, t2, are, friends,
        and, t2, and, t3, are, friends, ',',
        then, t1, and, t3, are, friends, '.',
        end, of, definition, '.'
      ],
      []
    )
  )

:- end_tests(drsTests).