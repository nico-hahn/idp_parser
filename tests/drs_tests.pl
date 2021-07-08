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

test(andConnection) :-
  assertion(
    superTSentence(
      drs([], []),
      drs([t3, t1, t2], [friends(t2, t3), friends(t1, t2)]),
      [t1, and, t2, are, friends, and, t2, and, t3, are, friends],
      []
    )
  ).

test(definitionEmpty) :- 
  assertion(
    sentenceComponent(
      drs([], [definition]),
      [we, define, the, following, ':', end, of, definition, '.'],
      []
    )
  ).

test(definitionSimple) :-
  assertion(
    sentenceComponent(
      drs([], [definition, drs([t1], [tall(t1)])]),
      [
        we, define, the, following, ':',
        the, tree, t1, is, tall, '.',
        end, of, definition, '.'
      ],
      []
    )
  ).

test(definitionComplex) :-
  assertion(
    sentenceComponent(
      drs([], [definition, drs([], [drsImpl(drs([t3, t1, t2], [friends(t2, t3), friends(t1, t2)]), drs([], [friends(t1, t3)]))])]),
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

test(quantImp) :-
  assertion(
    sentenceComponent(
      drs([t1, t2], [drsImpl(drs([t1, t2], [friends(t1, t2)]), drs([], [friends(t2, t1)]))]),
      [for, all, t1, and, t2, ',', if, t1, and, t2, are, friends, ',', then, t2, and, t1, are, friends],
      []
    )
  ).

test(everyQuant) :-
  assertion(
    sentenceComponent(
      drs([], [drsImpl(drs([t1], [tree(t1)]), drs([t2], [married(t1, t2)]))]),
      [every, tree, t1, is, married, to, a, flower, t2],
      []
    )
  ).

test(functionStructure) :-
  assertion(
    sSentence(
      drs([], []),
      drs([barker], [value(owner(barker), peter)]),
      [the, owner, of, the, dog, barker, is, peter],
      []
    )
  ).

test(functionTheory) :-
  assertion(
    tSentence(
      drs([], []),
      drs([dog2, dog1], [notEqual(owner(dog1), owner(dog2))]),
      [the, owner, of, the, dog, dog1, is, not, the, owner, of, dog2],
      []
    )
  ),
  assertion(
    tSentence(
      drs([], []),
      drs([dog1], [notEqual(owner(dog1), peter)]),
      [the, owner, of, the, dog, dog1, is, different, from, peter],
      []
    )
  ),
  assertion(
    tSentence(
      drs([], []),
      drs([dog1], [equal(peter, owner(dog1))]),
      [peter, is, the, owner, of, the, dog, dog1],
      []
    )
  ).

test(structureTypes) :-
  assertion(
    sSentence(
      drs([], []),
      drs([t1], [tree(t1)]),
      [t1, is, a, tree],
      []
    )
  ),
  assertion(
    sSentence(
      drs([], []),
      drs([t1, t2, t3], [tree(t1), tree(t2), tree(t3)]),
      [t1, and, t2, and, t3, are, trees],
      []
    )
  ).

test(structurePredicated) :-
  assertion(
    sSentence(
      drs([], []),
      drs([t1, t2, t3], [tall(t1), tall(t2), tall(t3)]),
      [t1, and, t2, and, t3, are, tall],
      []
    )
  ),
  assertion(
    sSentence(
      drs([], []),
      drs([t1, t2], [friends(t1, t2)]),
      [t1, and, t2, are, friends],
      []
    )
  ).

:- end_tests(drsTests).