:- begin_tests(grammarTests).

% TODO: DRS Threading tests go here once the system is set up.

test(types) :-
  assertion(sentence(vocabulary, _, user_error, [there, are, trees, '.'], [])),
  assertion(sentence(vocabulary, _, user_error, [there, are, flowers, '.'], [])),
  assertion(sentence(vocabulary, _, user_error, [there, are, areas, '.'], [])),
  assertion(sentence(vocabulary, _, user_error, [there, are, dogs, '.'], [])),
  assertion(sentence(vocabulary, _, user_error, [there, are, persons, '.'], [])),
  assertion(noun(tree, trees)),
  assertion(noun(flower, flowers)).

test(argList) :-
  assertion(
    optional_det_phrase([a, tree], [])
  ),
  assertion(
    argList(vSentence, notempty, [tree, flower], [a, tree, and, a, flower], [])
  ),
  assertion(
    argList(tSentence, notempty, [id1, id2], [a, tree, id1, and, a, flower, id2], [])
  ),
  assertion(
    argList(tSentence, notempty, [id1, id2], [id1, and, id2], [])
  ),
  assertion(
    argList(tSentence, notempty, [id1, id2], [a, tree, id1, and, id2], [])
  ),
  assertion(
    argList(sSentence, [id1, id2, id3], [id1, and, id2, and, id3], [])
  ).

test(predicateDefinitions) :-
  assertion(
    sentenceComponent(vocabulary, _, [an, area, can, border, an, area], [])
  ),
  assertion(
    sentenceComponent(vocabulary, _, [a, tree, can, be, tall], [])
  ),
  assertion(
    sentenceComponent(vocabulary, _, [a, tree, and, a, tree, can, be, friends], [])
  ),
  assertion(
    sentenceComponent(vocabulary, _, [a, tree, can, be, married, to, a, flower], [])
  ),
  assertion(
    sentenceComponent(vocabulary, _, [a, flower, can, fly], [])  
  ).

test(functionDefinitions) :-
  assertion(
    sentenceComponent(vocabulary, _, [an, area, can, have, a, color], [])
  ),
  assertion(
    sentenceComponent(vocabulary, _, [a, dog, can, have, an, owner, that, is, a, person], [])
  ).

test(metavocabulary) :-
  assertion(
    metaSentence(
      user_error,
      vocabulary,
      _,
      [
        there, are, members, '.', a, member, and, a,
        member, can, be, friends2, '.', a, member, can, be, tall2, '.',
        a, member, can, be, a, premium, '.'
      ],
      []
    )
  ).

test(metatheory) :-
  assertion(
    metaSentence(
      user_error,
      theory,
      _,
      [
        we, define, the, following, ':',
        if, a, member, m1, and, a, member, m2, are, friends2,
        and, m2, and, a, member, m3, are, friends2, ',',
        then, m1, and, m3, are, friends2, '.',
        end, of, definition, '.',
        if, a, member, m1, is, tall2, ',',
        then, m1, is, not, a, premium, '.'
      ],
      []
    )
  ).

test(metastructure) :-
  assertion(
    metaSentence(
      user_error,
      structure,
      _,
      [
        m1, and, m2, and, m3, and, m4, are, members, '.',
        m1, is, a, premium, '.',
        m2, is, tall2, '.',
        m3, and, m4, are, friends2, '.'    
      ],
      []
    )
  ).

:- end_tests(grammarTests).