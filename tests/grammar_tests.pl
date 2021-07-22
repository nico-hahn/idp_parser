:- begin_tests(grammarTests).

% TODO: DRS Threading tests go here once the system is set up.

test(types) :-
  assertion(sentence(vocabulary, user_error, [there, are, trees, '.'], [])),
  assertion(sentence(vocabulary, user_error, [there, are, flowers, '.'], [])),
  assertion(sentence(vocabulary, user_error, [there, are, areas, '.'], [])),
  assertion(sentence(vocabulary, user_error, [there, are, dogs, '.'], [])),
  assertion(sentence(vocabulary, user_error, [there, are, persons, '.'], [])),
  assertion(noun(tree, trees)),
  assertion(noun(flower, flowers)).

%test(typesFail, [fail]) :-
%  assertion(sentence([there, are, spoons, spoons, '.'], [])).

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

:- end_tests(grammarTests).