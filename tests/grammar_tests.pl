:- begin_tests(grammarTests).

% TODO: DRS Threading tests go here once the system is set up.

test(types) :-
  assertion(sentence([there, are, trees, '.'], [])),
  assertion(sentence([there, are, flowers, '.'], [])),
  assertion(sentence([there, are, areas, '.'], [])),
  assertion(sentence([there, are, dogs, '.'], [])),
  assertion(sentence([there, are, persons, '.'], [])),
  assertion(noun(tree, trees)),
  assertion(noun(flower, flowers)).

%test(typesFail, [fail]) :-
%  assertion(sentence([there, are, spoons, spoons, '.'], [])).

test(argList) :-
  assertion(
    optional_det_phrase([a, tree], [])
  ),
  assertion(
    argList(vSentence, notempty, [flower, tree], [a, tree, and, a, flower], [])
  ),
  assertion(
    argList(tSentence, notempty, [id2, id1], [a, tree, id1, and, a, flower, id2], [])
  ),
  assertion(
    argList(tSentence, notempty, [id2, id1], [id1, and, id2], [])
  ),
  assertion(
    argList(tSentence, notempty, [id2, id1], [a, tree, id1, and, id2], [])
  ),
  assertion(
    argList(sSentence, [id3, id2, id1], [id1, and, id2, and, id3], [])
  ).

test(predicateDefinitions) :-
  assertion(
    sentenceComponent(_, [an, area, can, border, an, area], [])
  ),
  assertion(
    sentenceComponent(_, [a, tree, can, be, tall], [])
  ),
  assertion(
    sentenceComponent(_, [a, tree, and, a, tree, can, be, friends], [])
  ),
  assertion(
    sentenceComponent(_, [a, tree, can, be, married, to, a, flower], [])
  ),
  assertion(
    sentenceComponent(_, [a, flower, can, fly], [])  
  ).

test(functionDefinitions) :-
  assertion(
    sentenceComponent(_, [an, area, can, have, a, color], [])
  ),
  assertion(
    sentenceComponent(_, [a, dog, can, have, an, owner, that, is, a, person], [])
  ).

:- end_tests(grammarTests).