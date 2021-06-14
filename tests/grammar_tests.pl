:- begin_tests(grammarTests).

% TODO: DRS Threading tests go here once the system is set up.

test(types) :-
  assertion(sentence([there, are, trees, '.'], [])),
  assertion(sentence([there, are, flowers, '.'], [])),
  assertion(noun(tree, trees)),
  assertion(noun(flower, flowers)).

%test(typesFail, [fail]) :-
%  assertion(sentence([there, are, spoons, spoons, '.'], [])).

test(argList) :-
  assertion(
    optional_det_phrase([a, tree], [])
  ),
  assertion(
    argList(vSentence, notempty, _, [a, tree, and, a, flower], [])
  ),
  assertion(
    argList(tSentence, notempty, [id1, id2], [a, tree, id1, and, a, flower, id2], [])
  ),
  assertion(
    argList(tSentence, notempty, [id1, id2], [id1, and, id2], [])
  ),
  assertion(
    argList(tSentence, notempty, [id1, id2], [a, tree, id1, and, id2], [])
  ).
  
:- end_tests(grammarTests).