:- begin_tests(grammarTests).

% TODO: DRS Threading tests go here once the system is set up.

test(argList) :-
  assertion(sentence(_, _, [there, are, trees, '.'], [])),
  assertion(sentence(_, _, [there, are, flowers, '.'], [])),
  assertion(argList(vSentence, _, _, _, [a, tree, and, a, flower], [])),
  assertion(argList(tSentence, notempty, [], RefsOut1, [a, tree, id1, and, a, flower, id2], [])),
  assertion(RefsOut1 = [id1, id2]),
  assertion(argList(tSentence, notempty, [], RefsOut2, [id1, and, id2], [])),
  assertion(RefsOut2 = [id1, id2]),
  assertion(argList(tSentence, notempty, [], RefsOut3, [a, tree, id1, and, id2], [])),
  assertion(RefsOut3 = [id1, id2]).
  
:- end_tests(grammarTests).