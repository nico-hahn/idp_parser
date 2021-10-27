drs2fol(Drs, String) :-
  drs2fol(Drs, String, ['\t', '\n']).
drs2fol(Drs, String, [Tabs, Lines]) :-
  Drs = drs(Referents, Conditions),
  translateReferents(Referents, RefString, refBrackets(Open, Close)),
  translateConditions(Conditions, FolString),
  string_concat(FolCorrect, ' & ', FolString),
  super_concat([Tabs, RefString, Open, FolCorrect, Close, Lines], String).

translateConditions([], '').

translateConditions([Condition, DefinitionDrs|Rest], String) :-
  Condition = definition,
  drs2fol(DefinitionDrs, DefinitionString, ['\t\t', '\n']),
  super_concat(['definition {\n', DefinitionString, '\t} & '], String).

translateConditions([Condition|Rest], String) :-
  Condition = drsImpl(DrsAntecedent, DrsConsequent),
  drs2fol(DrsAntecedent, AntString, ['', '']),
  drs2fol(DrsConsequent, ConString, ['', '']),
  translateConditions(Rest, StrRest),
  super_concat([AntString, ' => ', ConString, ' & ', StrRest], String).

translateConditions([Condition|Rest], String) :-
  Condition = drsOr(Drs1, Drs2),
  drs2fol(Drs1, String1, ['', '']),
  drs2fol(Drs2, String2, ['', '']),
  translateConditions(Rest, StrRest),
  super_concat(['( ', String1, ' | ', String2, ' ) & ', StrRest], String).

translateConditions([Condition|Rest], String) :-
  Condition = drsNeg(DrsNeg),
  drs2fol(DrsNeg, NegFol, ['', '']),
  translateConditions(Rest, StrRest),
  super_concat(['~( ', NegFol, ' ) & ', StrRest], String).

translateConditions([Condition|Rest], String) :-
  translateConditions(Rest, StrRest),
  term_string(Condition, CondStr),
  super_concat([CondStr, ' & ', StrRest], String).

translateReferents([], '', refBrackets('', '')).
translateReferents(Referents, RefString, refBrackets('( ', ' )')) :-
  atomics_to_string(Referents, ' ', Refs),
  super_concat(['!', Refs, ' : '], RefString).