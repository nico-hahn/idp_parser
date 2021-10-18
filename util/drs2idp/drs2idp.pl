:- [func2idp].

drs2idp(vocabulary, Drs, IdpOut) :-
  reverse(Drs, DrsRev),
  buildIdp(vocabulary, DrsRev, Idp),
  super_concat(['vocabulary v1 {\n', Idp, '}'], IdpOut).

drs2idp(theory, Drs, IdpOut).

drs2idp(structure, Drs, IdpOut) :- 
  reverse(Drs, DrsRev),
  buildIdp(structure, DrsRev, Idp),
  super_concat(['structure s1 {\n', Idp, '}'], IdpOut).

buildIdp(_, [], '').

buildIdp(vocabulary, [Drs|Rest], IdpNew) :-
  buildIdp(vocabulary, Rest, Idp),
  buildStringfromDrs(vocabulary, Drs, String),
  string_concat(Idp, String, IdpNew).

buildIdp(structure, DrsList, IdpNew) :-
  concatConditions(DrsList, Conditions),
  getFunctionValues(Conditions, Values),
  getStructureElements(Conditions, StructureElements),
  log(Values),
  log(StructureElements),
  buildStructure(StructureElements, Str1),
  buildFunctionStruct(Values, Str2),
  string_concat(Str1, Str2, IdpNew).
  % StructureElements = [type([type(a), type(b)]), predicate([predicate(a,b), ...])]

buildStructure([], '').
buildStructure([Element|Rest], OutString) :-
  buildStructure(Rest, Idp),
  createElementString(Element, String),
  string_concat(Idp, String, OutString).

buildFunctionStruct([], '').
buildFunctionStruct([Function|Functions], OutString) :-
  buildFunctionStruct(Functions, Idp),
  createFunctionString(Function, String),
  string_concat(Idp, String, OutString).

buildStringfromDrs(vocabulary, drs([typeDeclaration], [type(X)]), String) :-
  atom_string(X, XStr),
  super_concat(['\ttype ', XStr, '\n'], String).

buildStringfromDrs(vocabulary, drs([functionDeclaration], [function(FName, FArgs, FType)]), String) :-
  atom_string(FName, FNameStr),
  atom_string(FType, FTypeStr),
  atomics_to_string(FArgs, ', ', FArgsStr),
  super_concat(['\tf_', FNameStr, '(', FArgsStr, '): ', FTypeStr, '\n'], String).

buildStringfromDrs(vocabulary, drs([predicateDeclaration], [Predicate]), String) :-
  term_string(Predicate, PredicateStr),
  super_concat(['\t', PredicateStr, '\n'], String).

% Takes all conditions from a list of DRS
% and puts them into a single list
concatConditions([], []).
concatConditions([Drs|Rest], ConditionsOut) :-
  concatConditions(Rest, Conditions),
  Drs = drs(_, ConditionsCurrent),
  append(ConditionsCurrent, Conditions, ConditionsOut).

% Takes a list of conditions, sorts it by predicate name
% and returns a list of the form [predicate([predicate(a, b)]), p2([p2(a), p2(b)])]
getStructureElements([], []).
getStructureElements([Condition|Conditions], Elements) :-
  Condition =.. [ElementName|_],
  ElementName = f_value,
  getStructureElements(Conditions, Elements).
getStructureElements([Condition|Conditions], [Element|Elements]) :-
  Condition =.. [ElementName|_],
  ElementName \= f_value,
  obtainAllRemaining(ElementName, Conditions, ConditionsFound, ConditionsRest),
  Element =.. [ElementName, [Condition|ConditionsFound]],
  getStructureElements(ConditionsRest, Elements).

obtainAllRemaining(_, [], [], []).
obtainAllRemaining(Element, [Condition|Conditions], [Condition|Found], Rest) :-
  Condition =.. [Element|_], % maybe cut here
  obtainAllRemaining(Element, Conditions, Found, Rest).
obtainAllRemaining(Element, [Condition|Conditions], Found, [Condition|Rest]) :-
  Condition =.. [E|_],
  Element \= E,
  obtainAllRemaining(Element, Conditions, Found, Rest).

% Element = predicate([predicate(a, b), predicate(a,c)])
createElementString(Element, String) :-
  Element =.. [PredicateName, PredicateList],
  buildPredicateMembers(PredicateList, MemString),
  string_concat(MemberString, '; ', MemString),
  super_concat(['\t', PredicateName, ' = { ', MemberString, ' }\n'], String).

buildPredicateMembers([], '').
buildPredicateMembers([Predicate|Rest], String) :-
  buildPredicateMembers(Rest, Str1),
  Predicate =.. [_|Args],
  atomics_to_string(Args, ', ', Str2),
  super_concat([Str2, '; ', Str1], String).