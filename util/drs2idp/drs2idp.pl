:- [func2idp].
:- [theory2idp].

drs2idp(vocabulary, Drs, IdpOut) :-
  reverse(Drs, DrsRev),
  buildIdp(vocabulary, DrsRev, Idp),
  super_concat(['vocabulary v1 {\n', Idp, '}'], IdpOut).

drs2idp(theory, Drs, IdpOut) :-
  reverse(Drs, DrsRev),
  buildIdp(theory, DrsRev, Idp),
  super_concat(['theory t1: v1 {\n', Idp, '}'], IdpOut).

drs2idp(structure, Drs, IdpOut) :- 
  reverse(Drs, DrsRev),
  buildIdp(structure, DrsRev, Idp),
  super_concat(['structure s1: v1 {\n', Idp, '}'], IdpOut).

drs2idp(procedure, _, IdpOut) :-
  super_concat([
    'procedure main(){\n',
    '\tmodels = modelexpand(t1, s1)\n',
    '\tprint(models)\n',
    '}'
  ], IdpOut).

buildIdp(_, [], '').

buildIdp(vocabulary, [Drs|Rest], IdpNew) :-
  buildIdp(vocabulary, Rest, Idp),
  buildStringfromDrs(vocabulary, Drs, String),
  string_concat(Idp, String, IdpNew).

buildIdp(theory, [Drs|Rest], IdpNew) :-
  buildIdp(theory, Rest, Idp),
  drs2fol(Drs, String),
  string_concat(Idp, String, IdpNew).

buildIdp(structure, DrsList, IdpNew) :-
  concatConditions(DrsList, Conditions),
  getTypes(DrsList, Types),
  getFunctionValues(Conditions, Values),
  getStructureElements(Conditions, StructureElements),
  getStructureElements(Types, TypeElements),
  log(Values),
  log(StructureElements),
  log(TypeElements),
  buildStructure(type, TypeElements, Str0),
  buildStructure(noType, StructureElements, Str1),
  buildFunctionStruct(Values, Str2),
  string_concat(Str0, Str1, Str01),
  string_concat(Str01, Str2, IdpNew).
  % StructureElements = [type([type(a), type(b)]), predicate([predicate(a,b), ...])]

buildStructure(IsType, [], '').
buildStructure(IsType, [Element|Rest], OutString) :-
  buildStructure(IsType, Rest, Idp),
  createElementString(IsType, Element, String),
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
  concatConditions(Rest, ConditionsOut),
  Drs = drs(typeInstance, _).
concatConditions([Drs|Rest], ConditionsOut) :-
  concatConditions(Rest, Conditions),
  Drs = drs(_, ConditionsCurrent),
  append(ConditionsCurrent, Conditions, ConditionsOut).

getTypes([], []).
getTypes([Drs|Rest], ConditionsOut) :-
  getTypes(Rest, ConditionsOut),
  Drs = drs(A, _),
  A \= typeInstance.
getTypes([Drs|Rest], ConditionsOut) :-
  getTypes(Rest, Conditions),
  Drs = drs(typeInstance, ConditionsCurrent),
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
createElementString(noType, Element, String) :-
  Element =.. [PredicateName, PredicateList],
  buildPredicateMembers(PredicateList, MemString),
  string_concat(MemberString, '; ', MemString),
  super_concat(['\t', PredicateName, '<ct> = { ', MemberString, ' }\n'], String).

createElementString(type, Element, String) :-
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