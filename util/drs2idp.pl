drs2idp(vocabulary, Drs, IdpOut) :-
  reverse(Drs, DrsRev),
  buildIdp(vocabulary, DrsRev, Idp),
  super_concat(['vocabulary v1 {\n', Idp, '}'], IdpOut).

drs2idp(theory, Drs, IdpOut).

drs2idp(structure, Drs, IdpOut).

buildIdp(vocabulary, [], '').
buildIdp(vocabulary, [Drs|Rest], IdpNew) :-
  buildIdp(vocabulary, Rest, Idp),
  buildStringfromDrs(vocabulary, Drs, String),
  string_concat(Idp, String, IdpNew).

buildStringfromDrs(vocabulary, drs([typeDeclaration], [type(X)]), String) :-
  atom_string(X, XStr),
  super_concat(['\ttype ', XStr, '\n'], String).

buildStringfromDrs(vocabulary, drs([functionDeclaration], [function(FName, FArgs, FType)]), String) :-
  atom_string(FName, FNameStr),
  atom_string(FType, FTypeStr),
  atomics_to_string(FArgs, ', ', FArgsStr),
  super_concat(['\t', FNameStr, '(', FArgsStr, '): ', FTypeStr, '\n'], String).

buildStringfromDrs(vocabulary, drs([predicateDeclaration], [Predicate]), String) :-
  term_string(Predicate, PredicateStr),
  super_concat(['\t', PredicateStr, '\n'], String).