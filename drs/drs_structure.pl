%----------------------------
% Structure production

sSentence(DrsIn, DrsOut) -->
  sSentenceTyped(noun, DrsIn, DrsOut);
  sSentenceTyped(verb, DrsIn, DrsOut);
  sSentenceTyped(adjective, DrsIn, DrsOut).

sSentenceTyped(TYPE, DrsIn, DrsOut) -->
  argList(sSentence, ReferentList),
  sSentenceSuffix(TYPE, DrsIn, ReferentList, DrsOut).

sSentenceSuffix(noun, DrsIn, ReferentList, DrsOut) -->
  sSentenceConnector(noun),
  [TypeName],
  {
    type(TypeName),
    DrsIn = drs(_, Conditions),
    singularize(TypeName, Singular),
    encapsuleListInFunctor(ReferentList, Singular, NewConditions),
    append(NewConditions, Conditions, CondsOut),
    DrsOut = drs(typeInstance, CondsOut)
  }.
sSentenceConnector(noun) --> lit_is, determiner.
sSentenceConnector(noun) --> lit_are.

% Special cases for unary predicates ...

sSentenceSuffix(verb, DrsIn, ReferentList, DrsOut) -->
  [PredicateName],
  {
    length(ReferentList, Len),
    Len > 1,
    valid_predicate(PredicateName, 1),
    \+ valid_preposition(PredicateName, _),
    DrsIn = drs(RefsIn, CondsIn),
    append(ReferentList, RefsIn, RefsOut),
    encapsuleListInFunctor(ReferentList, PredicateName, CondsNew),
    append(CondsNew, CondsIn, CondsOut),
    DrsOut = drs(RefsOut, CondsOut)
  }.

sSentenceSuffix(_, DrsIn, ReferentList, DrsOut) -->
  lit_are,
  [PredicateName],
  {
    length(ReferentList, Len),
    Len > 1,
    valid_predicate(PredicateName, 1),
    \+ valid_preposition(PredicateName, _),
    DrsIn = drs(RefsIn, CondsIn),
    append(ReferentList, RefsIn, RefsOut),
    encapsuleListInFunctor(ReferentList, PredicateName, CondsNew),
    append(CondsNew, CondsIn, CondsOut),
    DrsOut = drs(RefsOut, CondsOut)
  }.


% Standard structure sentences ...

sSentenceSuffix(noun, DrsIn, ReferentList, DrsOut) -->
  sSentenceConnector(noun),
  [PredicateName],
  {
    length(ReferentList, Len),
    valid_predicate(PredicateName, Len),
    \+valid_preposition(PredicateName, _),
    DrsIn = drs(RefsIn, Conditions),
    append(ReferentList, RefsIn, RefsOut),
    buildDrsPredicate(PredicateName, ReferentList, NewCondition),
    DrsOut = drs(RefsOut, [NewCondition|Conditions])
  }.

sSentenceSuffix(noun, DrsIn, ReferentList, DrsOut) -->
  sSentenceConnector(noun),
  [PredicateName],
  structPrepositionPhrase(PredicateName, sSentence, DrsIn, ReferentList, DrsOut).

sSentenceSuffix(verb, DrsIn, ReferentList, DrsOut) -->
  [PredicateName],
  {
    remove_s_form(PredicateName, PredicateNameValid)
  },
  argList(sSentence, empty, MoreReferents),
  {
    append(ReferentList, MoreReferents, Referents),
    length(Referents, Len),
    valid_predicate(PredicateNameValid, Len),
    \+valid_preposition(PredicateNameValid, _),
    DrsIn = drs(RefsIn, Conditions),
    append(Referents, RefsIn, RefsOut),
    buildDrsPredicate(PredicateNameValid, Referents, NewCondition),
    DrsOut = drs(RefsOut, [NewCondition|Conditions])
  }.

sSentenceSuffix(verb, DrsIn, ReferentList, DrsOut) -->
  [PredicateName],
  {
    remove_s_form(PredicateName, PredicateNameValid)
  },
  structPrepositionPhrase(PredicateNameValid, sSentence, DrsIn, ReferentList, DrsOut).

remove_s_form(SForm, SForm) :-
  s_form_pair(SForm, _).
remove_s_form(SForm, SRemoved) :-
  s_form_pair(SRemoved, SForm).

sSentenceSuffix(adjective, DrsIn, ReferentList, DrsOut) -->
  is_are,
  [PredicateName],
  {
    length(ReferentList, Len),
    valid_predicate(PredicateName, Len),
    \+valid_preposition(PredicateName, _),
    DrsIn = drs(Referents, Conditions),
    append(ReferentList, Referents, RefsOut),
    buildDrsPredicate(PredicateName, ReferentList, NewCondition),
    DrsOut = drs(RefsOut, [NewCondition|Conditions])
  }.

sSentenceSuffix(adjective, DrsIn, ReferentList, DrsOut) -->
  is_are,
  [PredicateName],
  structPrepositionPhrase(PredicateName, sSentence, DrsIn, ReferentList, DrsOut).

% special preposition phrase rule
structPrepositionPhrase(Predicate, sSentence, DrsIn, ReferentList, DrsOut) -->
  [Preposition],
  argList(sSentence, MoreReferents),
  {
    append(ReferentList, MoreReferents, Referents),
    length(Referents, Len),
    Len > 2,
    valid_predicate(Predicate, 2),
    valid_preposition(Predicate, Preposition),
    DrsIn = drs(RefsIn, CondsIn),
    append(Referents, RefsIn, RefsOut),
    buildDrsBinaryPredicates(Predicate, Referents, NewConds),
    append(NewConds, CondsIn, CondsOut),
    DrsOut = drs(RefsOut, CondsOut)
  }.

% standard preposition phrase rule
structPrepositionPhrase(Predicate, sSentence, DrsIn, ReferentList, DrsOut) -->
  [Preposition],
  argList(sSentence, MoreReferents),
  {
    append(ReferentList, MoreReferents, Referents),
    length(Referents, Len),
    valid_predicate(Predicate, Len),
    valid_preposition(Predicate, Preposition),
    DrsIn = drs(RefsIn, CondsIn),
    append(Referents, RefsIn, RefsOut),
    buildDrsPredicate(Predicate, Referents, NewCondition),
    DrsOut = drs(RefsOut, [NewCondition|CondsIn])
  }.

sSentence(DrsIn, DrsOut) -->
  determiner,
  [FunctionName],
  lit_of,
  argList(tSentence, Referents),
  lit_is,
  [FunctionVal],
  {
    verify_not_reserved(FunctionName),
    verify_not_reserved(FunctionVal),
    DrsIn = drs(RefsIn, CondsIn),
    append(Referents, RefsIn, RefsOut),
    buildDrsPredicate(FunctionName, Referents, Function),
    DrsOut = drs(RefsOut, [f_value(Function, FunctionVal)|CondsIn])
  }.

%TODO: Refactor repeating extra arguments.