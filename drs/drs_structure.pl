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
  [TYPE_NAME],
  {
    type(TYPE_NAME),
    DrsIn = drs(Referents, Conditions),
    append(ReferentList, Referents, RefsOut),
    %TODO: Singularize TYPE_NAME here
    encapsuleListInFunctor(ReferentList, TYPE_NAME, NewConditions),
    append(NewConditions, Conditions, CondsOut),
    DrsOut = drs(RefsOut, CondsOut)
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

sSentenceSuffix(verb, DrsIn, ReferentList, DrsOut) -->
  [PredicateName],
  argList(sSentence, empty, MoreReferents),
  {
    append(ReferentList, MoreReferents, Referents),
    length(Referents, Len),
    valid_predicate(PredicateName, Len),
    \+valid_preposition(PredicateName, _),
    DrsIn = drs(RefsIn, Conditions),
    append(Referents, RefsIn, RefsOut),
    buildDrsPredicate(PredicateName, Referents, NewCondition),
    DrsOut = drs(RefsOut, [NewCondition|Conditions])
  }.

sSentenceSuffix(verb, DrsIn, ReferentList, DrsOut) -->
  [PredicateName],
  structPrepositionPhrase(PredicateName, sStentence, DrsIn, ReferentList, DrsOut).

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

sSentence -->
  determiner,
  [FUNCTION_NAME],
  lit_of,
  argList(tSentence),
  lit_is,
  [FUNCTION_VAL],
  {
    verify_not_reserved(FUNCTION_NAME),
    verify_not_reserved(FUNCTION_VAL)
  }.

%TODO: Refactor repeating extra arguments.