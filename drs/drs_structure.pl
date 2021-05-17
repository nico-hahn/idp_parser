%----------------------------
% Structure production

idList(RefsIn, RefsOut) -->
  [IDENTIFIER],
  moreIdList(RefsIn, RefsOut), 
  {
    verify_not_reserved(IDENTIFIER),
    RefsOut = [IDENTIFIER|RefsIn]
  }.
moreIdList(Refs, Refs) --> [].
moreIdList(RefsIn, RefsOut) -->
  cc,
  idList(RefsIn, RefsOut).
optionalIdList(RefsIn, RefsOut) -->
  idList(RefsIn, RefsOut);
  [].

sSentence(DrsIn, DrsOut) -->
  sSentenceTyped(noun, DrsIn, DrsOut);
  sSentenceTyped(verb, DrsIn, DrsOut);
  sSentenceTyped(adjective, DrsIn, DrsOut).

sSentenceTyped(TYPE, DrsIn, DrsOut) -->
  idList([], ReferentList),
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
 
sSentenceSuffix(verb, DrsIn, ReferentList, DrsOut) -->
  [PREDICATE_NAME],
  optionalIdList,
  {
    valid_predicate(PREDICATE_NAME),
    \+valid_preposition(PREDICATE_NAME, _),
    DrsIn = drs(Referents, Conditions),
    append(ReferentList, Referents, RefsOut),
    encapsuleListInFunctor(ReferentList, PREDICATE_NAME, NewConditions),
    append(NewConditions, Conditions, CondsOut),
    DrsOut = drs(RefsOut, CondsOut)
  }.

sSentenceSuffix(verb, DrsIn, ReferentList, DrsOut) -->
  [PREDICATE_NAME],
  structPrepositionPhrase(PREDICATE_NAME, sStentence, DrsIn, ReferentList, DrsOut).

sSentenceSuffix(adjective, DrsIn, ReferentList, DrsOut) -->
  is_are,
  [PREDICATE_NAME],
  {
    valid_predicate(PREDICATE_NAME),
    \+valid_preposition(PREDICATE_NAME, _),
    DrsIn = drs(Referents, Conditions),
    append(ReferentList, Referents, RefsOut),
    encapsuleListInFunctor(ReferentList, PREDICATE_NAME, NewConditions),
    append(NewConditions, Conditions, CondsOut),
    DrsOut = drs(RefsOut, CondsOut)
  }.

sSentenceSuffix(adjective, DrsIn, ReferentList, DrsOut) -->
  is_are,
  [PREDICATE_NAME],
  structPrepositionPhrase(PREDICATE_NAME, sSentence, DrsIn, ReferentList, DrsOut).

structPrepositionPhrase(PRED, sSentence, DrsIn, ReferentList, DrsOut) -->
  [PREPOSITION],
  idList([], MoreReferents),
  {
    valid_predicate(PRED),
    valid_preposition(PRED, PREPOSITION)
  }.

sSentence --> determiner, [FUNCTION_NAME], lit_of, argList(tSentence), lit_is, [FUNCTION_VAL], {verify_not_reserved(FUNCTION_NAME), verify_not_reserved(FUNCTION_VAL)}.

%TODO: Refactor repeating extra arguments.