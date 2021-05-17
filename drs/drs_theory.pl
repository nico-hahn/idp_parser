% TODO: Put common extra goals into a predicate

tSentence(DrsIn, DrsOutNeg) -->
  argList(tSentence, notempty, [], ReferentList),
  is_are,
  neg(adjective),
  predicate(tSentence, _, DrsNext, DrsOut),
  {
    DrsIn = drs(RefsIn, CondsIn),
    DrsNext = drs(RefsNext, CondsIn),
    append(ReferentList, RefsIn, RefsNext),
    DrsOut = drs(RefsOutNeg, [CondNeg|CondsOutRest]),
    DrsOutNeg = drs(RefsOutNeg, [drsNeg(drs([], [CondNeg]))|CondsOutRest])
  }.

tSentence(DrsIn, DrsOut) -->
  argList(tSentence, notempty, [], ReferentList),
  is_are,
  predicate(tSentence, _, DrsNext, DrsOut),
  {
    DrsIn = drs(RefsIn, CondsIn),
    DrsNext = drs(RefsOut, CondsIn),
    append(ReferentList, RefsIn, RefsOut)
  }.

tSentence(DrsIn, DrsOutNeg) -->
  argList(tSentence, notempty, [], ReferentList),
  neg(verb),
  predicate(tSentence, _, DrsNext, DrsOut),
  {
    DrsIn = drs(RefsIn, CondsIn),
    DrsNext = drs(RefsOut, CondsIn)
    append(ReferentList, RefsIn, RefsOut),
    DrsOut = drs(RefsOutNeg, [CondNeg|CondsOutRest]),
    DrsOutNeg = drs(RefsOutNeg, [drsNeg(drs([], [CondNeg]))|CondsOutRest])
  }.

tSentence(DrsIn, DrsOutNeg) -->
  argList(tSentence, notempty, [], ReferentList),
  is_are,
  neg(noun),
  determiner,
  predicate(tSentence, _, DrsNext, DrsOut),
  {
    DrsIn = drs(RefsIn, CondsIn),
    DrsNext = drs(RefsOut, CondsIn),
    append(ReferentList, RefsIn, RefsOut),
    DrsOut = drs(RefsOutNeg, [CondNeg|CondsOutRest]),
    DrsOutNeg = drs(RefsOutNeg, [drsNeg(drs([], [CondNeg]))|CondsOutRest])
  }.

neg(verb) --> lit_does, lit_not.
neg(verb) --> lit_do, lit_not.
neg(_) --> lit_not.

superTSentence(DrsIn, DrsOut) -->
  lit_for,
  arglist(tSentence, notempty, [], ReferentList),
  lit_comma,
  tSentence(DrsNext, DrsOut),
  {
    DrsIn = drs(RefsIn, CondsIn),
    append(ReferentList, RefsIn, RefsNext),
    DrsNext(RefsNext, CondsIn)
  }.

superTSentence -->
  tSentence,
  log_connective,
  superTSentence.

log_connective --> cc; cc_or.

% predicates
predicate(ST, WORD_TYPE, DrsIn, DrsOut) -->
  [PREDICATE_NAME],
  prepositional_phrase(PREDICATE_NAME, ST, WORD_TYPE),
  argList(ST, empty, [], NewRefs),
  {
    verify_not_reserved(PREDICATE_NAME),
    validate_predicate(ST, PREDICATE_NAME, WORD_TYPE),
    DrsIn = drs(RefsIn, CondsIn),
    % Order of RefsIn and NewRefs is semantically important (e.g. parents(x, y, z), where z is the child)
    append(RefsIn, NewRefs, RefsOut),
    buildDrsPredicate(PREDICATE_NAME, RefsOut, Condition),
    DrsOut = drs(RefsOut, [Condition|_])
  }.

validate_predicate(vSentence, P, WORD_TYPE) :-
  myAssert(valid_predicate(P), WORD_TYPE), 
  verify_not_reserved(P).

validate_predicate(tSentence, P, _) :-
  valid_predicate(P),
  verify_not_reserved(P).

prepositional_phrase(_, vSentence, _) --> [].

prepositional_phrase(P, tSentence, _) -->
  [],
  { % if a predicate is mentioned without a preposition, a preposition must not be declared for the predicate.
    \+valid_preposition(P, _)
  }.

prepositional_phrase(P, ST, WORD_TYPE) -->
  [PREPOSITION],
  {
    validate_preposition(ST, P, PREPOSITION, WORD_TYPE)
  }.

validate_preposition(vSentence, P, PREP, WORD_TYPE) :-
  myAssert(valid_preposition(P, PREP), WORD_TYPE).

validate_preposition(tSentence, P, PREP, _) :-
  valid_preposition(P, PREP).

% functions
tSentence --> [IDENTIFIER], functionOperatorPhrase, determiner, [FUNCTION_NAME], lit_of, argList(tSentence), {verify_not_reserved(IDENTIFIER), function(FUNCTION_NAME)}.
functionOperatorPhrase --> lit_is.
functionOperatorPhrase --> lit_is, lit_not.
functionOperatorPhrase --> lit_is, lit_different, lit_from.
