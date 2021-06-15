% TODO: Put common extra goals into a predicate

tSentence(DrsIn, DrsOutNeg) -->
  argList(tSentence, notempty, RefList1),
  is_are,
  neg(adjective),
  predicate(tSentence, _, Arity, DrsNext, DrsOut),
  arglist(tSentence, empty, RefList2),
  {
    DrsIn = drs(RefsIn, CondsIn),
    DrsNext = drs(RefsNext, CondsIn),
    % Order of RefsIn and NewRefs is semantically important (e.g. parents(x, y, z), where z is the child)
    append(RefList1, RefList2, ReferentList),
    append(ReferentList, RefsIn, RefsNext),
    length(ReferentList, Arity),
    DrsOut = drs(RefsOutNeg, [CondNeg|CondsOutRest]),
    DrsOutNeg = drs(RefsOutNeg, [drsNeg(drs([], [CondNeg]))|CondsOutRest])
  }.

tSentence(DrsIn, DrsOut) -->
  argList(tSentence, notempty, RefList1),
  is_are,
  predicate(tSentence, _, Arity, DrsNext, DrsOut),
  argList(tSentence, empty, RefList2),
  {
    DrsIn = drs(RefsIn, CondsIn),
    DrsNext = drs(RefsOut, CondsIn),
    % Order of RefsIn and NewRefs is semantically important (e.g. parents(x, y, z), where z is the child)
    append(RefList1, RefList2, ReferentList),
    append(ReferentList, RefsIn, RefsOut),
    length(ReferentList, Arity)
  }.

tSentence(DrsIn, DrsOutNeg) -->
  argList(tSentence, notempty, RefList1),
  neg(verb),
  predicate(tSentence, _, Arity, DrsNext, DrsOut),
  argList(tSentence, empty, RefList2),
  {
    DrsIn = drs(RefsIn, CondsIn),
    DrsNext = drs(RefsOut, CondsIn),
    % Order of RefsIn and NewRefs is semantically important (e.g. parents(x, y, z), where z is the child)
    append(RefList1, RefList2, ReferentList),
    append(ReferentList, RefsIn, RefsOut),
    length(ReferentList, Arity),
    DrsOut = drs(RefsOutNeg, [CondNeg|CondsOutRest]),
    DrsOutNeg = drs(RefsOutNeg, [drsNeg(drs([], [CondNeg]))|CondsOutRest])
  }.

tSentence(DrsIn, DrsOutNeg) -->
  argList(tSentence, notempty, RefList1),
  is_are,
  neg(noun),
  determiner,
  predicate(tSentence, _, Arity, DrsNext, DrsOut),
  argList(tSentence, empty, RefList2),
  {
    DrsIn = drs(RefsIn, CondsIn),
    DrsNext = drs(RefsOut, CondsIn),
    % Order of RefsIn and NewRefs is semantically important (e.g. parents(x, y, z), where z is the child)
    append(RefList1, RefList2, ReferentList),
    append(ReferentList, RefsIn, RefsOut),
    length(ReferentList, Arity),
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
    DrsNext = drs(RefsNext, CondsIn)
  }.

superTSentence -->
  tSentence,
  log_connective,
  superTSentence.

log_connective --> cc; cc_or.

% predicates
predicate(SentenceType, WordType, Arity, DrsIn, DrsOut) -->
  [PREDICATE_NAME],
  prepositional_phrase(PREDICATE_NAME, SentenceType, WordType),
  {
    verify_not_reserved(PREDICATE_NAME),
    validate_predicate(SentenceType, PREDICATE_NAME, Arity, WordType),
    DrsIn = drs(RefsIn, CondsIn),
    buildDrsPredicate(PREDICATE_NAME, RefsIn, Condition),
    DrsOut = drs(RefsIn, [Condition|CondsIn])
  }.

validate_predicate(vSentence, P, Arity, WordType) :-
  myAssert(valid_predicate(P, Arity), WordType), 
  verify_not_reserved(P).

validate_predicate(tSentence, P, Arity, _) :-
  valid_predicate(P, Arity),
  verify_not_reserved(P).

prepositional_phrase(_, vSentence, _) --> [].

prepositional_phrase(P, tSentence, _) -->
  [],
  { % if a predicate is mentioned without a preposition, a preposition must not be declared for the predicate.
    \+valid_preposition(P, _)
  }.

prepositional_phrase(P, SentenceType, WordType) -->
  [PREPOSITION],
  {
    validate_preposition(SentenceType, P, PREPOSITION, WordType)
  }.

validate_preposition(vSentence, P, PREP, WordType) :-
  myAssert(valid_preposition(P, PREP), WordType).

validate_preposition(tSentence, P, PREP, _) :-
  valid_preposition(P, PREP).

% functions
tSentence -->
  [IDENTIFIER],
  functionOperatorPhrase,
  determiner,
  [FUNCTION_NAME],
  lit_of,
  argList(tSentence),
  {
    verify_not_reserved(IDENTIFIER),
    function(FUNCTION_NAME)
  }.
functionOperatorPhrase --> lit_is.
functionOperatorPhrase --> lit_is, lit_not.
functionOperatorPhrase --> lit_is, lit_different, lit_from.
