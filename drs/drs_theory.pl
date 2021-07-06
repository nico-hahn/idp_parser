% TODO: Put common extra goals into a predicate

tSentence(DrsIn, DrsOutNeg) -->
  argList(tSentence, RefList1),
  is_are,
  neg(adjective),
  predicate(tSentence, _, RefList1, DrsIn, DrsOut),
  {
    DrsOut = drs(RefsOut, [CondNeg|CondsOutRest]),
    DrsOutNeg = drs(RefsOut, [drsNeg(drs([], [CondNeg]))|CondsOutRest])
  }.

tSentence(DrsIn, DrsOut) -->
  argList(tSentence, RefList1),
  is_are,
  predicate(tSentence, _, RefList1, DrsIn, DrsOut).

tSentence(DrsIn, DrsOutNeg) -->
  argList(tSentence, RefList1),
  neg(verb),
  predicate(tSentence, _, RefList1, DrsIn, DrsOut),
  {
    DrsOut = drs(RefsOut, [CondNeg|CondsOutRest]),
    DrsOutNeg = drs(RefsOut, [drsNeg(drs([], [CondNeg]))|CondsOutRest])
  }.

tSentence(DrsIn, DrsOutNeg) -->
  argList(tSentence, RefList1),
  is_are,
  neg(noun),
  determiner,
  predicate(tSentence, _, RefList1, DrsIn, DrsOut),
  {
    DrsOut = drs(RefsOut, [CondNeg|CondsOutRest]),
    DrsOutNeg = drs(RefsOut, [drsNeg(drs([], [CondNeg]))|CondsOutRest])
  }.

neg(verb) --> lit_does, lit_not.
neg(verb) --> lit_do, lit_not.
neg(_) --> lit_not.

superTSentence(DrsIn, DrsOut) -->
  tSentence(DrsIn, DrsOut).

superTSentence(DrsIn, DrsOut) -->
  lit_for,
  argList(tSentence, ReferentList),
  lit_comma,
  tSentence(DrsNext, DrsOut),
  {
    DrsIn = drs(RefsIn, CondsIn),
    append(ReferentList, RefsIn, RefsNext),
    DrsNext = drs(RefsNext, CondsIn)
  }.

superTSentence(DrsIn, DrsOut) -->
  tSentence(DrsIn, DrsNext),
  cc,
  superTSentence(DrsNext, DrsOut).

superTSentence(DrsIn, DrsOut) -->
  tSentence(DrsIn, DrsNext1),
  cc_or,
  superTSentence(drs([], []), DrsNext2),
  {
    DrsOut = drs([], drsOr(DrsNext1, DrsNext2))
  }.

% predicates
predicate(SentenceType, WordType, RefList1, DrsIn, DrsOut) -->
  [Predicate],
  prepositional_phrase(Predicate, SentenceType, WordType),
  argList(SentenceType, empty, RefList2),
  {
    DrsIn = drs(RefsIn, CondsIn),
    verify_not_reserved(Predicate),
    append(RefList1, RefList2, Referents),
    length(Referents, Arity),
    validate_predicate(SentenceType, Predicate, Arity, WordType),
    buildDrsPredicate(Predicate, Referents, Condition),
    remove_intersection(Referents, RefsIn, RefsNew),
    append(RefsNew, RefsIn, RefsOut),
    DrsOut = drs(RefsOut, [Condition|CondsIn])
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
    verify_not_reserved(P),
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
