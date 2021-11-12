%-----------------------------
% SENTENCE STRUCTURE

:- consult('../util/literals').
:- [drs_general].
:- [drs_structure].
:- [drs_theory].
:- [drs_vocabulary].

metaSentence(Section) -->
  metaSentence(user_output, Section, DrsOutput).

metaSentence(Stream, Section, DrsOutput) -->
  {
    log(['...', parsing, Section, '...'])
  },
  sentences(Stream, Section, DrsOutput),
  {
    drs2idp(Section, DrsOutput, IdpCode),
    log(['IDP Code for ', Section, ':', IdpCode]),
    writeln(Stream, IdpCode)
  }.

sentences(Stream, Section, [Output|Outputs]) --> 
  sentence(Section, Output),
  sentences(Stream, Section, Outputs).
sentences(_, _, []) --> [].
sentences(_, procedure, _) --> [].
% a sentence can be: a super-theory-sentence, a vocabulary-sentence, a structure-sentence, or a definition.
% every sentence ends with a '.'
sentence(Section, DrsOut) --> sentence(Section, DrsOut, user_output).
sentence(Section, DrsOut, Stream) -->
  sentenceComponent(Section, DrsOut),
  lit_period,
  {
    log([Section, ':', DrsOut])
  }.

sentenceComponent(vocabulary, DrsOut) --> vSentence(DrsOut). % Vocabulary sentences don't produce DRS

sentenceComponent(structure, DrsOut) -->
  sSentence(drs([], []), DrsOut).

sentenceComponent(theory, DrsOut) -->
  definition(drs([], []), DrsOut);
  theoryComponent(drs([], []), DrsOut).

theoryComponent(DrsIn, DrsOut) -->
  superTSentence(DrsIn, DrsOut);
  implication(DrsIn, DrsOut);
  quantified_implication(DrsIn, DrsOut);
  every_quantification(DrsIn, DrsOut).

quantified_implication(DrsIn, DrsImp) -->
  lit_for,
  quantifier,
  argList(sSentence, notempty, ReferentList),
  lit_comma,
  implication(DrsNext, DrsImp),
  {
    DrsIn = drs(RefsIn, CondsIn),
    append(RefsIn, ReferentList, RefsNext),
    DrsNext = drs(RefsNext, CondsIn)
  }.

% QUANTIFICATIONS
every_quantification(DrsIn, DrsOut) -->
  quantifier,
  [TypeName],
  [Identifier],
  is_are_optional,
  predicate(tSentence, _, [Identifier], DrsIn, DrsNext),
  {
    type(TypeName),
    verify_not_reserved(Identifier),
    buildDrsPredicate(TypeName, [Identifier], TypeCondition),
    DrsNext = drs(RefsNext, CondsNext),
    remove_intersection(RefsNext, [Identifier], RefsConsequent),
    DrsOut = drs(
      [],
      [
        drsImpl(
          drs([Identifier], [TypeCondition]),
          drs(RefsConsequent, CondsNext)
        )
      ]
    )
  }.


% IMPLICATIONS
implication(DrsIn, DrsOut) -->
  lit_if,
  superTSentence(drs([],[]), DrsAntecedent),
  lit_comma,
  lit_then,
  superTSentence(drs([],[]), DrsConsequent),
  {
    buildDrsImplication(DrsIn, DrsAntecedent, DrsConsequent, DrsOut)
  }.

implication(DrsIn, DrsOut) -->
  superTSentence(drs([],[]), DrsConsequent),
  lit_comma,
  lit_if,
  superTSentence(drs([],[]), DrsAntecedent),
  {
    buildDrsImplication(DrsIn, DrsAntecedent, DrsConsequent, DrsOut)
  }.

% DEFINITIONS
definition(DrsIn, DrsOut) -->
  lit_def_begin,
  sentenceLoop(DrsOuts),
  lit_def_end,
  {
    DrsIn = drs(RefsIn, CondsIn),
    append([definition|DrsOuts], CondsIn, CondsOut),
    DrsOut = drs(RefsIn, CondsOut)
  }.

sentenceLoop([]) --> [].
sentenceLoop(DrsOuts) -->
  theoryComponent(drs([], []), DrsNext),
  lit_period,
  sentenceLoop(DrsO),
  {
    DrsOuts = [DrsNext|DrsO]
  }.

%-----------------------------
% ASSERTIONS

:- consult('../util/lexicon_checker').
:- dynamic type/1.
:- dynamic function/1.
:- dynamic valid_predicate/2.
:- dynamic valid_preposition/2.

myAssert(function(F_NAME), _) :-
  assertz(function(F_NAME)).

myAssert(type(TYPE_PL), noun) :- 
  log(['asserting type:', TYPE_PL]),
  noun(TYPE_SG, TYPE_PL),
  assertz(type(TYPE_SG)),
  assertz(type(TYPE_PL)).

myAssert(valid_predicate(PRED, Arity), WordType) :-
  member(WordType, [adjective, noun]),
  assert_predicate(PRED, Arity).

myAssert(valid_preposition(PRED, PREP), WordType) :-
  member(WordType, [adjective, noun]),
  assertz(valid_preposition(PRED, PREP)).

myAssert(valid_predicate(PRED, Arity), verb):-
  verb(PRED_SFORM, PRED),
  assert_predicate(PRED_SFORM, Arity),
  assert_predicate(PRED, Arity).

myAssert(valid_preposition(PRED, PREP), verb) :-
  verb(PRED_SFORM, PRED),
  assertz(valid_preposition(PRED_SFORM, PREP)),
  assertz(valid_preposition(PRED, PREP)).

% If predicate exists and new arity is larger, then replace
assert_predicate(Predicate, Arity) :-
  valid_predicate(Predicate, X),
  number(X),
  Arity > X,
  retract(valid_predicate(Predicate, X)),
  log(['asserting predicate:', Predicate, '/', Arity]),
  assertz(valid_predicate(Predicate, Arity)).

% If nothing of the above, assert the predicate
assert_predicate(Predicate, Arity) :-
  \+ valid_predicate(Predicate, _),
  log(['asserting predicate:', Predicate, '/', Arity]),
  assertz(valid_predicate(Predicate, Arity)).