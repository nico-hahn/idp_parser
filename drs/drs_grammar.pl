%-----------------------------
% SENTENCE STRUCTURE

:- consult('../util/literals').
:- [drs_general].
:- [drs_structure].
:- [drs_theory].
:- [drs_vocabulary].

% a sentence can be: a super-theory-sentence, a vocabulary-sentence, a structure-sentence, or a definition.
% every sentence ends with a '.'
sentence(DrsIn, DrsOut) -->
  sentenceComponent(DrsIn, DrsOut),
  lit_period,
  { write(DrsOut) }.

sentenceComponent(DrsIn, DrsOut) -->
  vSentence(DrsIn, DrsOut);
  superTSentence(DrsIn, DrsOut);
  implication(DrsIn, DrsOut);
  sSentence(DrsIn, DrsOut);
  definition(DrsIn, DrsOut);
  quantified_implication(DrsIn, DrsOut).

superTSentence(DrsIn, DrsOut) -->
  tSentence(DrsIn, DrsOut).

quantified_implication(DrsIn, DrsOut) -->
  lit_for,
  arglist(tSentence, notempty, DrsIn, DrsNext),
  lit_comma,
  implication(DrsNext, DrsOut).

% IMPLICATIONS
% TODO: This Drs threading implementation is currently wrong.
implication(DrsIn, DrsOut) -->
  lit_if,
  superTSentence(DrsIn, DrsNext),
  lit_comma,
  lit_then,
  superTSentence(DrsNext, DrsOut).

implication(DrsIn, DrsOut) -->
  superTSentence(DrsIn, DrsNext),
  lit_comma,
  lit_if,
  superTSentence(DrsNext, DrsOut).

% DEFINITIONS
definition(DrsIn, DrsOut) -->
  lit_def_begin,
  sentenceLoop(DrsIn, DrsOut),
  lit_def_end.

sentenceLoop(_, _) --> [].
sentenceLoop(DrsIn, DrsOut) -->
  superTSentence(DrsIn, DrsNext),
  lit_period,
  sentenceLoop(DrsNext, DrsOut).

%-----------------------------
% ASSERTIONS

:- consult('../util/lexicon_checker').
:- dynamic type/1.
:- dynamic function/1.
:- dynamic valid_predicate/1.
:- dynamic valid_preposition/2.

myAssert(function(F_NAME), _) :-
  assertz(function(F_NAME)).

myAssert(type(TYPE_PL), noun) :- 
  noun(TYPE_SG, TYPE_PL),
  assertz(type(TYPE_SG)),
  assertz(type(TYPE_PL)).

myAssert(valid_predicate(PRED), adjective) :-
  assertz(valid_predicate(PRED)).

myAssert(valid_preposition(PRED, PREP), adjective) :-
  assertz(valid_preposition(PRED, PREP)).

myAssert(valid_predicate(PRED), verb):-
  verb(PRED_SFORM, PRED),
  assertz(valid_predicate(PRED_SFORM)),
  assertz(valid_predicate(PRED)).

myAssert(valid_preposition(PRED, PREP), verb) :-
  verb(PRED_SFORM, PRED),
  assertz(valid_preposition(PRED_SFORM, PREP)),
  assertz(valid_preposition(PRED, PREP)).