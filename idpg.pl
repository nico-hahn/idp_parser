:- [lexicon_checker].

lit_comma --> [','].
lit_period --> ['.'].
lit_if --> [if].
lit_then --> [then].
lit_def_begin --> ['we define the following:'].
lit_def_end --> ['<def. end>'].
lit_there --> [there].
lit_are --> [are].
lit_is --> [is].
lit_can --> [can].
lit_be --> [be].
lit_has --> [has].
lit_for --> [for].
lit_does --> [does].
lit_do --> [do].
lit_not --> [not].
lit_the --> [the].
cc --> [and].
cc_or --> [or].
lit_different --> [different].
lit_from --> [from].
determiner --> [the]; [a]; [an]; [another]; [some].
quantifier --> [each]; [every]; [all].
rp --> [that]; [which]; [who].
is_are --> lit_is; lit_are.

quant_det --> quantifier ; determiner.

% This predicate is used to ensure that no user-input
% matches a key word
reserved_words -->
  cc; cc_or; determiner; quantifier;
  rp; lit_comma, lit_period; lit_if; lit_then; lit_def_begin;
  lit_def_end; lit_there; lit_are; lit_is; lit_can; lit_be; lit_has;
  lit_for; lit_does; lit_do; lit_not; lit_the.

verify_not_reserved(WORD) :- \+ reserved_words([WORD], []). 

% just a short hand for testing comfort
s --> sentence.

% a sentence can be: a super-theory-sentence, a vocabulary-sentence, a structure-sentence, or a definition.
% every sentence ends with a '.'
sentence --> sentenceComponent, lit_period.
sentenceComponent --> vSentence; superTSentence; implication; sSentence; definition; quantified_implication.
superTSentence --> tSentence.
quantified_implication --> lit_for, arglist(tSentence, notempty), lit_comma, implication.

% IMPLICATIONS
implication --> lit_if, superTSentence, lit_comma, lit_then, superTSentence.
implication --> superTSentence, lit_comma, lit_if, superTSentence.

% DEFINITIONS
definition --> lit_def_begin, sentenceLoop, lit_def_end.
sentenceLoop --> [].
sentenceLoop --> superTSentence, lit_period, sentenceLoop.

%--------------------------------------------------

% type delcarations
vSentence --> lit_there, lit_are, [TYPE_NAME], {verify_not_reserved(TYPE_NAME), myAssert(type(TYPE_NAME), noun)}.

% generic arglist rules
argList(_, empty) --> [].
argList(vSentence, _) --> determiner, [TYPE_NAME], moreArgs(vSentence), {type(TYPE_NAME)}.
argList(tSentence, _) --> optional_det_phrase, [IDENTIFIER], moreArgs(tSentence), {verify_not_reserved(IDENTIFIER)}.
optional_det_phrase --> determiner, [TYPE_NAME], {type(TYPE_NAME)}.
optional_det_phrase --> [].
moreArgs(X) --> cc, argList(X, notempty).
moreArgs(_) --> [].

% predicate declarations
vSentence --> argList(vSentence, notempty), lit_can, lit_be, predicate(vSentence, adjective).
vSentence --> argList(vSentence, notempty), lit_can, predicate(vSentence, verb).

% predicates in use
tSentence --> argList(tSentence, notempty), is_are, optional_neg(adjective), predicate(tSentence, _).
tSentence --> argList(tSentence, notempty), optional_neg(verb), predicate(tSentence, _).
tSentence --> argList(tSentence, notempty), is_are, optional_neg(noun), determiner, predicate(tSentence, _).
optional_neg(TYPE) --> []; neg(TYPE).
neg(verb) --> lit_does, lit_not.
neg(verb) --> lit_do, lit_not.
neg(_) --> lit_not.

superTSentence --> lit_for, arglist(tSentence, notempty), lit_comma, tSentence.

superTSentence --> tSentence, log_connective, superTSentence.
log_connective --> cc; cc_or.

% predicate specifications
predicate(ST, WORD_TYPE) --> [PREDICATE_NAME], prepositional_phrase(PREDICATE_NAME, ST, WORD_TYPE), argList(ST, empty), {validate_predicate(ST, PREDICATE_NAME, WORD_TYPE)}.
validate_predicate(vSentence, P, WORD_TYPE) :- myAssert(valid_predicate(P), WORD_TYPE), verify_not_reserved(P).
validate_predicate(tSentence, P, _) :- valid_predicate(P), verify_not_reserved(P).

prepositional_phrase(_, vSentence, _) --> [].
prepositional_phrase(P, tSentence, _) --> [], {\+valid_preposition(P, _)}. % if a predicate is mentioned without a preposition, a preposition must not be declared for the predicate.
prepositional_phrase(P, ST, WORD_TYPE) --> [PREPOSITION], {validate_preposition(ST, P, PREPOSITION, WORD_TYPE)}.
validate_preposition(vSentence, P, PREP, WORD_TYPE) :- myAssert(valid_preposition(P, PREP), WORD_TYPE).
validate_preposition(tSentence, P, PREP, _) :- valid_preposition(P, PREP).

%----------------------------
% Structure production

optionalIdList --> idList; [].
idList --> [IDENTIFIER], moreIdList, {verify_not_reserved(IDENTIFIER)}.
moreIdList --> [].
moreIdList --> cc, idList.

sSentence --> sSentenceTyped(noun); sSentenceTyped(verb); sSentenceTyped(adjective).
sSentenceTyped(TYPE) --> idList, sSentenceSuffix(TYPE).
sSentenceSuffix(noun) --> lit_is, determiner, [TYPE_NAME], {type(TYPE_NAME)}.
sSentenceSuffix(noun) --> lit_are, [TYPE_NAME], {type(TYPE_NAME)}.
sSentenceSuffix(verb) --> [PREDICATE_NAME], optionalIdList, {valid_predicate(PREDICATE_NAME), \+valid_preposition(PREDICATE_NAME, _)}.
sSentenceSuffix(verb) --> [PREDICATE_NAME], prepositional_phrase(PREDICATE_NAME, sStentence, _).
sSentenceSuffix(adjective) --> is_are, [PREDICATE_NAME], {valid_predicate(PREDICATE_NAME), \+valid_preposition(PREDICATE_NAME, _)}.
sSentenceSuffix(adjective) --> is_are, [PREDICATE_NAME], prepositional_phrase(PREDICATE_NAME, sSentence, _).
prepositional_phrase(PRED, sSentence, _) --> [PREPOSITION], idList, {valid_predicate(PRED), valid_preposition(PRED, PREPOSITION)}.

% ASSERTIONS

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