lit_comma --> [','].
lit_period --> ['.'].
lit_if --> ['if'].
lit_then --> ['then'].
lit_def_begin --> ['we define the following:'].
lit_def_end --> ['<def. end>'].
lit_there --> ['there'].
lit_are --> ['are'].
lit_is --> ['is'].
lit_can --> ['can'].
lit_be --> ['be'].
lit_has --> ['has'].
lit_for --> ['for'].
lit_does --> ['does'].
lit_do --> ['do'].
lit_not --> ['not'].
lit_the --> ['the'].
cc --> ['and'].
cc_or --> ['or'].
lit_different --> ['different'].
lit_from --> ['from'].
lit_of --> ['of'].
lit_to --> ['to'].
determiner --> ['the']; ['a']; ['an']; ['another']; ['some'].
quantifier --> ['each']; ['every']; ['all'].
rp --> ['that']; ['which']; ['who'].

quant_det --> quantifier.
quant_det --> determiner.

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
vSentence --> lit_there, lit_are, [TYPE_NAME], {verify_not_reserved(TYPE_NAME), assertz(type(TYPE_NAME))}.

% generic arglist rules
argList(_, empty) --> [].
argList(vSentence, _) --> determiner, [TYPE_NAME], moreArgs(vSentence), {type(TYPE_NAME)}.
argList(tSentence, _) --> optional_det_phrase, [IDENTIFIER], moreArgs(tSentence), {verify_not_reserved(IDENTIFIER)}.
optional_det_phrase --> determiner, [TYPE_NAME], {type(TYPE_NAME)}.
optional_det_phrase --> [].
moreArgs(X) --> cc, argList(X, notempty).
moreArgs(_) --> [].

% predicate declarations
vSentence --> argList(vSentence, notempty), lit_can, lit_be, predicate(vSentence).
vSentence --> argList(vSentence, notempty), lit_can, predicate(vSentence).

% predicates in use
tSentence --> argList(tSentence, notempty), is_are, optional_neg(adjective), predicate(tSentence).
tSentence --> argList(tSentence, notempty), optional_neg(verb), predicate(tSentence).
tSentence --> argList(tSentence, notempty), is_are, optional_neg(noun), determiner, predicate(tSentence).
is_are --> lit_is; lit_are.
optional_neg(TYPE) --> []; neg(TYPE).
neg(verb) --> lit_does, lit_not.
neg(verb) --> lit_do, lit_not.
neg(_) --> lit_not.

superTSentence --> lit_for, arglist(tSentence, notempty), lit_comma, tSentence.

superTSentence --> tSentence, log_connective, superTSentence.
log_connective --> cc; cc_or.

% predicate specifications
predicate(ST) --> [PREDICATE_NAME], prepositional_phrase(PREDICATE_NAME, ST), argList(ST, empty), {validate_predicate(ST, PREDICATE_NAME)}.
validate_predicate(vSentence, P) :- assertz(valid_predicate(P)), verify_not_reserved(P).
validate_predicate(tSentence, P) :- valid_predicate(P), verify_not_reserved(P).

prepositional_phrase(_, vSentence) --> [].
prepositional_phrase(P, tSentence) --> [], {\+valid_preposition(P, _)}. % if a predicate is mentioned without a preposition, a preposition must not be declared for the predicate.
prepositional_phrase(P, ST) --> [PREPOSITION], {validate_preposition(ST, P, PREPOSITION)}.
validate_preposition(vSentence, P, PREP) :- assertz(valid_preposition(P, PREP)).
validate_preposition(tSentence, P, PREP) :- valid_preposition(P, PREP).

%----------------------------
% Structure production

idList --> [IDENTIFIER], moreIdList, {verify_not_reserved(IDENTIFIER)}.
moreIdList --> [].
moreIdList --> cc, idList.

sSentence --> [IDENTIFIER], lit_is, determiner, [TYPE_NAME], {verify_not_reserved(IDENTIFIER), verify_not_reserved(TYPE_NAME), type(TYPE_NAME)}.
sSentence --> idList, lit_are, [TYPE_NAME], {verify_not_reserved(TYPE_NAME), type(TYPE_NAME)}.
sSentence --> [SEQUENCE_ID_1], lit_to, [SEQUENCE_ID_2], lit_are, [TYPE_NAME],
  {verify_not_reserved(SEQUENCE_ID_1), verify_not_reserved(SEQUENCE_ID_2), verify_not_reserved(TYPE_NAME), type(TYPE_NAME)}.

sSentence --> idList, [VERB_PREDICATE], idList, {verify_not_reserved(VERB_PREDICATE), valid_predicate(VERB_PREDICATE)}.
sSentence --> idList, is_are, optional_det, [ADJECTIVE_PREDICATE], preposition, idList, 
  {verify_not_reserved(ADJECTIVE_PREDICATE), valid_predicate(ADJECTIVE_PREDICATE)}.
optional_det --> []; determiner.