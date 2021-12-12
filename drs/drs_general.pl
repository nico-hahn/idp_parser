:- use_module(library(lists)).

argList(SentenceType, Refs) -->
  argList(SentenceType, Refs, _).

argList(_, [], []) --> [].

argList(sSentence, [Identifier|Refs], _) --> 
  [Identifier],
  moreArgs(sSentence, Refs, _), 
  {
    verify_not_reserved(Identifier)
    % TODO: Make sure that all members of RefsOut and Identifier are distinct.
  }.

argList(vSentence, [TypeName|Refs], _) -->
  determiner,
  [TypeName],
  moreArgs(vSentence, Refs, _),
  {
    type(TypeName)
  }.

argList(tSentence, [Identifier|Refs], ConditionsOut) -->
  determiner_optional,
  arglist_predicates(Identifier, NewConditions),
  optional_type,
  [Identifier],
  moreArgs(tSentence, Refs, Conditions),
  {
    verify_not_reserved(Identifier),
    append(NewConditions, Conditions, ConditionsOut)
    % TODO: Make sure that all members of RefsOut and Identifier are distinct.
  }.

moreArgs(_, [], []) --> [].
moreArgs(SentenceType, Refs, Conditions) -->
  cc,
  argList(SentenceType, Refs, Conditions).

optional_det_phrase --> [].
optional_det_phrase -->
  determiner_optional,
  optional_type.
optional_type --> [].
optional_type -->
  [TypeName],
  {
    type(TypeName)
  }.

arglist_predicates(_, []) --> [].
arglist_predicates(Identifier, [Condition|Conditions]) -->
  [Predicate],
  arglist_predicates(Identifier, Conditions),
  {
    valid_predicate(Predicate, 1),
    verify_not_reserved(Predicate),
    buildDrsPredicate(Predicate, [Identifier], Condition)
  }.