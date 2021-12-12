:- use_module(library(lists)).

argList(_, []) --> [].

argList(sSentence, [Identifier|Refs]) --> 
  [Identifier],
  moreArgs(sSentence, Refs), 
  {
    verify_not_reserved(Identifier)
    % TODO: Make sure that all members of RefsOut and Identifier are distinct.
  }.

argList(vSentence, [TypeName|Refs]) -->
  determiner,
  [TypeName],
  moreArgs(vSentence, Refs),
  {
    type(TypeName)
  }.

argList(tSentence, [Identifier|Refs]) -->
  optional_det_phrase,
  arglist_predicates(Identifier, Conditions),
  [Identifier],
  moreArgs(tSentence, Refs),
  {
    verify_not_reserved(Identifier)
    % TODO: Make sure that all members of RefsOut and Identifier are distinct.
  }.

moreArgs(_, []) --> [].
moreArgs(SentenceType, Refs) -->
  cc,
  argList(SentenceType, Refs).

optional_det_phrase --> [].
optional_det_phrase -->
  determiner,
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