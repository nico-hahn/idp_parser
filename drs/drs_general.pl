% generic arglist rules

argList(SentenceType, RefsOut) -->
  argList(SentenceType, notempty, RefsOut).

argList(SentenceType, EmptyAllowed, RefsOut) -->
  argList(SentenceType, EmptyAllowed, [], RefsOut).

argList(_, empty, _, []) --> [].

argList(sSentence, _, RefsIn, RefsOut) --> 
  [Identifier],
  moreArgs(sSentence, [Identifier|RefsIn], RefsOut), 
  {
    verify_not_reserved(Identifier)
    % TODO: Make sure that all members of RefsOut and Identifier are distinct.
  }.

argList(vSentence, _, RefsIn, RefsOut) -->
  determiner,
  [TypeName],
  moreArgs(vSentence, [TypeName|RefsIn], RefsOut),
  {
    type(TypeName)
  }.

argList(tSentence, _, RefsIn, RefsOut) -->
  optional_det_phrase,
  [Identifier],
  moreArgs(tSentence, [Identifier|RefsIn], RefsOut),
  {
    verify_not_reserved(Identifier)
    % TODO: Make sure that all members of RefsOut and Identifier are distinct.
  }.

moreArgs(_, X, X) --> [].
moreArgs(X, RefsIn, RefsOut) -->
  cc,
  argList(X, notempty, RefsIn, RefsOut).

optional_det_phrase --> [].
optional_det_phrase -->
  determiner,
  [TypeName],
  {
    type(TypeName)
  }.