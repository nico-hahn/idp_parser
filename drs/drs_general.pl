% generic arglist rules
argList(_, empty, _) --> [].

argList(vSentence, _, _) -->
  determiner,
  [TypeName],
  moreArgs(vSentence, _),
  {
    type(TypeName)
  }.

argList(tSentence, _, [Identifier|RefsOut]) -->
  optional_det_phrase,
  [Identifier],
  moreArgs(tSentence, RefsOut),
  {
    verify_not_reserved(Identifier)
    % TODO: Make sure that all members of RefsOut and Identifier are distinct.
  }.

moreArgs(_, _) --> [].
moreArgs(X, RefsOut) -->
  cc,
  argList(X, notempty, RefsOut).

optional_det_phrase --> [].
optional_det_phrase -->
  determiner,
  [TypeName],
  {
    type(TypeName)
  }.