% generic arglist rules
argList(_, empty, _) --> [].

argList(_, _, []) --> [].

argList(vSentence, _, _) -->
  determiner,
  [TYPE_NAME],
  moreArgs(vSentence, _, _),
  {
    type(TYPE_NAME)
  }.

argList(tSentence, _, [IDENTIFIER|RefsOut]) -->
  optional_det_phrase,
  [IDENTIFIER],
  moreArgs(tSentence, RefsOut),
  {
    verify_not_reserved(IDENTIFIER)
    % TODO: Make sure that all members of RefsNext are distinct.
  }.

moreArgs(X, RefsOut) -->
  cc,
  argList(X, notempty, RefsOut).

optional_det_phrase --> [].
optional_det_phrase -->
  determiner,
  [TYPE_NAME],
  {
    type(TYPE_NAME)
  }.