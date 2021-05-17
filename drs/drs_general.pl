% generic arglist rules
argList(_, empty, _, _) --> [].

argList(vSentence, _, _, _) -->
  determiner,
  [TYPE_NAME],
  moreArgs(vSentence, _, _),
  {
    type(TYPE_NAME)
  }.

argList(tSentence, _, RefsIn, RefsOut) -->
  optional_det_phrase,
  [IDENTIFIER],
  moreArgs(tSentence, [IDENTIFIER|RefsIn], RefsOut),
  {
    verify_not_reserved(IDENTIFIER),
    RefsOut = [IDENTIFIER|RefsIn]
    % TODO: Make sure that all members of RefsOut are distinct.
  }.

moreArgs(_, _, _) --> [].
moreArgs(X, RefsIn, RefsOut) -->
  cc,
  argList(X, notempty, RefsIn, RefsOut).

optional_det_phrase --> [].
optional_det_phrase -->
  determiner,
  [TYPE_NAME],
  {
    type(TYPE_NAME)
  }.