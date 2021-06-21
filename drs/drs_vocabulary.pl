% type delcarations
vSentence -->
  lit_there,
  lit_are,
  [TYPE_NAME],
  {
    verify_not_reserved(TYPE_NAME),
    myAssert(type(TYPE_NAME), noun)
  }.

% predicate declarations
vSentence -->
  argList(vSentence, notempty, RefList1),
  lit_can,
  lit_be,
  predicate(vSentence, adjective, RefList1, drs([], []), _).

vSentence -->
  argList(vSentence, notempty, RefList1),
  lit_can,
  predicate(vSentence, verb, RefList1, drs([], []), _).

% function declaration
vSentence -->
  argList(vSentence, notempty, _),
  lit_can,
  lit_have,
  determiner,
  [FUNCTION_NAME],
  functionTypePhrase,
  {
    verify_not_reserved(FUNCTION_NAME),
    myAssert(function(FUNCTION_NAME), _)
  }.

functionTypePhrase --> [].
functionTypePhrase -->
  rp,
  lit_is,
  determiner,
  [TYPE_NAME],
  {
    type(TYPE_NAME)
  }.