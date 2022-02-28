% type delcarations
vSentence(DrsOut) -->
  lit_there,
  lit_are,
  [Type],
  {
    verify_not_reserved(Type),
    myAssert(type(Type), noun),
    singularize(Type, TypeSingular),
    DrsOut = drs([typeDeclaration], [type(TypeSingular)])
  }.

% predicate declarations
vSentence(DrsOut) -->
  argList(vSentence, RefList1),
  lit_can,
  lit_be,
  determiner,
  predicate(vSentence, noun, RefList1, drs([], []), Drs),
  {
    Drs = drs(_, Conditions),
    DrsOut = drs([predicateDeclaration], Conditions)
  }.

vSentence(DrsOut) -->
  argList(vSentence, RefList1),
  lit_can,
  lit_be,
  predicate(vSentence, adjective, RefList1, drs([], []), Drs),
  {
    Drs = drs(_, Conditions),
    DrsOut = drs([predicateDeclaration], Conditions)
  }.

vSentence(DrsOut) -->
  argList(vSentence, RefList1),
  lit_can,
  predicate(vSentence, verb, RefList1, drs([], []), Drs),
  {
    Drs = drs(_, Conditions),
    DrsOut = drs([predicateDeclaration], Conditions)
  }.

% function declaration
vSentence(DrsOut) -->
  argList(vSentence, Args),
  lit_can,
  lit_have,
  determiner,
  [FunctionName],
  functionTypePhrase(Type),
  {
    verify_not_reserved(FunctionName),
    myAssert(function(FunctionName), _),
    DrsOut = drs([functionDeclaration], [function(FunctionName, Args, Type)])
  }.

vSentence(DrsOut) -->
  argList(vSentence, Args),
  lit_can,
  lit_have,
  determiner,
  [FunctionName],
  {
    Args = [Type|_],
    verify_not_reserved(FunctionName),
    myAssert(function(FunctionName), _),
    DrsOut = drs([functionDeclaration], [function(FunctionName, Args, Type)])
  }.

functionTypePhrase(Type) -->
  rp,
  lit_is,
  determiner,
  [Type],
  {
    type(Type)
  }.

% translate natural number into type nat.
functionTypePhrase(nat) -->
  rp,
  lit_is,
  determiner,
  [natural],
  [number].

% translate number into type int.
functionTypePhrase(int) -->
  rp,
  lit_is,
  determiner,
  [number].