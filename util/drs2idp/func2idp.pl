getFunctionValues(Conditions, Values) :-
  filterValues(Conditions, Vals),
  sortValues(Vals, Values).

filterValues([], []).
filterValues([Condition|Conditions], Values) :-
  Condition \= f_value(_, _),
  filterValues(Conditions, Values).
filterValues([Condition|Conditions], [Condition|Values]) :-
  Condition = f_value(_, _),
  filterValues(Conditions, Values).

sortValues([], []).
sortValues([Value|Values], [Func|Sorted]) :-
  Value = f_value(NameArgs, _),
  NameArgs =.. [Name|_],
  findAll(Name, [Value|Values], Remaining, Found),
  Func =.. [Name, Found],
  sortValues(Remaining, Sorted).

findAll(_, [], [], []).
findAll(FName, [Value|Values], [Value|Remaining], Found) :-
  Value = f_value(NameArgs, _),
  NameArgs =.. [Name|_],
  Name \= FName,
  findAll(FName, Values, Remaining, Found).
findAll(FName, [Value|Values], Remaining, [Func|Found]) :-
  Value = f_value(NameArgs, Val),
  NameArgs =.. [FName|Args],
  Func =.. [FName, Args, Val],
  findAll(FName, Values, Remaining, Found).

createFunctionString(Function, String) :-
  Function =.. [Name, Values],
  createValueString(Values, ValString),
  string_concat(ValueString, '; ', ValString),
  super_concat(['\tf_', Name, '<ct> = { ', ValueString, ' }\n'], String).

createValueString([], '').
createValueString([Value|Values], ValueString) :-
  createValueString(Values, String),
  Value =.. [_, Args, Val],
  atomics_to_string(Args, ',', ArgStr),
  super_concat([ArgStr,'->',Val, '; ', String], ValueString).