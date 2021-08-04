readText(CharList, Text) :-
  readWordsFromList(CharList, AtomList),
  handleSpecialCharacters(AtomList, Text).

% Takes a list of char values and returns a list of atom words
readWordsFromList([], []).
readWordsFromList(List, [AtomWord|Words]) :-
  readNextWord(List, Word, Rest),
  atom_codes(AtomWord, Word),
  readWordsFromList(Rest, Words).

% End the current word on: space and new line
readNextWord([], [], []).
readNextWord([32|Rest], [], Rest).
readNextWord([10|Rest], [], Rest).
readNextWord(List, [Char|Word], R) :-
  List = [Char|Rest],
  readNextWord(Rest, Word, R).

handleSpecialCharacters([], []).
handleSpecialCharacters([Word|WordsRest], WordOut) :-
  atom_codes(Word, Codes),
  last(Codes, SpecialChar),
  append(CodesPure, [SpecialChar], Codes),
  atom_codes(WordPure, CodesPure),
  checkSpecialChar(SpecialChar, SpecialCharAtom),
  WordOut = [WordPure, SpecialCharAtom|WordsOutRest],
  handleSpecialCharacters(WordsRest, WordsOutRest).
handleSpecialCharacters([Word|WordsRest], [Word|WordsOutRest]) :-
  atom_codes(Word, Codes),
  last(Codes, SpecialChar),
  \+ checkSpecialChar(SpecialChar, _),
  handleSpecialCharacters(WordsRest, WordsOutRest).

checkSpecialChar(58, ':').
checkSpecialChar(44, ',').
checkSpecialChar(46, '.').