readText(Stream, Text) :-
  readText(Stream, [], Text).

readText(Stream, WordsCollected, Text) :-
  process_char_sequence(Stream, [], Words),
  Words \= [end_of_file],
  append(WordsCollected, Words, WordsNew),
  writeln(user_error, Text),
  readText(Stream, WordsNew, Text).

readText(Stream, Text, Text) :-
  process_char_sequence(Stream, [], [end_of_file]).

% Detect end of file to exit reading procedure.
process_char_sequence(Stream, Chars, Words) :-
  get_code(Stream, -1),
  Words = [end_of_file].
process_char_sequence(Stream, Chars, Words) :-
  get_code(Stream, end_of_file),
  Words = [end_of_file].

% Detect colon and add the parsed word and the colon to the text.
process_char_sequence(Stream, Chars, Words) :-
  get_code(Stream, 58),
  atom_codes(W2, Chars),
  W1 = ':',
  Words = [W2, W1].

% Detect period and add the parsed word and the period to the text.
process_char_sequence(Stream, Chars, Words) :-
  get_code(Stream, 46),
  atom_codes(W2, Chars),
  W1 = '.',
  Words = [W2, W1].

% Detect blank space and add the parsed word to the text.
process_char_sequence(Stream, Chars, Words) :-
  get_code(Stream, 32),
  atom_codes(Word, Chars),
  Words = [Word].

% Ignore new line characters.
process_char_sequence(Stream, Chars, Words) :-
  get_code(Stream, 10),
  process_char_sequence(Stream, Chars, Words).

% For any other character, add it to the chars list (= the current word)
process_char_sequence(Stream, Chars, Words) :-
  get_code(Stream, Char),
  atom_codes(Letter, [Char]),
  writeln(user_error, Letter),
  process_char_sequence(Stream, [Char|Chars], Words).