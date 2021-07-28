% Main entry point to the system.

% Load up system.
:- use_module(library(pure_input)).
:- [util/helper_predicates].
:- [drs/drs_grammar].
:- [util/file_reader].

% Load and run tests.
:- [test].

% Do the work.
all([])     --> [].
all([L|Ls]) --> [L], all(Ls).

parse :-
  phrase_from_stream(all(Ls), user_input),
  readText(Ls, Words),
  write(Words).

:- writeln(user_error, '*********** Initiating parser ***********').
:- parse.
:- writeln(user_error, '***********  Finished parser  ***********').

:- halt.