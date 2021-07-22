% Main entry point to the system.

% Load up system.
:- use_module(library(pure_input)).
:- [util/helper_predicates].
:- [drs/drs_grammar].
:- [util/file_reader].

% Load and run tests.
:- [test].

% Do the work.
parse :-
  readText(user_input, Text),
  write(Text).

:- writeln(user_error, '*********** Initiating parser ***********').
:- parse.
:- writeln(user_error, '***********  Finished parser  ***********').

:- halt.