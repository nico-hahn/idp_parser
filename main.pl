% Main entry point to the system.

% Load up system.
:- use_module(library(pure_input)).
:- [util/helper_predicates].
:- [util/drs2idp/drs2idp].
:- [drs/drs_grammar].
:- [util/file_reader].

% Load and run tests.
%:- [test].
% Reset everything after testing.
:- retractall(type(_)).
:- retractall(function(_)).
:- retractall(valid_predicate(_,_)).
:- retractall(valid_preposition(_,_)).


% Do the work.
all([])     --> [].
all([L|Ls]) --> [L], all(Ls).

parse :-
  phrase_from_stream(all(Ls), user_input),
  readText(Ls, Words),
  splitText(Words, V, T, S),
  log(['vocabulary:'|V]), !,
  log(['theory:'|T]), !,
  log(['structure:'|S]), !,
  log(''), % empty line
  metaSentence(vocabulary, V, []),
  metaSentence(theory, T, []),
  metaSentence(structure, S, []),
  metaSentence(procedure, _, []).

:- log('*********** Initiating parser ***********').
:- parse.
:- log('***********  Finished parser  ***********').

:- halt.