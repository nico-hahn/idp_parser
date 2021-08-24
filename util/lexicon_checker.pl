:- [clex_lexicon].
:- dynamic noun_guess/2.
:- dynamic verb_guess/2.


% this retrieves the s form or the standard of the given respective other form
verb(SFORM, NORMALFORM) :-
  iv_finsg(SFORM, NORMALFORM), !.

verb(SFORM, NORMALFORM) :-
  tv_finsg(SFORM, NORMALFORM), !.

verb(SFORM, NORMALFORM) :-
  verb_guess(SFORM, NORMALFORM), !.

verb(NEW_S, NML) :-
  atom_concat(NML, s, NEW_S),
  log(['unknown verb:', NML, ', guessing:', NEW_S]),
  assertz(verb_guess(NEW_S, NML)).

% TODO: Add another rule to guess ..y to ..ies

% this retrieves the plural or singular form of a noun
noun(S, P) :- 
  noun_pl(P, S, _), !.

noun(S, P) :-
  noun_mass(P, S, _), !.

noun(S, P) :-
  noun_guess(P, S), !.

noun(NEW_SG, PL) :-
  atom_concat(NEW_SG, s, PL),
  log(['unknown noun:', PL, ', guessing:', NEW_SG]),
  assertz(noun_guess(PL, NEW_SG)).

noun(_, PL) :-
  log(['unknown noun:', PL, ', word too hard to guess']),
  fail.

singularize(Word, WordSing) :-
  noun(WordSing, Word), !.
singularize(Word, Word).
  
  
