[clex_lexicon].

% this retrieves the s form or the standard of the given respective other form
verb(SFORM, NORMALFORM) :-
  iv_finsg(SFORM, NORMALFORM);
  tv_finsg(SFORM, NORMALFORM).

% this retrieves the plural or singular form of a noun
noun(S, P) :- 
  noun_pl(S, P, _);
  noun_mass(S, P, _).