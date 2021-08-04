lit_vocabulary --> [vocabulary].
lit_theory --> [theory].
lit_structure --> [structure].
lit_colon --> [':'].
lit_comma --> [','].
lit_period --> ['.'].
lit_if --> [if].
lit_then --> [then].
lit_def_begin --> [we, define, the, following, ':'].
lit_def_end --> [end, of, definition, '.'].
lit_there --> [there].
lit_are --> [are].
lit_is --> [is].
lit_can --> [can].
lit_be --> [be].
lit_has --> [has].
lit_have --> [have].
lit_for --> [for].
lit_does --> [does].
lit_do --> [do].
lit_not --> [not].
lit_the --> [the].
cc --> [and].
cc_or --> [or].
lit_different --> [different].
lit_from --> [from].
lit_of --> [of].
determiner --> [the]; [a]; [an]; [another]; [some].
optional_determiner --> determiner; [].
quantifier --> [each]; [every]; [all].
rp --> [that]; [which]; [who].

is_are_optional --> lit_is; lit_are; [].
is_are --> lit_is; lit_are.
quant_det --> quantifier ; determiner.

% This predicate is used to ensure that no user-input
% matches a key word
reserved_words -->
  cc; cc_or; determiner; quantifier;
  rp; lit_comma, lit_period; lit_if; lit_then; lit_def_begin;
  lit_def_end; lit_there; lit_are; lit_is; lit_can; lit_be; lit_has; lit_have;
  lit_for; lit_does; lit_do; lit_not; lit_the; lit_vocabulary; lit_theory;
  lit_structure; lit_colon.
verify_not_reserved(Word) :- 
  \+ reserved_words([Word], []). 