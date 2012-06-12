-module(dm_table_matchers).

-export([
  to_function/1,
  % basic comparison functions
  equal_to/2, not_equal_to/2, exactly_equal_to/2, exactly_not_equal_to/2, less_than_or_equal_to/2,
  less_than/2, greater_than_or_equal_to/2, greater_than/2
]).


% convert function title (Atom) to function reference (for delayed invocation purpose)
to_function(Atom) ->
  fun(A, E) -> apply(?MODULE, Atom, [A, E]) end.


equal_to(A, E) -> A == E.

not_equal_to(A, E) -> A /= E.

exactly_equal_to(A, E) -> A =:= E.

exactly_not_equal_to(A, E) -> A =/= E.

less_than_or_equal_to(A, E) -> A =< E.

less_than(A, E) -> A < E.

greater_than_or_equal_to(A, E) -> A >= E.

greater_than(A, E) -> A > E.
