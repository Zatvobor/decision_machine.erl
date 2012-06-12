-module(dm_table_matchers_tests).
-include_lib("eunit/include/eunit.hrl").


to_function_test() ->
  EqualTo = dm_table_matchers:to_function(equal_to),
  {true, false} = {EqualTo(object, object), EqualTo(object, energy)},

  GreaterThan = dm_table_matchers:to_function(greater_than),
  {false, true} = {GreaterThan(1, 2), GreaterThan(2, 1)},

  Unknown = dm_table_matchers:to_function(unknown),
  ?assertException(error, undef, Unknown(actual, expected)).
