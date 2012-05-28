-module(dm_table_engine_tests).
-include_lib("eunit/include/eunit.hrl").


action_stub(_One) ->
  nil.

column_stub(_One, _Two) ->
  nil.

before_each() ->
  C = [{a, fun_column_stub}, {b, fun_column_stub}],
  A = [{title, fun action_stub/1}],
  R = {undefined, undefined, undefined, undefined, rows, [[ a, b, {[true]}]]},

  {C, A, R}.

map_column_functions_test_() ->
  Scenario = fun({C, A, R}) ->
    Engine = dm_table_engine:new(C,A,R),
    InputListExtended = Engine:map_column_functions([c,d]),

    InputListExtendedSpecification = [{input, c, func, fun_column_stub, n, 1}, {input, d, func, fun_column_stub, n, 2}],

    % test definitions
    [?_assertMatch(InputListExtendedSpecification, InputListExtended)]
  end,

  {"Map column function to user's input list", {setup, fun before_each/0, Scenario}}.