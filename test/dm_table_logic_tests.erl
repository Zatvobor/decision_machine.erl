-module(dm_table_logic_tests).
-include_lib("eunit/include/eunit.hrl").


action_stub(_One) ->
  nil.

column_stub(_One, _Two) ->
  nil.

before_each() ->
  C = [{a, fun_column_stub}, {b, fun_column_stub}],
  A = [{title, fun_action_stub}],
  T = {undefined, undefined, undefined, undefined, rows, [[ a, b, {[true]}]]},

  dm_table_logic:new(C,A,T).

map_column_functions_test_() ->
  Scenario = fun(Logic) ->
    Actual = Logic:map_columns_and_actions([c,d]),

    Expected = [[{input, c}, {func, fun_column_stub}, {n, 1}], [{input, d}, {func, fun_column_stub}, {n, 2}]],

    % test definitions
    [?_assertMatch(Expected, Actual)]
  end,

  {"Map column function to user's input list", {setup, fun before_each/0, Scenario}}.


map_cells_value_and_action_functions_test_() ->
  Scenario = fun(Logic) ->
    InputListExtended = Logic:map_columns_and_actions([c,d]),
    Actual = Logic:map_cells_value_and_action_functions(InputListExtended, [ a, b, {[true, false, nil]}]),

    Expected = {
      row,
        [[{cell, a}, {input, c}, {func, fun_column_stub}, {n, 1}], [{cell, b}, {input, d}, {func, fun_column_stub}, {n, 2}]],
      actions,
        [fun_action_stub]
    },

    [?_assertMatch(Expected, Actual)]
  end,

  {"Map cells value and actions", {setup, fun before_each/0, Scenario}}.