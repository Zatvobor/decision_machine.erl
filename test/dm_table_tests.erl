-module(dm_table_tests).
-include_lib("eunit/include/eunit.hrl").

% callback method mock
-export([init/1]).


new_2_module_test() ->
  application:start(decision_machine),
  {dm_table, new_2_module_test, _} = E = dm_table:new(new_2_module_test, dm_table_tests),

  ?assert(erlang:is_process_alive(whereis(new_2_module_test))),

  assert_new_2_tests(E).


new_2_function_test() ->
  {dm_table, new_2_function_test, _} = E = dm_table:new(new_2_function_test, fun init/1),

  ?assert(erlang:is_process_alive(whereis(new_2_function_test))),

  assert_new_2_tests(E).


assert_new_2_tests(E) ->
  [{column, _}] = E:conditions(),
  [{action, _}] = E:actions().

init(E) ->
  E:add_condition(column, equal_to),
  E:add_action(action, equal_to),

  E.


set_columns_test() ->
  E = dm_table:new(set_columns_test),

  Conditions = [{column, func}, {column2, func2}],
  E:set_conditions(Conditions),

  Conditions = E:conditions().


add_condition_test() ->
  E = dm_table:new(add_condition_test),

  E:add_condition(column, equal_to),
  [{column, _}] = E:conditions(),

  E:add_condition(column2, equal_to),
  [{column, _}, {column2, _}] = E:conditions(),

  E:add_condition(column3, equal_to),
  [{column, _}, {column2, _}, {column3, _}] = E:conditions().


set_actions_test() ->
  E = dm_table:new(set_actions_test),

  Actions = [{action, func}, {action2, func2}],
  E:set_actions(Actions),

  Actions = E:actions().


add_action_test() ->
  E = dm_table:new(add_action_test),

  E:add_action(action, func),
  [{action, func}] = E:actions(),

  E:add_action(action2, func2),
  [{action, func}, {action2, func2}] = E:actions(),

  E:add_action(action3, func3),
  [{action, func}, {action2, func2}, {action3, func3}] = E:actions().


set_decisions_test() ->
  E = dm_table:new(set_decisions_test),

  Consequences = [['Y', 'Y', 'N', {[true]}], ['Y', 'N', 'N', {[false]}]],
  E:set_consequences(Consequences),

  Consequences = E:consequences().


add_decision_test() ->
  E = dm_table:new(add_decision_test),

  E:add_consequence(['Y', 'Y', 'N'], [true]),
  D = E:consequences(),
  [['Y', 'Y', 'N', {[true]}]] = D,

  E:add_consequence(['Y', 'N', 'N'], [false]),
  D1 = E:consequences(),
  [['Y', 'Y', 'N', {[true]}], ['Y', 'N', 'N', {[false]}]] = D1.
