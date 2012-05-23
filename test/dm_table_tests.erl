-module(dm_table_tests).
-include_lib("eunit/include/eunit.hrl").


new_2_test() ->
  { dm_table, new_2_test, _ } = dm_table:new(new_2_test, db_table),
  ?assert(erlang:is_process_alive(whereis(new_2_test))).

new_2_function_test() ->
  pending.

new_2_module_test() ->
  pending.


set_columns_test() ->
  E = dm_table:new(set_columns_test),

  Columns = [{column, func}, {column2, func2}],
  E:set_columns(Columns),

  Columns = E:columns(),

  {columns, C, _, _, _, _} = E:table(),
  C = [column, column2].


add_column_test() ->
  E = dm_table:new(add_column_test),

  E:add_column(column, func),
  [{column, func}] = E:columns(),

  E:add_column(column2, func2),
  [{column, func}, {column2, func2}] = E:columns(),

  E:add_column(column3, func3),
  [{column, func}, {column2, func2}, {column3, func3}] = E:columns().


set_actions_test() ->
  E = dm_table:new(set_actions_test),

  Actions = [{action, func}, {action2, func2}],
  E:set_actions(Actions),

  Actions = E:actions(),

  {_, _, actions, A, _, _} = E:table(),
  A = [action, action2].


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

  Decisions = [['Y', 'Y', 'N', {[true]}], ['Y', 'N', 'N', {[false]}]],
  E:set_decisions(Decisions),

  {_, _, _, _, rows, D} = E:table(),
  Decisions = D.


add_decision_test() ->
  E = dm_table:new(add_decision_test),

  E:add_decision(['Y', 'Y', 'N'], [true]),

  {_, _, _, _, rows, D } = E:table(),
  [['Y', 'Y', 'N', {[true]}]] = D,

  E:add_decision(['Y', 'N', 'N'], [false]),

  {_, _, _, _, rows, D1 } = E:table(),

  [['Y', 'Y', 'N', {[true]}], ['Y', 'N', 'N', {[false]}]] = D1.
