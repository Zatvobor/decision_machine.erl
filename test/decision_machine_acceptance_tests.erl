-module(decision_machine_acceptance_tests).
-include_lib("eunit/include/eunit.hrl").


attach_test_() ->
  Background = fun() ->
    ok % = application:start(decision_machine)
  end,

  Scenario = fun() ->
    % attach
    {true, true} = {decision_machine:attach_new(alpha), decision_machine:attach_new(betta)},
    {true, true} = {decision_machine:member(alpha), decision_machine:member(betta)},

    % delete
    true = decision_machine:delete(alpha),
    true = decision_machine:delete(unknown), % ???
    {false, true} = {decision_machine:member(alpha), decision_machine:member(betta)},

    % lookup
    {betta, {dm_table, betta, _}} = decision_machine:lookup(betta),
    {error, unknown} = decision_machine:lookup(unknown)
  end,

  {"Decision Machine: insert/delete/lookup/member",
    {setup, Background, Scenario}
  }.

make_decision_test_() ->
  Background = fun() ->
    DMTable = dm_table:new(untitled),
    DMTable:add_column(age, exactly_equal_to),
    DMTable:add_action(eligibility, fun(_InputList) -> granted end),
    DMTable:add_decision([10], [true]),
    DMTable:add_decision([20], [false]),

    decision_machine:insert(DMTable)
  end,

  Scenario = fun() ->
    % tries
    {error, unknown} = decision_machine:make_decision(unknown, [10]),

    % success
    [{eligibility, granted}] = decision_machine:make_decision(untitled, [10]),
    [] = decision_machine:make_decision(untitled, [20])
  end,

  {"Decision Machine: make_decision",
    {setup, Background, Scenario}
  }.

