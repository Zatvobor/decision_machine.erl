-module(dm_table_acceptance_tests).
-include_lib("eunit/include/eunit.hrl").

make_decision_test_() ->

  Background = fun() ->
    % decision table constructor (user defined callback)
    Init = fun(DMTable) ->
      DMTable:add_column(eligible_state, fun(EligibleStates, Input) -> lists:member(Input, EligibleStates) end),

      % there are many options for define column operations (matchers, functions)
      %% DMTable:add_column(age, fun(Age, Input) -> Age =:= Input end),
      %% DMTable:add_column(age, fun dm_table_matchers:exactly_equal_to/2),
      %% DMTable:add_column(age, exactly_equal_to), % simplified from 'fun dm_table_matchers:exactly_equal_to/2' form
      DMTable:add_column(age, exactly_equal_to),
      DMTable:add_column(gender, exactly_equal_to),
      %
      DMTable:add_action(eligibility, fun(_InputList) -> granted end),
      %
      DMTable:add_decision([[], [19, nil]], [nil]),
      DMTable:add_decision([[], [66, nil]], [nil]),
      %
      DMTable:add_decision([['VA', 'PA', 'NY'], [20, 65], 'Male'], [true]),
      DMTable:add_decision([['VA', 'PA', 'NY'], [20, 65], 'Female'], [true])
    end,

    dm_table:new(eligibility_rules, Init)
  end,

  % Apply real data for 'eligibility_rules' decision table
  Scenario = fun(DMTable) ->
    [
      ?_assertMatch(undefined,  DMTable:make_decision([nil, [19, nil]])),
      ?_assertMatch(undefined,  DMTable:make_decision([nil, [66, nil]])),

      ?_assertMatch([{ eligibility, granted }], DMTable:make_decision(['NY', [20,65], 'Male'])),
      ?_assertMatch([{ eligibility, granted }], DMTable:make_decision(['PA', [20,65], 'Female']))
    ]
  end,


  {"Eligibility rules: make decision",
    {setup, Background, Scenario}
  }.
