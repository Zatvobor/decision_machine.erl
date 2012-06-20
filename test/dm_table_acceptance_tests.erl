-module(dm_table_acceptance_tests).
-include_lib("eunit/include/eunit.hrl").

make_decision_test_() ->

  Background = fun() ->
    % decision table constructor (user defined callback)
    Init = fun(DMTable) ->
      DMTable:add_condition(eligible_state, fun(EligibleStates, Input) -> lists:member(Input, EligibleStates) end),

      % there are many options to define column operations (aka matchers, functions)
      %% DMTable:add_condition(age, fun(Age, Input) -> Age =:= Input end),
      %% DMTable:add_condition(age, fun dm_table_matchers:exactly_equal_to/2),
      %% DMTable:add_condition(age, exactly_equal_to), % simplified form from 'fun dm_table_matchers:exactly_equal_to/2'
      DMTable:add_condition(age, exactly_equal_to),
      DMTable:add_condition(gender, exactly_equal_to),

      DMTable:add_action(eligibility, fun(_InputList) -> granted end),

      DMTable:add_consequence([[], [19, nil]], [nil]),
      DMTable:add_consequence([[], [66, nil]], [nil]),
      DMTable:add_consequence([['VA', 'PA', 'NY'], [20, 65], 'Male'], [true]),
      DMTable:add_consequence([['VA', 'PA', 'NY'], [20, 65], 'Female'], [true])
    end,

    dm_table:new(eligibility_rules, Init)
  end,

  % Find a decision for 'eligibility_rules' table
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