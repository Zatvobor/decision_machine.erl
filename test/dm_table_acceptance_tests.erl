-module(dm_table_acceptance_tests).
-include_lib("eunit/include/eunit.hrl").


try_to_make_decision_test_() -> pending.
%% > EligibilityRules = dm_table:new(eligibility_rules).
%%
%% % Setup operation's rules
%% EligibilityRules:add_columns(eligible_state, fun(EligibleStates, Input) ->  EligibleStates =:= Input end).
%% EligibilityRules:add_columns(gender, fun(Gender, Input) ->  Gender =:= Input end).
%% EligibilityRules:add_columns(age, fun(Gender, Input) ->  Gender =:= Input end).
%%
%% % Setup decision's actions
%% EligibilityRules:add_actions(eligibility, fun(InputList) -> available end.).
%%
%% % Apply input data and make decision
%% EligibilityRules:add_decision([['VA', 'PA', 'NY'], 'Male', [20, 65]], [true]).
%% EligibilityRules:add_decision([['VA', 'PA', 'NY'], 'Female', [20, 65]], [true]).
%% EligibilityRules:add_decision([['CO', 'LV', 'CA', 'TX']], [nil]).
%%
%% % Iterate through decision table 'eligibility_rules' and apply suitable actions
%% EligibilityRules:make_decision([['VA', 'PA', 'NY'], 'Male', [20, 65]]).
%% > [{row, N, eligibility, available}]

make_decision_test_() ->

  FunBackground = fun() ->
    % decision table constructor (user defined callback)
    FunInit = fun(DMTable) ->

      DMTable:add_columns(age, fun(Age, Input) ->  Age =:= Input end),
      DMTable:add_columns(gender, fun(Gender, Input) ->  Gender =:= Input end),

      DMTable:add_actions(eligibility, fun(InputList) -> granted end),

      DMTable:add_decision([19, nil], [nil]),

      DMTable:add_decision([[20, 65], 'Male'], [true]),
      DMTable:add_decision([[20, 65], 'Female'], [true]),

      DMTable:add_decision([66, nil], [nil])
    end,

    dm_table:new(eligibility_rules, FunInit)
  end,

  % Apply real data for 'eligibility_rules' decision table
  FunScenario = fun(DMTable) ->
    [] = DMTable:make_decision([19, nil]),

    [{_, _, eligibility, granted}] = DMTable:make_decision([[20,65], 'Male']),
    [{_, _, eligibility, granted}] = DMTable:make_decision([[20,65], 'Female']),

    [] = DMTable:make_decision([66, nil])
  end,


  {"Eligibility rules: make decision",
    {setup, FunBackground, FunScenario}
  }.
