-module(dm_table_acceptance_tests).
-include_lib("eunit/include/eunit.hrl").


try_to_make_decision_test_() -> pending.
%% > EligibilityRules = dm_table:new(eligibility_rules).
%%
%% % Setup operation rules
%% EligibilityRules:add_columns(eligible_state, fun(EligibleStates, Input) ->  EligibleStates =:= Input end).
%% EligibilityRules:add_columns(gender, fun(Gender, Input) ->  Gender =:= Input end).
%% EligibilityRules:add_columns(age, fun(Gender, Input) ->  Gender =:= Input end).
%%
%% % Setup decision actions
%% EligibilityRules:add_actions(eligibility, fun(true) -> io:format("available") end.).
%%
%% % Apply input data and make decision
%% EligibilityRules:add_decision([['VA', 'PA', 'NY'], 'Male', [20, 65]], [true]).
%% EligibilityRules:add_decision([['VA', 'PA', 'NY'], 'Female', [20, 65]], [true]).
%% EligibilityRules:add_decision([['CO', 'LV', 'CA', 'TX']], [nil]).
%%
%% % Iterate through decision table 'eligibility_rules' and apply suitable actions
%% EligibilityRules:make_decision([['VA', 'PA', 'NY'], 'Male', [20, 65]]).
%% > [{eligibility, "available"}]

make_decision_test_() ->

  FunBackground = fun() ->
    % decision table constructor (user defined callback)
    FunInit = fun(DMTable) ->

      DMTable:add_columns(age, fun(Age, Input) ->  Age =:= Input end),
      DMTable:add_columns(gender, fun(Gender, Input) ->  Gender =:= Input end),

      DMTable:add_actions(eligibility, fun(true) -> granted end),

      DMTable:add_decision([[19]], [nil]),

      DMTable:add_decision([[20, 65], 'Male'], [true]),
      DMTable:add_decision([[20, 65], 'Female'], [true]),

      DMTable:add_decision([[66]], [nil])
    end,

    dm_table:new(eligibility_rules, FunInit)
  end,

  % Apply real data for 'eligibility_rules' decision table
  FunScenario = fun(DMTable) ->
    [] = DMTable:make_decision([19]),

    [{eligibility, granted}] = DMTable:make_decision([[20,65], 'Male'], [true]),
    [{eligibility, granted}] = DMTable:make_decision([[20,65], 'Female'], [true]),

    [] = DMTable:make_decision([66])
  end,


  {"Eligibility rules: make decision",
    {setup, FunBackground, FunScenario}
  }.
