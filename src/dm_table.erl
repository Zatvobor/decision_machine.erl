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
%% > {eligibility, "available"}


-module(dm_table, [Name, UserOptsModule]). % parameterized module


-export([new/1, new/2]). % module initialization functions

% module instance functions
-export([columns/0, columns/1, set_columns/1, add_column/2]).
-export([actions/0, actions/1, set_actions/1, add_action/2]).
-export([table/0, table/1, set_decisions/1, add_decision/2]).

% spawned process stuff (belongs to current object instance)
-behavior(gen_server).
-record(decision_table, {columns = [], actions = [], table = {columns, [], actions, [], rows, [] }}).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).



%% module functions

new(Name) ->
  new(Name, fun(DmTable) -> DmTable end). % stubbed behavior

new(Name, UserOptsModule) ->
  Instance = instance(Name, UserOptsModule),
  Instance:start_link(),
  Instance.



%% @Public object instance methods

columns() ->
  columns(gen_server:call(Name, {fetch_decision_table})).

columns(#decision_table{ columns = C} = _T) ->
  C.


set_columns(ColumnsList) ->
  T = gen_server:call(Name, {fetch_decision_table}),

  NewSchema = rebuild_decision_table_schema(T, 2, ColumnsList),
  NewT = T#decision_table{ columns = ColumnsList, table = NewSchema },

  gen_server:cast(Name, {push_decision_table, NewT}).


add_column(Atom, Func) ->
  set_columns(columns() ++ [{Atom, Func}]).


actions() ->
  actions(gen_server:call(Name, {fetch_decision_table})).

actions(#decision_table{ actions = A } = _T) ->
  A.


set_actions(ActionsList) ->
  T = gen_server:call(Name, {fetch_decision_table}),

  NewSchema = rebuild_decision_table_schema(T, 4, ActionsList),
  NewT = T#decision_table{ actions = ActionsList, table = NewSchema },

  gen_server:cast(Name, {push_decision_table, NewT}).


add_action(Atom, Func) ->
  set_actions(actions() ++ [{Atom, Func}]).


table() ->
  table(gen_server:call(Name, {fetch_decision_table})).

table(#decision_table{ table = Table } = _T) ->
  Table.


set_decisions(DecisionsList) ->
  T = gen_server:call(Name, {fetch_decision_table}),
  Table = table(T),

  NewTable = setelement(6, Table, DecisionsList),

  NewT = T#decision_table{ table = NewTable },

  gen_server:cast(Name, {push_decision_table, NewT}).


add_decision(Row, Actions) ->
  {_, _, _, _, rows, RowsList} = table(),
  set_decisions(RowsList ++ ([Row ++ [ {Actions} ]])).



%% @Inernal instance stuff

get_decision_table() ->
  gen_server:call(Name, {fetch_decision_table}).


rebuild_decision_table_schema(T, Index, SourceList) ->
  {columns, _, actions, _, _, _} = Table = table(T),
  setelement(Index, Table, [L || {L, _} <- SourceList]).



%% @Internal stuff for gen_server behavior

start_link() ->
  gen_server:start_link({local, Name}, THIS, [], []).


init([]) ->
  {ok, #decision_table{}}.


handle_call({fetch_decision_table}, _From, State) ->
  {reply, State, State}.


handle_cast({push_decision_table, NewState}, _State) ->
  {noreply, NewState}.
