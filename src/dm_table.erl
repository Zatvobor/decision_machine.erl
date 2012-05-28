%%
%% For more details about API usage see: test/dm_table_acceptance_test.erl
%%
-module(dm_table, [Name, UserOptsModule]). % parameterized module

-export([new/1, new/2]). % module initialization functions

% module instance functions
-export([columns/0, columns/1, set_columns/1, add_column/2]).
-export([actions/0, actions/1, set_actions/1, add_action/2]).
-export([table/0, table/1, set_decisions/1, add_decision/2]).
-export([make_decision/1]).

% spawned process stuff (belongs to current object instance)
-behavior(gen_server).
-record(decision_table, {columns = [], actions = [], table = {columns, [], actions, [], rows, [] }}).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).



%% module functions

new(Name) ->
  new(Name, fun(DmTable) -> DmTable end). % stubbed behavior

new(Name, UserOptsModule) when is_atom(UserOptsModule) ->
  new(Name, fun UserOptsModule:init/1);

new(Name, UserOptsFun) when is_function(UserOptsFun) ->
  % 1. initialize p-module
  Instance = instance(Name, UserOptsFun),
  % 2. initialize satelite process
  Instance:start_link(),
  % 3. apply user defined callback
  UserOptsFun(Instance),
  % 4. return instance reference
  Instance.


%% @Public object instance methods

columns() ->
  columns(gen_server:call(Name, {fetch_decision_table})).

columns(#decision_table{columns = C} = _T) ->
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


make_decision(InputList) ->
  gen_server:call(Name, {find_a_match_and_execute_actions, InputList}).



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
  {reply, State, State};

handle_call({find_match_and_execute_actions, InputList}, _From, #decision_table{ columns = C, actions = A, table = T } = _State) ->
  {_, _, _, _, rows, Rows} = T,
  e.


handle_cast({push_decision_table, NewState}, _State) ->
  {noreply, NewState}.
