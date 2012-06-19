%%
%% For more details about API usage see: test/dm_table_acceptance_test.erl
%%
-module(dm_table, [Name, UserOptsModule]). % parameterized module

-export([new/1, new/2]). % module initialization functions

% module instance functions
-export([name/0, get_tim/0, set_tim/1, columns/0, set_columns/1, add_column/2]).
-export([actions/0, set_actions/1, add_action/2]).
-export([table/0, set_decisions/1, add_decision/2]).
-export([make_decision/1]).

-record(decision_table, {columns = [], actions = [], table = {rows, [] }}).



%% module functions

new(Name) ->
  new(Name, fun(DmTable) -> DmTable end). % stubbed behavior

new(Name, UserOptsModule) when is_atom(UserOptsModule) ->
  new(Name, fun UserOptsModule:init/1);

new(Name, UserOptsFun) when is_function(UserOptsFun) ->
  % 1. initialize p-module
  Instance = instance(Name, UserOptsFun),
  % 2. apply user defined callback
  UserOptsFun(Instance),
  % 3. initialize satelite process
  supervisor:start_child(dm_process_sup, [Name]),
  % 4. return instance reference
  Instance.


%% @Public object instance methods

name() -> Name.


get_tim() ->
  case ets:lookup(dm_internal_tables_registry, Name) of
    [H|_T] -> {Name, Record} = H, Record;
    _      -> #decision_table{}
  end.


set_tim(Record) ->
  ets:insert(dm_internal_tables_registry, {Name, Record}),
  gen_server:cast(Name, {tim_updated, Record}).


columns() ->
  #decision_table{columns = C} = get_tim(),
  C.


set_columns(ColumnsList) ->
  T = get_tim(),
  set_tim(T#decision_table{ columns = ColumnsList }).


add_column(Atom, Matcher) when is_atom(Matcher) ->
  add_column(Atom, dm_table_matchers:to_function(Matcher));

add_column(Atom, Func) when is_function(Func) ->
  set_columns(columns() ++ [{Atom, Func}]).


actions() ->
  #decision_table{ actions = A } = get_tim(),
  A.


set_actions(ActionsList) ->
  T = get_tim(),
  set_tim(T#decision_table{ actions = ActionsList }).


add_action(Atom, Func) ->
  set_actions(actions() ++ [{Atom, Func}]).


table() ->
  #decision_table{ table = T } = get_tim(),
  T.


set_decisions(DecisionsList) ->
  Table = table(),
  T = get_tim(),

  set_tim(T#decision_table{ table = setelement(2, Table, DecisionsList) }).


add_decision(Row, Actions) ->
  {rows, RowsList} = table(),
  set_decisions(RowsList ++ ([Row ++ [ {Actions} ]])).


make_decision(InputList) ->
  gen_server:call(Name, {find_match_and_execute_actions, InputList}).