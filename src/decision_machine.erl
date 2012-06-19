-module(decision_machine).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([
  attach_new/1, attach_new/2,

  insert/1, delete/1, lookup/1, member/1,

  make_decision/2
]).



start(normal, _StartArgs) ->
    ets:new(dm_tables_registry, [set, public, named_table]),
    ets:new(dm_internal_tables_registry, [set, public, named_table]),

    decision_machine_sup:start_link().

stop(_State) ->
    ok.



attach_new(TableName) ->
  insert(dm_table:new(TableName)).

attach_new(TableName, TableInit) ->
  insert(dm_table:new(TableName, TableInit)).



insert(Table) ->
  ets:insert(dm_tables_registry, {Table:name(), Table}).


delete(TableName) ->
  ets:delete(dm_tables_registry, TableName).


lookup(TableName) ->
  case ets:lookup(dm_tables_registry, TableName) of
    [H|_T] -> H;
    _      -> {error, unknown}
  end.


member(TableName) ->
  ets:member(dm_tables_registry, TableName).



make_decision(TableName, InputList) ->
  case O = lookup(TableName) of
     {_, T} = {TableName, {dm_table, TableName, _}} -> T:make_decision(InputList);
     _                                              -> O
  end.
