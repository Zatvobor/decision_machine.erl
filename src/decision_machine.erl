-module(decision_machine).

-export([
  start/0,

  attach_new/1, attach_new/2,

  insert/1, delete/1, lookup/1, member/1,

  make_decision/2
]).



start() ->
  ets:new(dm_table_registry, [set, public, named_table]),
  ok.


attach_new(TableName) ->
  insert(dm_table:new(TableName)).

attach_new(TableName, TableInit) ->
  insert(dm_table:new(TableName, TableInit)).



insert(Table) ->
  ets:insert(dm_table_registry, {Table:name(), Table}).


delete(TableName) ->
  ets:delete(dm_table_registry, TableName).


lookup(TableName) ->
  case ets:lookup(dm_table_registry, TableName) of
    [H|_T] -> H;
    _      -> {error, unknown}
  end.


member(TableName) ->
  ets:member(dm_table_registry, TableName).



make_decision(TableName, InputList) ->
  case O = lookup(TableName) of
     {_, T} = {TableName, {dm_table, TableName, _}} -> T:make_decision(InputList);
     _                                              -> O
  end.
