-module(dm_table_engine, [Columns, Actions, Rows]).

-export([map_column_functions/1, foreach_table_rows_tail/1]).



map_column_functions(InputList) ->
  map_column_functions(InputList, 1, []).

map_column_functions(InputList, N, Acc) when length(InputList) >= N ->
  Input = lists:nth(N, InputList),
  {_, Func} = lists:nth(N, Columns),
  NewAcc = Acc ++ [{input, Input, func, Func, n, N}],

  map_column_functions(InputList, N + 1, NewAcc);

map_column_functions(InputList, N, Acc) -> Acc.


foreach_table_rows_tail(InputListExtended) ->
  not_implemented.
