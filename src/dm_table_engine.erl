-module(dm_table_engine, [Columns, Actions, Table]).

-export([map_column_functions/1, foreach_table_rows_tail/1]).

-ifdef(TEST).
  -export([map_cells_value_and_action_functions/2]).
-endif.



%% @Public functions

map_column_functions(InputList) ->
  map_column_functions(InputList, 1, []).


foreach_table_rows_tail(InputListExtended) ->
  { _, _, _, _, rows, Rows} = Table,
  foreach_table_rows_tail(InputListExtended, Rows, 1).



%% @Private functions

map_column_functions(InputList, N, Acc) when length(InputList) >= N ->
  Input = lists:nth(N, InputList),
  {_, Func} = lists:nth(N, Columns),
  NewAcc = Acc ++ [[{input, Input}, {func, Func}, {n, N}]],

  map_column_functions(InputList, N + 1, NewAcc);

map_column_functions(InputList, N, Acc) -> Acc.


foreach_table_rows_tail(InputListExtended, Rows, N) when length(Rows) >= N ->
  {row, MappedCells, actions, MappedActions} = map_cells_value_and_action_functions(InputListExtended, lists:nth(N, Rows)),

  Runnerfun = fun(F) ->
    { Name, _ } = lists:nth(string:str(MappedActions, [F]), Actions),
    { Name, F(InputListExtended) }
  end,

  case walk_through_columns(MappedCells) of
    true  -> lists:map(Runnerfun, MappedActions);
    false -> foreach_table_rows_tail(InputListExtended, Rows, N + 1)
  end;

foreach_table_rows_tail(InputListExtended, Rows, N) -> undefined.


map_cells_value_and_action_functions(InputListExtended, Row) ->
  {row, map_cells_values(InputListExtended, Row), actions, map_action_functions(Row)}.


map_cells_values(InputListExtended, Row) ->
  Mapfun = fun(E) ->
    [_, _, {n, N}] = E,
    [{cell,lists:nth(N, Row)}|E]
  end,

  lists:map(Mapfun, InputListExtended).


map_action_functions(Row) ->
  {A} = lists:last(Row),
  Mapfun = fun(E) ->
    case E of
      % skip mapping for 'nil' elements
      nil   -> nil;
      false -> nil;
      % map and return action function as reference
      true  -> {_, Func} = lists:nth(string:str(A, [E]), Actions), Func
    end
  end,

  L = lists:map(Mapfun, A),
  lists:filter(fun(E) -> E /= nil end, L).


walk_through_columns(InputListExtended) ->
  Mapfun = fun(E) ->
    [{cell, Cell}, {input, Input}, {func, Func}, _] = E,
    case is_function(Func, 2) of
      true -> Func(Cell, Input);
      _    -> true % as common behavior
    end
  end,

  Reducefun = fun(E) ->
    case E of
      true  -> true;
      false -> false;
      _     -> true
    end
  end,

  NormalizedList = lists:map(Reducefun, lists:map(Mapfun, InputListExtended)),
  lists:member(false, NormalizedList) /= true.