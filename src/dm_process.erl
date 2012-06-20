-module(dm_process).

% spawned process stuff (belongs to current object instance)
-behavior(gen_server).
-record(decision_table, {columns = [], actions = [], table = []}).

-export([start_link/1, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2]).



start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, Name, []).


init(Name) ->
  Tim = case ets:lookup(dm_internal_tables_registry, Name) of
    [H|_T] -> {Name, Record} = H, Record;
    _      -> #decision_table{}
  end,

  {ok, Tim}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



handle_call({find_match_and_execute_actions, InputList}, _From, #decision_table{ columns = C, actions = A, table = T } = State) ->
  Logic = dm_table_logic:new(C, A, T),

  % 1) map column function to user's input list
  LinkedInputList = Logic:map_columns_and_actions(InputList),

  % 2) apply linked input to decision table: go through, fire actions and return execution log as tuple
  Decisions = Logic:match_one(LinkedInputList),

  {reply, Decisions, State}.


handle_cast({tim_updated, Record}, _State) ->
  {noreply, Record}.