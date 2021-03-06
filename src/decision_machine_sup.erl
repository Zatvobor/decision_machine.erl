-module(decision_machine_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, { {one_for_all, 5, 10},
      [{ dm_process_sup, {dm_process_sup, start_link, []}, permanent, 5000, supervisor, [dm_process_sup] }]
    }}.

