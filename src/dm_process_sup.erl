-module(dm_process_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, { {simple_one_for_one, 5, 10},
      [{ dm_process, {dm_process, start_link, []}, transient, 5000, worker, [dm_process] }]
    }}.

