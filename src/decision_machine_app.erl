-module(decision_machine_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    decision_machine_sup:start_link().

stop(_State) ->
    ok.
