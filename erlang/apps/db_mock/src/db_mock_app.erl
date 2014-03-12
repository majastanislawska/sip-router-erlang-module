-module(db_mock_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    db_mock_sup:start_link().

stop(_State) ->
    ok.
