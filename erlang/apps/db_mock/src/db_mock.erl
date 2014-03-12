%%
%% db_mock.erl
%% db_mock entry point
%%
-module(db_mock).

-export([start/0, start_link/0, stop/0]).

start_link() ->
    db_mock_sup:start_link().

start() ->
    application:start(db_mock).

stop() ->
    application:stop(db_mock).

