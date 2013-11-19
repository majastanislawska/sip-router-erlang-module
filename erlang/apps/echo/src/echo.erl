%%
%% echo.erl
%% echo entry point
%%
-module(echo).

-export([start/0, start_link/0, stop/0]).

start_link() ->
    echo_sup:start_link().

start() ->
    application:start(echo).

stop() ->
    application:stop(echo).

