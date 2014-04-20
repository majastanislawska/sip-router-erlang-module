%%
%% kam_boss_db.erl
%% kam_boss_db entry point
%%
-module(kam_boss_db).

-export([start/0, start_link/0, stop/0]).

start_link() ->
    kam_boss_db_sup:start_link().

start() ->
    application:start(kam_boss_db).

stop() ->
    application:stop(kam_boss_db).

