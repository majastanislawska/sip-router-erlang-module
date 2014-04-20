-module(kam_boss_db_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    DBOptions=[
    {adapter, mysql },
    {db_host, "localhost"},
%    {db_port, PortNumber::integer()},
    {db_username, "kamailio"},
    {db_password, "kamailiorw"},
    {db_database, "kamailio"},
%    {shards, [
%        {db_shard_models, [ModelName::atom()]},
%        {db_shard_id, ShardId::atom()},
%        {db_host, _}, {db_port, _}, ...
%    ]},
    {cache_enable, false}
%    {cache_exp_time, TTLSeconds::integer()}
    ],
    boss_db:start(DBOptions),
    boss_news:start(),
    kam_boss_db_sup:start_link().

stop(_State) ->
    ok.
