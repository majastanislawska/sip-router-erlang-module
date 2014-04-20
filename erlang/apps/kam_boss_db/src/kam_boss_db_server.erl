%%%-------------------------------------------------------------------
%%% @author $author
%%% @copyright (C) $year, $company
%%% @doc
%%%
%%% @end
%%% Created : $fulldate
%%%-------------------------------------------------------------------
-module(kam_boss_db_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         translate_fields/1,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%% {ok, State, Timeout} |
%% ignore |
%% {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
        {ok, {}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%% {reply, Reply, State} |
%% {reply, Reply, State, Timeout} |
%% {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, Reply, State} |
%% {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request={select,Table,Fields,Keys,[]}, _From, State) ->
    FT=[ X || NN <- Fields, X={N,_T,_S,_D,_C} <- schema:schema(Table), N==NN ],
    Keys2=translate_keys(Keys),
    Fields2=translate_fields(Fields),
    Data=lists:map( fun(X) -> X:attributes()
		    end,boss_db:find(Table, Keys2)),
    TrimmedData=lists:map( fun(X) ->
			FinProp=lists:map(fun(Key) -> lists:keyfind(Key, 1, X)
			end,Fields2),
			{_Keys,FinRec}=lists:unzip(FinProp),
			list_to_tuple(FinRec)
		    end,Data),
    Reply={ FT,TrimmedData},
    error_logger:info_msg("db_mock handle_call ~p >=> ~p",[Request,Reply]),
    {reply, Reply, State};
handle_call(Request={insert,Table,Fields,Keys, Values}, _From, State) ->
    FT=[ X || NN <- Fields, X={N,_T,_S,_D,_C} <- schema:schema(Table), N==NN ],
    Data=boss_record:new(Table, Values),
    Data1=Data:save(),
    Reply={ FT,Data1},
    error_logger:info_msg("db_mock handle_call ~p >=> ~p",[Request,Reply]),
    {reply, Reply, State};
handle_call(Request={delete,Table,Fields,Keys, Values}, _From, State) ->
    FT=[ X || NN <- Fields, X={N,_T,_S,_D,_C} <- schema:schema(Table), N==NN ],
    Data=lists:map( fun(X) -> boss_db:delete(X:id())
		    end,boss_db:find(Table, translate_keys(Keys))),
    Reply={ FT,Data},
    error_logger:info_msg("db_mock handle_call ~p >=> ~p",[Request,Reply]),
%
%handle_call(Request, _From, State) ->
%    error_logger:info_msg("db_mock handle_call ~p",[Request]),
%    Reply = Request,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
        error_logger:info_msg("handle_cast ~p",[Msg]),
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
        error_logger:info_msg("handle_info ~p",[Info]),
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
translate_field(instance) -> instanc;
translate_field(instanc) -> instance;
translate_field(Field) -> Field.

translate_fields([]) -> [];
translate_fields([ H | T ]) -> [translate_field(H) | translate_fields(T)].

translate_key({Field0,Key,Val}) ->
    Field=translate_field(Field0),
    case Key of
	'='  -> {Field,'equals',Val};
	'!=' -> {Field,'not_equals',Val};
	'>'  -> {Field,'gt',Val};
	'<'  -> {Field,'lt',Val};
	'>=' -> {Field,'ge',Val};
	'<=' -> {Field,'le',Val};
	'equal'  -> {Field,'=',Val};
	'not_equal' -> {Field,'!=',Val};
	'gt'  -> {Field,'>',Val};
	'lt'  -> {Field,'<',Val};
	'ge' -> {Field,'>=',Val};
	'le' -> {Field,'<=',Val}
    end.

translate_keys([]) -> [];
translate_keys([H|T]) ->[translate_key(H) | translate_keys(T)].
