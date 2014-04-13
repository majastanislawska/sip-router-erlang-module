%%%-------------------------------------------------------------------
%%% @author $author
%%% @copyright (C) $year, $company
%%% @doc
%%%
%%% @end
%%% Created : $fulldate
%%%-------------------------------------------------------------------
-module(db_mock_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("../lib/schema.hrl").

-record(state, {ref}).

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
        {ok,Ref}=dets:open_file("mock_db.dets",[{type,bag}]),
        io:format("The data file's info: ~p~n", [dets:info(Ref)] ),
        {ok, #state{ref=Ref}}.

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
handle_call(Request={select,Table=version,Fields=[table_version],[{table_name,'=',TableName}],[]}, _From, State) ->
    FT=[ X || NN <- Fields, X={N,_T,_S,_D,_C} <- schema:schema(Table), N==NN ],
    Ver=schema:version(TableName),
    Reply={FT,[{Ver}]},
    error_logger:info_msg("db_mock handle_call ~p >=> ~p",[Request,Reply]),
    {reply, Reply, State};

%
handle_call(Request={select,Table,Fields,Keys,[]}, _From, State=#state{ref=Ref}) ->
    FT=[ X || NN <- Fields, X={N,_T,_S,_D,_C} <- schema:schema(Table), N==NN ],
    FilteredData=lookup_data(Ref,Table,Keys),
    TrimmedData=lists:map( fun(X) ->
			FinProp=lists:map(fun(Key) -> lists:keyfind(Key, 1, X)
			end,Fields),
			{_Keys,FinRec}=lists:unzip(FinProp),
			list_to_tuple(FinRec)
		    end,FilteredData),
    Reply={ FT,TrimmedData},
    error_logger:info_msg("db_mock handle_call ~p >=> ~p",[Request,Reply]),
    {reply, Reply, State};

handle_call(Request={insert,Table,Fields,Keys, Values}, _From, State=#state{ref=Ref}) ->
    FT=[ X || NN <- Fields, X={N,_T,_S,_D,_C} <- schema:schema(Table), N==NN ],
    Def=[Table |[ D || {_X,_T,_S,D,_C} <- schema:schema(Table)]],
    TF=[ X || {X,_T,_S,_D,_C} <- schema:schema(Table)],
    Data=dets:insert(Ref, etbx:to_rec({Table,Def,TF}, Values)),
    Reply={ FT,Data},
    error_logger:info_msg("db_mock handle_call ~p >=> ~p",[Request,Reply]),
    {reply, Reply, State};

handle_call(Request={delete,Table,Fields,Keys, Values}, _From, State=#state{ref=Ref}) ->
    FT=[ X || NN <- Fields, X={N,_T,_S,_D,_C} <- schema:schema(Table), N==NN ],
    FilteredData=lookup_data(Ref,Table,Keys),
    Ret=case FilteredData of
	[] -> no_data;
	List -> lists:foldr(fun (X,Acc) -> {_Keys,XVal}=lists:unzip(X),dets:delete_object(Ref, list_to_tuple([Table|XVal])),Acc+1 end,0,List)
    end,
    Reply={ FT,Ret},
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
lookup_data(Ref,Table,Keys) ->
    AllFields=[ X || {X,_T,_S,_D,_C} <- schema:schema(Table)],
    Data=lists:map( fun(X) ->
			[_RecName|Ret]=tuple_to_list(X),
			lists:zip(AllFields,Ret)
		    end,dets:lookup(Ref, Table)),
    FilteredData=lists:filter( fun(X) ->
	Acc=lists:foldl(fun
		({Key,'=', Val},Acc) -> Ret=lists:keyfind(Key, 1, X) == {Key,Val},case Acc of true ->Ret;false ->false end;
		({Key,'!=',Val},Acc) -> Ret=lists:keyfind(Key, 1, X) /= {Key,Val},case Acc of true ->Ret;false ->false end;
		({Key,'>', Val},Acc) -> Ret=lists:keyfind(Key, 1, X) >  {Key,Val},case Acc of true ->Ret;false ->false end;
		({Key,'<', Val},Acc) -> Ret=lists:keyfind(Key, 1, X) <  {Key,Val},case Acc of true ->Ret;false ->false end;
		({Key,'>=',Val},Acc) -> Ret=lists:keyfind(Key, 1, X) >= {Key,Val},case Acc of true ->Ret;false ->false end;
		({Key,'<=',Val},Acc) -> Ret=lists:keyfind(Key, 1, X) =< {Key,Val},case Acc of true ->Ret;false ->false end
	end,true,Keys),
	Acc
    end,Data),
    FilteredData.
