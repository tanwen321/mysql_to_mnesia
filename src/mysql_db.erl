-module(mysql_db).
-behaviour(gen_server).
%-compile(export_all).
-export([start_link/0, stop/0, select/1, select/2, insert/1, use/1, init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).

-include("../include/m2m.hrl"). 

-define(SERVERNAM, ?MODULE).

-record(state,
    {
        pid     %pid of mysql conn
    }).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

select(Sql) ->
    gen_server:call(?SERVER, {select, Sql}). 

select(Sql, Par) ->
    gen_server:call(?SERVER, {select, Sql, Par}). 

insert(Sql) ->
    gen_server:call(?SERVER, {insert, Sql}). 

use(Sql) ->
    gen_server:call(?SERVER, {use, Sql}). 

stop() -> 
    gen_server:cast(?SERVER, stop). 

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_) ->
    case mysql_conn:start_link(?DBSERVER, ?DPORT, ?DBUSER, ?DBPASS, ?INFODATA,
        fun(_, _, _, _) -> ok end, utf8, ?DBCONN) of
        {ok, Pid} ->
            {ok, #state{pid=Pid}};
        _ ->
            {stop, "connect db fail!"}
    end.
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
handle_call({select, Sql}, _From, #state{pid=Pid} = State) ->
    Reply = select_sql(Pid, Sql),
    {reply, Reply, State};

handle_call({select, Sql, Parameters}, _From, #state{pid=Pid} = State) ->
    Reply = select_sql(Pid, Sql, Parameters),
    {reply, Reply, State};

handle_call({insert, Sql}, _From, #state{pid=Pid} = State) ->
    Reply = insert_sql(Pid, Sql),
    {reply, Reply, State};

handle_call({use, Sql}, _From, #state{pid=Pid} = State) ->
    Reply = use_sql(Pid, Sql),
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Unrec, _From, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast(stop, #state{pid=Pid}) ->
    exit(Pid, normal);

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    io:format("hello gen server: terminating~n").

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%other fun
%%====================================================================

select_sql(Pid, Sql, Parameters) when is_list(Sql), is_list(Parameters) ->
    Res = fetch(Pid, Sql, Parameters),
    case Res of
        {data, {mysql_result,_,Datas,_,_,_,_,_}} ->
            {ok, Datas};
        {error, MysqlRes} ->
             {no, MysqlRes}
    end.

select_sql(Pid, Sql) when is_list(Sql)->
    Res = fetch(Pid, Sql),
    case Res of
        {data, {mysql_result,_,Datas,_,_,_,_,_}} ->
            {ok, Datas};
        {error, MysqlRes} ->
             {no, MysqlRes}
    end.

insert_sql(Pid, Sql) when is_list(Sql)->
    Res = fetch(Pid, Sql),
    case Res of
        {updated,{mysql_result,_,_,_,_,_,_,_}} ->
            {ok, "insert_ok"};
        {error, MysqlRes} ->
             {no, MysqlRes}
    end.

use_sql(Pid, Sql) when is_list(Sql)->
    Res = fetch(Pid, Sql),
    case Res of
        {updated,{mysql_result,[],[],0,0,[],0,[]}} ->
            ok;
        {error, MysqlRes} ->
             {no, MysqlRes}
    end.

%pack_now(Now) -> pack_datetime(calendar:now_to_datetime(Now)).

pack_value(null) ->
    "null";
pack_value(undefined) ->
    "null";
pack_value(V) when is_binary(V) ->
    pack_value(binary_to_list(V));
pack_value(V) when is_list(V) ->
    mysql:encode(V);
pack_value(Val) when is_integer(Val) ->
    integer_to_list(Val);
pack_value(Val) when is_float(Val) ->
    float_to_list(Val);
pack_value(true) ->
    "TRUE";
pack_value(false) ->
    "FALSE".

fetch(Pid, Query) ->
%    _ = io:format("Query ~s~n", [Query]),
    Res = mysql_conn:fetch(Pid, [Query], self()),
    _ = case Res of
        {error, MysqlRes} ->
            _ = io:format("SQL Error: ~p~n",[MysqlRes]);
        _ -> ok
    end,
    Res.

fetch(Pid, Query, Parameters) ->
    Sql = replace_parameters(lists:flatten(Query), Parameters),
    fetch(Pid, Sql).

replace_parameters([$$, X, Y | Rest], Parameters) when X >= $1, X =< $9, Y >= $0, Y =< $9 ->
    Position = (X-$0)*10 + (Y-$0),
    [lookup_single_parameter(Position, Parameters) | replace_parameters(Rest, Parameters)];
replace_parameters([$$, X | Rest], Parameters) when X >= $1, X =< $9 ->
    Position = X-$0,
    [lookup_single_parameter(Position, Parameters) | replace_parameters(Rest, Parameters)];
replace_parameters([X | Rest], Parameters) ->
    [X | replace_parameters(Rest, Parameters)];
replace_parameters([], _) ->
    [].

lookup_single_parameter(Position, Parameters) ->
    try lists:nth(Position, Parameters) of
        V -> pack_value(V)
    catch
        Error -> throw(io_lib:format("Error (~p) getting parameter $~w. Provided Params: ~p", [Error, Position, Parameters]))
    end.