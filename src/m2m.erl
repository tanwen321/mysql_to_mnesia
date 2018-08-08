-module(m2m).
%-compile(export_all).
-export([mysql_to_mnesia/1, mysql_to_mnesia/2]).

%%生成的erl文件和模块名称
-define(MDBFILE, "src/m2m_db_all.erl").	
-define(MDB, "m2m_db_all").

mysql_to_mnesia(Dbname) ->
	case mysql_db:start_link() of
		{ok, _Pid} ->
			get_tables(Dbname);
		{error,{already_started, _Pid}} ->
			get_tables(Dbname);
		_E ->
			io:format("connect to mysql db fail, please check info of mtm.hrl,error:~p~n",[_E])
	end. 

get_tables(Dbname) ->
	case mysql_db:use("use " ++ erlang:atom_to_list(Dbname)) of
		ok ->
			case mysql_db:select("show tables") of
				{ok, Tablist} ->
					get_tables(Dbname, Tablist);
				{no, E} ->
					io:format("get table list fail,error:~p~n",[E])
			end;
		_E ->
			io:format("db:~p is not exit,error:~p!!!~n",[Dbname, _E])
	end.

get_tables(Dbname, List) ->
	L = [ erlang:list_to_atom(erlang:binary_to_list(X)) || [X] <- List],
	case mnesia:system_info(use_dir) of
	true ->
		alread_created;
	_ ->
		mnesia:create_schema([node()])
	end,
	mnesia:start(),
	init_file(),
	L2 = get_tables_2(Dbname, L, []),
	finish_file(L2).

get_tables_2(_,[], L) ->
	L;
get_tables_2(Dbname, [H|T], L) ->
	case m2m_start(Dbname, H) of
		H ->
			get_tables_2(Dbname, T, [H|L]);
		_ ->
			get_tables_2(Dbname, T, L)
	end.

mysql_to_mnesia(Dbname, Tablename) ->
	case mnesia:system_info(use_dir) of
	true ->
		alread_created;
	_ ->
		mnesia:create_schema([node()])
	end,
	mnesia:start(),
	init_file(),
	Tablename = m2m_start(Dbname, Tablename),
	finish_file([Tablename]).

m2m_start(Dbname, Tablename) ->
	case mysql_db:start_link() of
		{ok, _Pid} ->
			get_table_info(Dbname, Tablename);
		{error,{already_started, _Pid}} ->
			get_table_info(Dbname, Tablename);
		_E ->
			io:format("connect to mysql db fail, please check info of mtm.hrl,error:~p~n",[_E])
	end.

get_table_info(Dbname, Tablename) ->
	case mysql_db:select("show full columns from " ++ erlang:atom_to_list(Dbname) ++ "." ++ erlang:atom_to_list(Tablename)) of
		{ok, Data} ->
%			write_info(Data, Dbname, Tablename),
			set_info(Data, Tablename),
			[[Id_name,_,_,_,_,_,_,_,_]|_] = Data,
			set_data(Id_name, Dbname, Tablename),
			Tablename;
		{no, _E} ->
			io:format("get db:~p, table:~p info error:~p~n",[Dbname,Tablename,_E])
	end.

init() ->
	{ok, Fd} = file:open(?MDBFILE, [write]),
	io:format(Fd, "-module(~s).~n-export([start/0]).~n",[?MDB]),
	file:close(Fd).


init_file() ->
	case filelib:is_file(?MDBFILE) of
		true ->
			Newname = ?MDBFILE ++ "_" ++ m2m_lib:timestamp_to_string(calendar:now_to_datetime(os:timestamp())),
			os:cmd("mv " ++ ?MDBFILE ++ " " ++ Newname);
		false ->
			ok
	end,
	init().


set_info(Data, Tablename) ->
	List = [ erlang:binary_to_list(Name) || [Name,_,_,_,_,_,_,_,_] <- Data],
	{ok, Fd} = file:open(?MDBFILE, [append]),
	set_info(Fd, Tablename, List),
	file:close(Fd).

set_info(Fd, Tablename, List) ->
	io:format(Fd, "~n%%table:~p sync from mysql~n-record(~p, {",[Tablename,Tablename]),
	set_info_2(Fd, List).

set_info_2(Fd, [H])->
	io:format(Fd, "~s}).~n",[H]);
set_info_2(Fd,[H|T]) ->
	io:format(Fd, "~s, ",[H]),
	set_info_2(Fd, T).

set_data(Idname, Dbname, Tablename) ->
	{ok, Fd} = file:open(?MDBFILE, [append]),
	io:format(Fd, "~n%%create table:~p from mysql~ncreate_table_~p() ->
	mnesia:create_table(~p, [{attributes, record_info(fields, ~p)},
  								{type, set}, {disc_copies, [node()]}]).
		",[Tablename, Tablename, Tablename, Tablename]),
	io:format(Fd, "~n%%init_data for table:~p from mysql.~ninit_data_~p(N) ->
	Sql = \"select * from ~p.~p where ~s >\" ++ erlang:integer_to_list(N) ++ \" order by ~s \"++ \" limit 1000\",
	case mysql_db:select(Sql) of
		{ok, []} ->
			ok;
		{ok, Bindata} ->
			[Nowid|_] = lists:last(Bindata),
			Data = [erlang:list_to_tuple(['~p'|X])||  X <- Bindata],
			insert_data(Data),
			init_data_~p(Nowid);
		{no, Error} ->
			{no, Error}
	end.~n",[Tablename, Tablename, Dbname, Tablename, Idname, Idname, Tablename, Tablename]),
	file:close(Fd).

finish_file(L) ->
	{ok, Fd} = file:open(?MDBFILE, [append]),
	io:format(Fd, "~n%% insert data~ninsert_data([]) -> ok;~ninsert_data([H|T]) ->
	_R = mnesia:transaction(fun() -> mnesia:write(H) end),
	insert_data(T).~n~n%%start transform ~nstart() ->",[]),
	finish_file(Fd, L).

finish_file(Fd, [H]) ->
	io:format(Fd, "~n\tcase lists:member(~p, mnesia:system_info(tables)) of
	true ->
		io:format(\"Table:~p is exit !!!\");
	false ->
		create_table_~p(),
		init_data_~p(0);
	_ ->
		io:format(\"mnesia is not started !!!\")
	end.~n",[H,H,H,H]),
	file:close(Fd);
finish_file(Fd, [H|T]) ->
	io:format(Fd, "~n\tcase lists:member(~p, mnesia:system_info(tables)) of
	true ->
		io:format(\"Table:~p is exit !!!\");
	false ->
		create_table_~p(),
		init_data_~p(0);
	_ ->
		io:format(\"mnesia is not started !!!\")
	end,",[H,H,H,H]),
	finish_file(Fd, T).


