-module(m2m_db_all).
-export([start/0]).

%%table:fenshu sync from mysql
-record('`test`.`fenshu`', {fid, name}).

%%create table:fenshu from mysql
create_table_fenshu() ->
	mnesia:create_table('`test`.`fenshu`', [{attributes, record_info(fields, '`test`.`fenshu`')},
  								{type, set}, {disc_copies, [node()]}]).
		
%%init_data for table:fenshu from mysql.
init_data_fenshu(N) ->
	Sql = "select * from test.fenshu where fid >" ++ erlang:integer_to_list(N) ++ " order by fid "++ " limit 1000",
	case mysql_db:select(Sql) of
		{ok, []} ->
			ok;
		{ok, Bindata} ->
			[Nowid|_] = lists:last(Bindata),
			Data = [erlang:list_to_tuple(['`test`.`fenshu`'|X])||  X <- Bindata],
			F = fun() ->
				insert_data(Data)
			end,
			mnesia:transaction(F),
			init_data_fenshu(Nowid);
		{no, Error} ->
			{no, Error}
	end.

%%table:storage_bin sync from mysql
-record('`test`.`storage_bin`', {id, created_time, last_modified_time, deleted, wh_id, class_id, section_id, bin_code, bin_type_id, length, width, height, measure_unit, max_volume, volume_unit, max_weight, weight_unit, state, bin_level_id, is_fix, x, y, z, channel, stack, layer, room, froze_state, up_assign_state, last_assign_time, channel_code, seq_no, creator_id, last_modifier_id, unique_code, class_code, section_code}).

%%create table:storage_bin from mysql
create_table_storage_bin() ->
	mnesia:create_table('`test`.`storage_bin`', [{attributes, record_info(fields, '`test`.`storage_bin`')},
  								{type, set}, {disc_copies, [node()]}]).
		
%%init_data for table:storage_bin from mysql.
init_data_storage_bin(N) ->
	Sql = "select * from test.storage_bin where id >" ++ erlang:integer_to_list(N) ++ " order by id "++ " limit 1000",
	case mysql_db:select(Sql) of
		{ok, []} ->
			ok;
		{ok, Bindata} ->
			[Nowid|_] = lists:last(Bindata),
			Data = [erlang:list_to_tuple(['`test`.`storage_bin`'|X])||  X <- Bindata],
			F = fun() ->
				insert_data(Data)
			end,
			mnesia:transaction(F),
			init_data_storage_bin(Nowid);
		{no, Error} ->
			{no, Error}
	end.

%%table:storage_bin_sort sync from mysql
-record('`test`.`storage_bin_sort`', {id, created_time, last_modified_time, deleted, wh_id, wh_code, task_type, bin_id, bin_code, storage_class_id, storage_class_code, storage_section_id, storage_section_code, activity_area_id, activity_area_code, order_seq, creator_id, last_modifier_id, pick_mode, bin_type_Id}).

%%create table:storage_bin_sort from mysql
create_table_storage_bin_sort() ->
	mnesia:create_table('`test`.`storage_bin_sort`', [{attributes, record_info(fields, '`test`.`storage_bin_sort`')},
  								{type, set}, {disc_copies, [node()]}]).
		
%%init_data for table:storage_bin_sort from mysql.
init_data_storage_bin_sort(N) ->
	Sql = "select * from test.storage_bin_sort where id >" ++ erlang:integer_to_list(N) ++ " order by id "++ " limit 1000",
	case mysql_db:select(Sql) of
		{ok, []} ->
			ok;
		{ok, Bindata} ->
			[Nowid|_] = lists:last(Bindata),
			Data = [erlang:list_to_tuple(['`test`.`storage_bin_sort`'|X])||  X <- Bindata],
			F = fun() ->
				insert_data(Data)
			end,
			mnesia:transaction(F),
			init_data_storage_bin_sort(Nowid);
		{no, Error} ->
			{no, Error}
	end.

%% insert data
insert_data([]) -> ok;
insert_data([H|T]) ->
	mnesia:write(H),
	insert_data(T).

%%start transform 
start() ->
	case lists:member('`test`.`storage_bin_sort`', mnesia:system_info(tables)) of
	true ->
		io:format("Table:'`test`.`storage_bin_sort`' is exit !!!");
	false ->
		create_table_storage_bin_sort(),
		init_data_storage_bin_sort(0);
	_ ->
		io:format("mnesia is not started !!!")
	end,
	case lists:member('`test`.`storage_bin`', mnesia:system_info(tables)) of
	true ->
		io:format("Table:'`test`.`storage_bin`' is exit !!!");
	false ->
		create_table_storage_bin(),
		init_data_storage_bin(0);
	_ ->
		io:format("mnesia is not started !!!")
	end,
	case lists:member('`test`.`fenshu`', mnesia:system_info(tables)) of
	true ->
		io:format("Table:'`test`.`fenshu`' is exit !!!");
	false ->
		create_table_fenshu(),
		init_data_fenshu(0);
	_ ->
		io:format("mnesia is not started !!!")
	end.
