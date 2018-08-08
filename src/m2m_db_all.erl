-module(m2m_db_all).
-export([start/0]).

%%table:product sync from mysql
-record(product, {id, created_time, last_modified_time, deleted, wh_id, merchant_id, ep_id, category_id, product_code, product_name, product_type, note, origin_place, storage_bin_type_id, wh_product_group_id, wh_storage_condition, wh_processing_status, transport_group_id, load_unload_group_id, shelf_life, remaining_shelf_life, shelf_life_unit_id, shelf_life_percent, ep_product_id, ep_product_code, batch_mgmt_req, catalog_no, hazard_indicator, reference_2, bom_category, entry_source, creator_id, last_modifier_id, unique_code, second_picking_tag, validation_tag}).

%%create table:product from mysql
create_table_product() ->
	mnesia:create_table(product, [{attributes, record_info(fields, product)},
  								{type, set}, {disc_copies, [node()]}]).
		
%%init_data for table:product from mysql.
init_data_product(N) ->
	Sql = "select * from wms_master_data.product where id >" ++ erlang:integer_to_list(N) ++ " order by id "++ " limit 1000",
	case mysql_db:select(Sql) of
		{ok, []} ->
			ok;
		{ok, Bindata} ->
			[Nowid|_] = lists:last(Bindata),
			Data = [erlang:list_to_tuple(['product'|X])||  X <- Bindata],
			insert_data(Data),
			init_data_product(Nowid);
		{no, Error} ->
			{no, Error}
	end.

%% insert data
insert_data([]) -> ok;
insert_data([H|T]) ->
	_R = mnesia:transaction(fun() -> mnesia:write(H) end),
	insert_data(T).

%%start transform 
start() ->
	case lists:member(product, mnesia:system_info(tables)) of
	true ->
		io:format("Table:product is exit !!!");
	false ->
		create_table_product(),
		init_data_product(0);
	_ ->
		io:format("mnesia is not started !!!")
	end.
