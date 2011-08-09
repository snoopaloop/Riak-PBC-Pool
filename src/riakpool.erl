-module(riakpool).

-compile([{parse_transform, lager_transform}]).

-export([	start/0, stop/0,
			add_pool/2, add_pool/3, add_pool/4, 
			remove_pool/2, increment_pool_size/3, decrement_pool_size/3
		]).

-include("riakpool.hrl").

%% @spec start() -> ok
%% @doc Start the riakpool_app application.
%%
%% Calls application:start(riakpool_app).
%%
start() ->
	application:start(crypto),
	application:start(riakpool_app).

%% @spec stop()
%% @doc Stop the riakpool_app application
%%
stop() ->
	application:stop(riakpool_app).

%% @spec get_conn() -> #connection{} | {error, connection_lock_timeout}
%% @doc get a #connection{} record, which contains the pid() of a 
%% Riak connection in #connection.pid
get_conn() ->
	riakpool_conn_mgr:wait_for_connection().

%% @spec return_conn(Connection) -> ok | {error, pool_not_found} | {error, connection_not_found}
%%		Connection = #connection{}
%% @doc Return a connection to the pool
%%
return_conn(Connection) ->
	riakpool_conn_mgr:pass_connection(Connection).
	
%% automatically add pools when starting, allow the user to add more (one pool == one server)
add_pool(Host, Port) ->
	add_pool(Host, Port, [], ?DEFAULT_CONNECTION_COUNT).
add_pool(Host, Port, Size) ->
	add_pool(Host, Port, Size, []).
add_pool(Host, Port, Size, Opts) ->
	Pool = #pool{
		pool_id = riakpool_conn_mgr:pool_id(Host, Port),
		size = Size,
		host = Host,
		port = Port,
		opts = Opts
	},
	Pool1 = riakpool_conn_mgr:open_connections(Pool),
	riakpool_conn_mgr:add_pool(Pool1).

%% @spec remove_pool(Host, Port) -> ok
%%		Host = atom()
%%
%% @doc Synchronous call to the connection manager to remove a pool.
%%
%% === Implementation ===
%%
%% Relies on emysql_conn:close_connection(Conn) for the proper closing of connections. Feeds
%% any connection in the pool to it, also the locked ones.
%% @end doc: hd feb 11

remove_pool(Host, Port) ->
	Pool = riakpool_conn_mgr:remove_pool(riakpool_conn_mgr:pool_id(Host, Port)),
	[riakpool_conn_mgr:close_connection(Conn) || Conn <- lists:append(queue:to_list(Pool#pool.available), gb_trees:values(Pool#pool.locked))],
	ok.

%% @spec increment_pool_size(Host, Port, By) -> Result
%%		Host = host()
%% 		Port = port()
%%		By = integer()
%%		Result = ok | {error, pool_not_found}
%%
%% @doc Synchronous call to the connection manager to enlarge a pool.
%%
%% === Implementation ===
%%
%% Opens connections and then adds them to the pool by a call to
%% riakpool_conn_mgr:add_connections().
%%

increment_pool_size(Host, Port, Num) when is_integer(Num) ->
	PoolId = riakpool_conn_mgr:pool_id(Host, Port),
	Conns = riakpool_conn_mgr:open_n_connections(PoolId, Num),
	riakpool_conn_mgr:add_connections(PoolId, Conns).

%% @spec decrement_pool_size(Host, Port, By) -> ok
%%		Host = host()
%% 		Port = port()
%%		By = integer()
%%
%% @doc Synchronous call to the connection manager to shrink a pool.
%%
%% === Implementation ===
%%
%% First gets a list of target connections from riakpool_conn_mgr:remove_connections(), then
%% relies on riakc_pb_socket:stop(Pid()) for the proper closing of connections. 
%% 

decrement_pool_size(Host, Port, Num) when is_integer(Num) ->
	PoolId = riakpool_conn_mgr:pool_id(Host, Port),
	Conns = riakpool_conn_mgr:remove_connections(PoolId, Num),
	[riakpool_conn_mgr:close_connection(Conn) || Conn <- Conns],
	ok.
