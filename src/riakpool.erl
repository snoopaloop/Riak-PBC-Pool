-module(riakpool).

-compile([{parse_transform, lager_transform}]).

-export([	start/0, stop/0,
			add_pool/2, add_pool/3, add_pool/4, 
			remove_pool/2, increment_pool_size/3, decrement_pool_size/3
		]).

% for record and constant defines
-include("riakpool.hrl").

%% @spec start() -> ok
%% @doc Start the Emysql application.
%%
%% Simply calls `application:start(emysql).'
%%
%% === From the Erlang Manual ===
%% If the application is not already loaded, the application controller will
%% first load it using application:load/1. It will check the value of the
%% applications key, to ensure that all applications that should be started
%% before this application are running. The application controller then
%% creates an application master for the application. The application master
%% is the group leader of all the processes in the application. The
%% application master starts the application by calling the application
%% callback function start/2 in the module, and with the start argument,
%% defined by the mod key in the .app file.
%%
%% application:start(Application) is the same as calling 
%% application:start(Application, temporary). If a temporary application
%% terminates, this is reported but no other applications are terminated.
%%
%% See [http://www.erlang.org/doc/design_principles/applications.html]
%% @end doc: hd feb 11
%%
start() ->
	application:start(crypto),
	application:start(riakpool_app).

stop() ->
	application:stop(riakpool_app).

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

%% @spec remove_pool(PoolId) -> ok
%%		PoolId = atom()
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

%% @spec increment_pool_size(PoolId, By) -> Result
%%		PoolId = atom()
%%		By = integer()
%%		Result = {reply, ok, State1} | {reply, {error, pool_not_found}, State}
%%
%% @doc Synchronous call to the connection manager to enlarge a pool.
%%
%% This opens n=By new connections and adds them to the pool of id PoolId.
%%
%% === Implementation ===
%%
%% Opens connections and then adds them to the pool by a call to
%% emysql_conn_mgr:add_connections().
%% 
%% That this function exposes the State and possibly pool_not_found
%% seems to be inconsistent with decrement_pool_size(), which invariably
%% returns 'ok'.
%% @end doc: hd feb 11

increment_pool_size(Host, Port, Num) when is_integer(Num) ->
	PoolId = riakpool_conn_mgr:pool_id(Host, Port),
	Conns = riakpool_conn_mgr:open_n_connections(PoolId, Num),
	riakpool_conn_mgr:add_connections(PoolId, Conns).

%% @spec decrement_pool_size(PoolId, By) -> ok
%%		PoolId = atom()
%%		By = integer()
%%
%% @doc Synchronous call to the connection manager to shrink a pool.
%%
%% This reduces the connections by up to n=By, but it only drops and closes available
%% connections that are not in use at the moment that this function is called. Connections
%% that are waiting for a server response are never dropped. In heavy duty, this function 
%% may thus do nothing.
%%
%% If 'By' is higher than the amount of connections or the amount of available connections,
%% exactly all available connections are dropped and closed. 
%%
%%
%% === Implementation ===
%%
%% First gets a list of target connections from emysql_conn_mgr:remove_connections(), then
%% relies on emysql_conn:close_connection(Conn) for the proper closing of connections. 
%% @end doc: hd feb 11
%% 

decrement_pool_size(Host, Port, Num) when is_integer(Num) ->
	PoolId = riakpool_conn_mgr:pool_id(Host, Port),
	Conns = riakpool_conn_mgr:remove_connections(PoolId, Num),
	[riakpool_conn_mgr:close_connection(Conn) || Conn <- Conns],
	ok.
