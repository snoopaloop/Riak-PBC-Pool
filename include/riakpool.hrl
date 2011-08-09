-type host() :: string() | atom() | inet:ip_address().
-type port() :: non_neg_integer().

-record(pool, {
	pool_id :: binary(), 
	size :: non_neg_integer(), 
	host :: host(), 
	port :: port(), 
	available :: queue(), 
	locked :: gb_trees(), 
	opts :: [tuple()]
}).
-record(connection, {
	id :: list(), 
	pool_id :: binary(), 
	pid :: pid(), 
	locked_at :: integer(), 
	spawn_time :: integer()
}).

-define(LOCK_TIMEOUT, 5000).
-define(DEFAULT_CONNECTION_COUNT, 1).