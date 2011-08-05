-record(pool, {pool_id, size, host, port, available=queue:new(), locked=gb_trees:empty(), opts}).
-record(connection, {id, pool_id, pid, locked_at, spawn_time}).

-define(LOCK_TIMEOUT, 5000).
-define(DEFAULT_CONNECTION_COUNT, 1).