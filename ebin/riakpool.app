{application, riakpool,
 [
  {description, "Riak Protocol Buffer connection pool based on Emysql pooling"},
  {vsn, "0.1"},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
				  crypto
                 ]},
  {modules, [
		riakpool,
		riakpool_app,
		riakpool_conn_mgr,
		riakpool_sup
	]},
  {mod, { riakpool_app, []}},
  {env, [
			{lock_timeout, 1000},
			{conns, [{"127.0.0.1", 8087, 1, []}]}
	]}
 ]}.
