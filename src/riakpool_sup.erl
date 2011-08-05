
-module(riakpool_sup).

-compile([{parse_transform, lager_transform}]).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Opts), {I, {I, start_link, [Opts]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	error_logger:info_msg("starting sup"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, {{one_for_one, 10, 100},
		[
			{riakpool_mgr, 
				{riakpool_conn_mgr, start_link, []},
				permanent,
				5000,
				worker,
				[riakpool_conn_mgr]}
		]
	}}.