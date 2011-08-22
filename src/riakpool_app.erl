-module(riakpool_app).

-behaviour(application).

-compile([{parse_transform, lager_transform}]).

%% Application callbacks
-export([start/2, stop/1, get_env/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    riakpool_sup:start_link().

stop(_State) ->
    ok.

get_env(Name, Default) ->
	case application:get_env(riakpool, Name) of
		undefined ->
			Default;
		{ok, V} ->
			V
	end.