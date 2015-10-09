-module(norum_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	application:start(sync),
	application:ensure_all_started(lager),
	application:start(norum_session),
	application:start(norum_db),
	application:start(norum_web),
	
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/static/[...]", cowboy_static, {priv_dir, norum, "static",
				[{mimetypes, cow_mimetypes, all}]}},
			{"/", home_handler, []},
			{"/auth/[...]", auth_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),

    norum_sup:start_link().

stop(_State) ->
    ok.
