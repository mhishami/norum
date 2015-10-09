-module(norum_web_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    application:ensure_all_started(cowboy),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/static/[...]", cowboy_static, {priv_dir, norum_web, "static",
                [{mimetypes, cow_mimetypes, all}]}},
            {"/", home_handler, []},
            {"/auth/[...]", auth_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
        {env, [{dispatch, Dispatch}]}
    ]),

    norum_web_sup:start_link().

stop(_State) ->
    ok.
