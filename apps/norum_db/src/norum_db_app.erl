-module(norum_db_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    application:ensure_all_started(mongodb),    
    norum_db_sup:start_link().

stop(_State) ->
    ok.
