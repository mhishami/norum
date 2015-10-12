-module(norum_session_app).
-behaviour(application).
-author ('Hisham Ismail <mhishami@gmail.com').

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    norum_session_sup:start_link().

stop(_State) ->
    ok.
