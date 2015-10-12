-module(home_handler).
-behaviour(cowboy_http_handler).
-author ('Hisham Ismail <mhishami@gmail.com').

-include("norum_web.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    ?DEBUG("~p: in /", [?MODULE]),
    {ok, Content} = home_dtl:render([]),
    {ok, Req2} = cowboy_req:reply(200, [], Content, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
