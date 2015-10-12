-module(adm_handler).
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
    ?DEBUG("~p: in /adm: Req= ~p~n", [?MODULE, Req]),
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {QsVals, Req4} = cowboy_req:qs_vals(Req3),
    {ok, PostVals, Req5} = cowboy_req:body_qs(Req4),
    {Sid, Req6} = cowboy_req:cookie(<<"sid">>, Req5),

	?DEBUG("~p: Method= ~p, Path= ~p, QsVals= ~p, PostVals=~p, Sid=~p~n",
		[?MODULE, Method, Path, QsVals, PostVals, Sid]),
    {ok, Email} = session_worker:get_cookies(Sid),
    {ok, User} = mongo_worker:find_one(?DB_USER, {<<"email">>, Email}),
    ?DEBUG("~p: User = ~p~n", [?MODULE, User]),

    {ok, Content} = admin_dtl:render([{user, maps:get(<<"name">>, User)}]),
    {ok, Req7} = cowboy_req:reply(200, [], Content, Req6),
    {ok, Req7, State}.

terminate(_Reason, _Req, _State) ->
    ok.
