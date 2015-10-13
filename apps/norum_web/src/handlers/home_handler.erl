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
    % ?DEBUG("in /: Req= ~p~n", [Req]),
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {QsVals, Req4} = cowboy_req:qs_vals(Req3),
    {ok, PostVals, Req5} = cowboy_req:body_qs(Req4),
    {Sid, Req6} = cowboy_req:cookie(<<"sid">>, Req5),

	?DEBUG("Method= ~p, Path= ~p, QsVals= ~p, PostVals= ~p, Sid= ~p~n",
		[Method, Path, QsVals, PostVals, Sid]),
    case catch session_worker:get_cookies(Sid) of
        {ok, Email} ->
        	process(Email, Req6, State);
        _ ->
            process([], Req6, State)
            
    end.

process([], Req, State) ->
	{ok, Content} = home_dtl:render([]),
    {ok, Req2} = cowboy_req:reply(200, [], Content, Req),
    {ok, Req2, State};

process(User, Req, State) ->
	{ok, Content} = home_dtl:render([{user, User}]),
    {ok, Req2} = cowboy_req:reply(200, [], Content, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
