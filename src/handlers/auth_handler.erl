-module(auth_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	{Method, Req2} = cowboy_req:method(Req),
	{Path, Req3} = cowboy_req:path(Req2),
	{QsVals, Req4} = cowboy_req:qs_vals(Req3),
	{ok, PostVals, Req5} = cowboy_req:body_qs(Req4),

	lager:log(debug, ?MODULE, 
		"~p: Method: ~p, Path: ~p, QsVals: ~p, PostVals: ~p~n", 
		[?MODULE, Method, Path, QsVals, PostVals]),

	{ok, Req6} = process(Method, Path, [QsVals, PostVals], Req5),
	{ok, Req6, State}.

process(<<"GET">>, <<"/auth/login">>, _, Req) ->
	{ok, Content} = login_dtl:render([]),
	cowboy_req:reply(200, [], Content, Req);

process(<<"POST">>, <<"/auth/login">>, [_, PostVals], Req) ->
	Email = proplists:get_value(<<"email">>, PostVals),
	Password = proplists:get_value(<<"password">>, PostVals),

	lager:log(debug, ?MODULE, "~p: Email: ~p, Password: ~p~n", [?MODULE, Email, Password]),
	cowboy_req:reply(302, [{<<"Location">>, <<"/">>}], [], Req);
	
process(_, _, _, Req) ->
	cowboy_req:reply(405, [], Req).

terminate(_Reason, _Req, _State) ->
	ok.