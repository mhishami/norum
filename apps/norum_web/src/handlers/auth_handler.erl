-module(auth_handler).
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
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {QsVals, Req4} = cowboy_req:qs_vals(Req3),
    {ok, PostVals, Req5} = cowboy_req:body_qs(Req4),
    {Sid, Req6} = cowboy_req:cookie(<<"sid">>, Req5),

    ?DEBUG("Method: ~p, Path: ~p, QsVals: ~p, PostVals: ~p~n", 
        [Method, Path, QsVals, PostVals]),

    {ok, Req7} = process(Method, Path, [QsVals, PostVals, Sid], Req6),
    {ok, Req7, State}.

process(<<"GET">>, <<"/auth/login">>, _, Req) ->
    % ?DEBUG("Authenticating user, Req= ~p~n", [Req]),
    {ok, Content} = login_dtl:render([]),
    cowboy_req:reply(200, [], Content, Req);

process(<<"POST">>, <<"/auth/login">>, [_, PostVals, _], Req) ->
    Email = proplists:get_value(<<"email">>, PostVals),
    Password = proplists:get_value(<<"password">>, PostVals),
    ?DEBUG("Email: ~p, Password: ~p~n", [Email, Password]),

    Res = mongo_worker:find_one(?DB_USER, {<<"email">>, Email}),
    ?DEBUG("Db Result = ~p~n", [Res]),
    case Res of
        {error, not_found} ->
            %% redirect to registration
            cowboy_req:reply(302, [{<<"Location">>, <<"/auth/register">>}], [], Req);
        {ok, Data} ->
            %% validate user
            case authenticate(Password, Data) of
                ok ->
                    %% set session, and cookies etc.
                    Sid = web_util:hash_password(word_util:gen_pnr()),
                    % session_worker:set_cookies(Email, Sid),
                    session_worker:set_cookies(Sid, Email),
                    Req2 = cowboy_req:set_resp_cookie(<<"sid">>, Sid, 
                        [{path, <<"/">>}, {max_age, 3600}], Req),
                    %% redirect
                    cowboy_req:reply(302, [{<<"Location">>, ?PATH_ADM}], [], Req2);
                error ->
                    {ok, Content} = login_dtl:render([
                            {error, "Username, or password is invalid"},
                            {email, Email}
                        ]),
                    cowboy_req:reply(200, [], Content, Req)
            end
    end;
    
process(<<"GET">>, <<"/auth/register">>, _, Req) ->
    {ok, Content} = register_dtl:render([]),
    cowboy_req:reply(200, [], Content, Req);

process(<<"POST">>, <<"/auth/register">> = Action, [_, PostVals, _], Req) ->
    ?DEBUG("Action=~p, PostVals = ~p~n", [Action, PostVals]),

    Name = proplists:get_value(<<"name">>, PostVals),
    Email = proplists:get_value(<<"email">>, PostVals),
    Password = proplists:get_value(<<"password">>, PostVals),
    Password2 = proplists:get_value(<<"password2">>, PostVals),

    case Password =/= Password2 of
        true ->
            %% passwords are not the same
            {ok, Content} = register_dtl:render([
                {name, Name},
                {email, Email},
                {error, "Passwords are not the same"}
            ]),
            cowboy_req:reply(200, [], Content, Req);
        _ ->
            %% save the user
            User = users:new(Name, Email, web_util:hash_password(Password)),
            ?DEBUG("Action= ~p, User = ~p~n", [Action, User]),
            case mongo_worker:save(?DB_USER, User) of
                {ok, _} ->
                    cowboy_req:reply(302, [{<<"Location">>, <<"/">>}], [], Req);
                _ ->
                    {ok, Content} = register_dtl:render([
                        {error, "Cannot save user data. Pls come again!"}
                    ]),
                    cowboy_req:reply(200, [], Content, Req)
            end

    end;

process(<<"GET">>, <<"/auth/logout">>, [_, _, Sid], Req) ->
    %% clear cookies
    ?DEBUG("Clearing cookies, Sid= ~p~n", [Sid]),
    session_worker:del_cookies(Sid),
    Req2 = cowboy_req:set_resp_cookie(<<"sid">>, <<>>, [{max_age, 0}], Req),
    cowboy_req:reply(302, [{<<"Location">>, <<"/">>}], [], Req2);

process(_, _, _, Req) ->
    cowboy_req:reply(405, [], Req).

terminate(_Reason, _Req, _State) ->
    ok.

%% ----------------------------------------------------------------------------
%% Private funs
%%
authenticate(Password, Data) ->
    HassPass = web_util:hash_password(Password),
    Pass = maps:get(<<"password">>, Data),
    case HassPass =:= Pass of
        true -> ok;
        _    -> error
    end.
