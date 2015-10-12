-module(auth_handler).
-behaviour(cowboy_http_handler).
-author ('Hisham Ismail <mhishami@gmail.com').

-include("norum_web.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([
        hash_password/1,
        authenticate/2
]).

-record(state, {
}).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {QsVals, Req4} = cowboy_req:qs_vals(Req3),
    {ok, PostVals, Req5} = cowboy_req:body_qs(Req4),

    ?DEBUG("~p: Method: ~p, Path: ~p, QsVals: ~p, PostVals: ~p~n", 
        [?MODULE, Method, Path, QsVals, PostVals]),

    {ok, Req6} = process(Method, Path, [QsVals, PostVals], Req5),
    {ok, Req6, State}.

process(<<"GET">>, <<"/auth/login">>, _, Req) ->
    {ok, Content} = login_dtl:render([]),
    cowboy_req:reply(200, [], Content, Req);

process(<<"POST">>, <<"/auth/login">>, [_, PostVals], Req) ->
    Email = proplists:get_value(<<"email">>, PostVals),
    Password = proplists:get_value(<<"password">>, PostVals),
    ?DEBUG("~p: Email: ~p, Password: ~p~n", [?MODULE, Email, Password]),

    Res = mongo_worker:find_one(?DB_USER, {<<"email">>, Email}),
    ?DEBUG("~p: Db Result = ~p~n", [?MODULE, Res]),
    case Res of
        {error, not_found} ->
            %% redirect to registration
            cowboy_req:reply(302, [{<<"Location">>, <<"/auth/register">>}], [], Req);
        {ok, Data} ->
            %% validate user
            case authenticate(Password, Data) of
                ok ->
                    %% set cookies etc.

                    %% redirect
                    cowboy_req:reply(302, [{<<"Location">>, <<"/">>}], [], Req);
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

process(<<"POST">>, <<"/auth/register">> = Action, [_, PostVals], Req) ->
    ?DEBUG("~p: ~p, PostVals = ~p~n", [?MODULE, Action, PostVals]),

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
            User = users:new(Name, Email, hash_password(Password)),
            ?DEBUG("~p: ~p, User = ~p~n", [?MODULE, Action, User]),
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

process(_, _, _, Req) ->
    cowboy_req:reply(405, [], Req).

terminate(_Reason, _Req, _State) ->
    ok.

%% ----------------------------------------------------------------------------
authenticate(Password, Data) ->
    HassPass = hash_password(Password),
    Pass = maps:get(<<"password">>, Data),
    case HassPass =:= Pass of
        true -> ok;
        _    -> error
    end.

hash_password(Password) ->
    {ok, Pass} = pbkdf2:pbkdf2(sha, Password, ?SALT, 4096, 20),
    pbkdf2:to_hex(Pass).
