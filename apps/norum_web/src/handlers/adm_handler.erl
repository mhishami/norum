-module(adm_handler).
-behaviour(cowboy_http_handler).
-author ('Hisham Ismail <mhishami@gmail.com').

-include("norum_web.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {user}).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {QsVals, Req4} = cowboy_req:qs_vals(Req3),
    {ok, PostVals, Req5} = cowboy_req:body_qs(Req4),
    {Sid, Req6} = cowboy_req:cookie(<<"sid">>, Req5),

	?DEBUG("Method= ~p, Path= ~p, QsVals= ~p, PostVals= ~p, Sid= ~p~n",
		[Method, Path, QsVals, PostVals, Sid]),
    case catch session_worker:get_cookies(Sid) of
        {ok, Email} ->
            ?DEBUG("User= ~p~n", [Email]),
            case catch mongo_worker:find_one(?DB_USER, {<<"email">>, Email}) of
                {ok, User} ->
                    ?DEBUG("Processing adm page, User = ~p~n", [User]),
                    process(Method, Path, [QsVals, PostVals, User], Req6, State);
                _ ->
                    %% redirect to login
                    redirect(Req6, ?PATH_LOGIN)
            end;
        _ ->
            %% redirect to login
            redirect(Req6, ?PATH_LOGIN)
    end.

process(<<"GET">>, <<"/adm/users">>, [_, _, User], Req, State) ->
    ?DEBUG("Generating adm page for User= ~p...~n", [User]),
    Email = maps:get(<<"email">>, User),
    Name = maps:get(<<"name">>, User),
    {ok, Users} = mongo_worker:find(?DB_USER, {}, [], 10),
    {ok, Content} = adm_users_dtl:render([
            {user, Email},
            {name, Name},
            {users, web_util:maps_to_list(Users)}
        ]),
    {ok, Req2} = cowboy_req:reply(200, [], Content, Req),
    {ok, Req2, State};

process(<<"POST">>, <<"/adm/users/edit">>, [_, PostVals, User], Req, State) ->
    ?DEBUG("PostVals= ~p~n", [PostVals]),
    Email = maps:get(<<"email">>, User),
    Uid = proplists:get_value(<<"id">>, PostVals),
    ?DEBUG("ID= ~p~n", [Uid]),
    {ok, Data} = mongo_worker:find_one(?DB_USER, {<<"email">>, Uid}),
    ?DEBUG("Email= ~p, Data= ~p", [Email, Data]),    
    {ok, Content} = user_edit_dtl:render([
            {user, Email},
            {name, maps:get(<<"name">>, Data)},
            {email, Email}
        ]),
    {ok, Req2} = cowboy_req:reply(200, [], Content, Req),
    {ok, Req2, State};

process(<<"POST">>, <<"/adm/users/edit/changeinfo">>, [_, PostVals, _], Req, _State) ->
    ?DEBUG("Update info: PostVals= ~p~n", [PostVals]),
    Name  = proplists:get_value(<<"name">>, PostVals),
    Email = proplists:get_value(<<"email">>, PostVals),
    {ok, User} = mongo_worker:find_one(?DB_USER, {<<"email">>, Email}),
    User2 = User#{<<"name">> := Name, <<"email">> := Email},
    mongo_worker:update(?DB_USER, User2),
    redirect(Req, ?PATH_ADM);


process(<<"POST">>, <<"/adm/users/edit/changepwd">>, [_, PostVals, _], Req, State) ->
    ?DEBUG("Update password: PostVals= ~p~n", [PostVals]),
    Name  = proplists:get_value(<<"name">>, PostVals),
    Email = proplists:get_value(<<"email">>, PostVals),
    Pass1 = proplists:get_value(<<"password">>, PostVals),
    Pass2 = proplists:get_value(<<"password2">>, PostVals),

    case Pass1 =/= Pass2 orelse Pass1 =:= <<>> orelse Pass2 =:= <<>> of
        true ->
            %% show error
            {ok, Content} = user_edit_dtl:render([
                    {user, Email},
                    {name, Name},
                    {email, Email},
                    {pwd_error, <<"Bad password!">>}
                ]),
            {ok, Req2} = cowboy_req:reply(200, [], Content, Req),
            {ok, Req2, State};
        false ->
            %% update password
            {ok, User} = mongo_worker:find_one(?DB_USER, {<<"email">>, Email}),
            User2 = User#{<<"password">> := web_util:hash_password(Pass2)},
            mongo_worker:update(?DB_USER, User2),
            redirect(Req, ?PATH_ADM)
    end;

process(<<"POST">>, <<"/adm/users/delete">>, [_, PostVals, _], Req, _State) ->
    ?DEBUG("PostVals= ~p~n", [PostVals]),
    Uid = proplists:get_value(<<"id">>, PostVals),
    ?DEBUG("ID= ~p~n", [Uid]),
    mongo_worker:delete(?DB_USER, {<<"email">>, Uid}),
    redirect(Req, ?PATH_ADM);

process(_, _, _, Req, _) ->
    ?ERROR("Opps! Reached down the line...~n", []),
    redirect(Req, ?PATH_HOME).

terminate(_Reason, _Req, _State) ->
    ok.

redirect(Req, Path) ->
    cowboy_req:reply(302, [{<<"Location">>, Path}], [], Req).

