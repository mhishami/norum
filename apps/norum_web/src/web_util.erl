-module (web_util).
-author ('Hisham Ismail <mhishami@gmail.com').

-include ("norum_web.hrl").

-export ([hash_password/1]).
-export ([hash_password/2]).
-export ([map_to_list/1]).
-export ([maps_to_list/1]).

hash_password(Password) ->
    hash_password(Password, 20).
    
hash_password(Password, Len) ->
    {ok, Pass} = pbkdf2:pbkdf2(sha, Password, ?SALT, 4096, Len),
    pbkdf2:to_hex(Pass).

map_to_list(Map) ->
    lists:zip(maps:keys(Map), maps:values(Map)).

maps_to_list(Maps) ->
    maps_to_list(Maps, []).

maps_to_list([H|T], Accu) ->
    % ?DEBUG("Accu= ~p~n", [Accu]),
    Res = lists:zip(maps:keys(H), maps:values(H)),
    maps_to_list(T, Accu ++ [Res]);

maps_to_list([], Accu) ->
    Accu.
