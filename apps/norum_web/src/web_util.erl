-module (web_util).
-author ('Hisham Ismail <mhishami@gmail.com').

-include ("norum_web.hrl").

-export ([hash_password/1]).
-export ([hash_password/2]).

hash_password(Password) ->
	hash_password(Password, 20).
	
hash_password(Password, Len) ->
    {ok, Pass} = pbkdf2:pbkdf2(sha, Password, ?SALT, 4096, Len),
    pbkdf2:to_hex(Pass).

