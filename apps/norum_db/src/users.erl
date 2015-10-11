-module(users).

-export[ 
    new/3,
    save/1
].

-define(DB, <<"norum_user">>).
-define(CONN, mongo:connect([{database, ?DB}]).

new(Name, Email, Password) when is_binary(Name), 
                                is_binary(Email), 
                                is_binary(Password) ->
    #{<<"name">> => Name,
      <<"email">> => Email,
      <<"password">> => Password}.

save(Data) when is_map(Data) ->
    {ok, C} = mongo:connect([{database, ?DB}]),
    mongo:insert(C, ?DB, Data).
