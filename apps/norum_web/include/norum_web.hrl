

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(DEBUG(Text, Args), lager:log(debug, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).
-define(INFO(Text, Args), lager:log(info, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).
-define(ERROR(Text, Args), lager:log(error, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).

-define(SALT, <<"This is supposed to be your salty words">>).

%% database
-define(DB_USER, <<"norum_user">>).

%% Path
-define(PATH_LOGIN, <<"/auth/login">>).
-define(PATH_ADM, <<"/adm/users">>).
-define(PATH_HOME, <<"/">>).
