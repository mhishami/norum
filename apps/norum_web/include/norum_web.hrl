

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(DEBUG(Text, Args), lager:log(debug, ?MODULE, Text, Args)).
-define(INFO(Text, Args), lager:log(info, ?MODULE, Text, Args)).
-define(ERROR(Text, Args), lager:log(error, ?MODULE, Text, Args)).

-define(SALT, <<"This is supposed to be your salty words">>).

%% database
-define(DB_USER, <<"norum_user">>).
