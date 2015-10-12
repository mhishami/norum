
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(DEBUG(Text, Args), lager:log(debug, ?MODULE, Text, Args)).
-define(INFO(Text, Args), lager:log(info, ?MODULE, Text, Args)).
-define(ERROR(Text, Args), lager:log(error, ?MODULE, Text, Args)).


%% record to keep the key, value
-record(norum_session, {key, val, expiry, timestamp = {date(), time()}}).
-record(norum_cookies, {key, val}).
