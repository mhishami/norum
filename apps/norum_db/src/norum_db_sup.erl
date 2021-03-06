-module(norum_db_sup).
-behaviour(supervisor).
-author ('Hisham Ismail <mhishami@gmail.com').

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MongoWorker = ?CHILD(mongo_worker, worker),
    MongoPool = ?CHILD(mongo_pool, worker),
    {ok, {{one_for_one, 1, 5}, [MongoWorker, MongoPool]}}.
