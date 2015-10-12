-module(norum_web_sup).
-behaviour(supervisor).
-author ('Hisham Ismail <mhishami@gmail.com').

-include("norum_web.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    WebWorker = ?CHILD(web_worker, worker),
    {ok, {{one_for_one, 1, 5}, [WebWorker]}}.
