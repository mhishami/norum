-module(norum_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Web = ?CHILD(norum_web_sup, supervisor),
	Db = ?CHILD(norum_db_sup, supervisor),
	Session = ?CHILD(norum_session_sup, supervisor),

    {ok, { {one_for_one, 5, 10}, [Web, Db, Session]} }.

