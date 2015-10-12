-module(session_worker).
-behaviour(gen_server).
-author ('Hisham Ismail <mhishami@gmail.com').

-include("session.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {nodes}).

%% API.
-export([
        set_cookies/2,
        get_cookies/1
    ]).

%% ----------------------------------------------------------------------------
%% API
set_cookies(Key, Val) ->
    gen_server:call(?MODULE, {set_cookies, Key, Val}).

get_cookies(Key) ->
    gen_server:call(?MODULE, {get_cookies, Key}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ----------------------------------------------------------------------------
%% gen_server.
init([]) ->
    % ?DEBUG("~p: Initializing...~n", [?MODULE]),
    application:stop(mnesia),
    Nodes = [node()|nodes()],
    % MnesiaDir = application:get_env(norum, mnesia_dir),
    % application:set_env(mnesia, dir, MnesiaDir),
    % application:set_env(mnesia, dir, "priv/db"),
    mnesia:create_schema(Nodes),
    
    % rpc:multicall(Nodes, application, start, [mnesia]),
    application:start(mnesia),
    mnesia:create_table(norum_session, [
        {attributes, record_info(fields, norum_session)},
        {disc_copies, Nodes},
        {type, set}
    ]),
    mnesia:create_table(norum_cookies, [
        {attributes, record_info(fields, norum_cookies)},
        {disc_copies, Nodes},
        {type, set}
    ]),

    mnesia:wait_for_tables([norum_session, norum_cookies], 1000),
    {ok, #state{nodes = Nodes}}.

handle_call({set_cookies, Key, Val}, _From, State) ->
    F = fun() -> mnesia:write(#norum_cookies{key=Key, val=Val}) end,
    ok = mnesia:activity(transaction, F),
    {reply, {ok, success}, State};

handle_call({get_cookies, Key}, _From, State) ->
    F = fun() -> mnesia:read({norum_cookies, Key}) end,
    Reply = case catch mnesia:activity(transaction, F) of
                {'EXIT', _} -> {error, undefined};
                Val         -> {ok, Val}
            end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
