-module(mongo_worker).
-behaviour(gen_server).
-author ('Hisham Ismail <mhishami@gmail.com').

-include("norum_db.hrl").

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {}).

%% API.
%% ----------------------------------------------------------------------------
-export([
    save/2,             %% save the records
    update/2,           %% update Doc
    find_one/2,         %% find first item by selector
    find/2,             %% find all items by selector
    find/3,             %% find all items by selector
    find/4,
    delete/2,
    test/0              %% test app
]).

save(Coll, Doc) ->
    gen_server:call(?MODULE, {save, Coll, Doc}).

update(Coll, Doc) when is_map(Doc) ->
    gen_server:call(?MODULE, {update, Coll, Doc}).

find_one(Coll, Selector) ->
    gen_server:call(?MODULE, {find_one, Coll, Selector}).

find(Coll, Selector) ->
    gen_server:call(?MODULE, {find, Coll, Selector, []}).

find(Coll, Selector, Projector) ->
    gen_server:call(?MODULE, {find, Coll, Selector, Projector}).

find(Coll, Selector, Projector, Limit) ->
    gen_server:call(?MODULE, {find, Coll, Selector, Projector, Limit}).

delete(Coll, Selector) ->
    gen_server:call(?MODULE, {delete, Coll, Selector}).

%% gen_server implementation.
%% ----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.
init([]) ->
    {ok, #state{}}.

handle_call({save, Coll, Doc}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Coll),
    Reply = mongo:insert(Conn, Coll, Doc),
    {reply, {ok, Reply}, State};

handle_call({update, Coll, Doc}, _From, State) ->
    ?DEBUG("Updating Doc= ~p~n", [Doc]),
    {ok, Conn} = mongo_pool:get(Coll),
    Id = maps:get(<<"_id">>, Doc),
    Reply = mongo:update(Conn, Coll, {<<"_id">>, Id}, {<<"$set">>, Doc}),
    {reply, {ok, Reply}, State};

handle_call({find_one, Coll, Selector}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Coll),
    Res = mongo:find_one(Conn, Coll, Selector),
    Reply = case maps:size(Res) of
                0 -> {error, not_found};
                _ -> {ok, Res}
            end,
    {reply, Reply, State};

handle_call({find, Coll, Selector, Projector}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Coll),
    Cursor = mongo:find(Conn, Coll, Selector, Projector),
    Res = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    {reply, {ok, Res}, State};

handle_call({find, Coll, Selector, Projector, Limit}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Coll),
    Cursor = mongo:find(Conn, Coll, Selector, Projector),
    Res = mc_cursor:take(Cursor, Limit),
    mc_cursor:close(Cursor),
    {reply, {ok, Res}, State};

handle_call({delete, Coll, Selector}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Coll),
    Reply = mongo:delete(Conn, Coll, Selector),
    {reply, {ok, Reply}, State};

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

%% ----------------------------------------------------------------------------
test() ->
    users:new(<<"Hisham Ismail">>, <<"hisham@mail.com">>, <<"sasa">>),
    ok.

