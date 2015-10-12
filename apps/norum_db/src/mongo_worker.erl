-module(mongo_worker).
-behaviour(gen_server).

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
    update/2,           %% update data
    find_one/2,    %% find user by email
    test/0              %% test app
]).

save(Db, Data) ->
    gen_server:call(?MODULE, {save, Db, Data}).

update(Db, Data) when is_map(Data) ->
    gen_server:call(?MODULE, {update, Db, Data}).

find_one(Db, Selector) ->
    gen_server:call(?MODULE, {find_one, Db, Selector}).

%% gen_server implementation.
%% ----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.
init([]) ->
    {ok, #state{}}.

handle_call({save, Db, Data}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Db),
    Reply = mongo:insert(Conn, Db, Data),
    {reply, {ok, Reply}, State};

handle_call({update, Db, Data}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Db),
    Command = {<<"$set">>, Data},
    Id = maps:get(<<"_id">>, Data),
    Reply = mongo:update(Conn, Db, {<<"_id">>, Id}, Command),
    {reply, {ok, Reply}, State};

handle_call({find_one, Db, Selector}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Db),
    Res = mongo:find_one(Conn, Db, Selector),
    Reply = case maps:size(Res) of
                0 -> {error, not_found};
                _ -> {ok, Res}
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

%% ----------------------------------------------------------------------------
test() ->
    users:new(<<"Hisham Ismail">>, <<"hisham@mail.com">>, <<"sasa">>),
    ok.

