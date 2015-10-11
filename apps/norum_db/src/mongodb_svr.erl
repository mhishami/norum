-module(mongodb_svr).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {conn, db}).

%% API.
%% ----------------------------------------------------------------------------
-export([
    save/1,             %% save the records
    update/1,           %% update data
    find_by_email/1,    %% find user by email
    test/0              %% test app
]).

save(Data) ->
    gen_server:call(?MODULE, {save, Data}).

update(Data) when is_map(Data) ->
    gen_server:call(?MODULE, {update, Data}).

find_by_email(Email) when is_binary(Email) ->
    gen_server:call(?MODULE, {find_by_email, Email}).

%% gen_server implementation.
%% ----------------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% gen_server.
init(Database) ->
    {ok, Conn} = mongo:connect([{database, Database}]),
    {ok, #state{conn = Conn, db = Database}}.

handle_call({save, Data}, _From, State) ->
    Reply = mongo:insert(State#state.conn, State#state.db, Data),
    {reply, {ok, Reply}, State};

handle_call({update, Data}, _From, State) ->
    Command = {<<"$set">>, Data},
    Id = maps:get(<<"_id">>, Data),
    Reply = mongo:update(State#state.conn, State#state.db, {<<"_id">>, Id}, Command),
    {reply, {ok, Reply}, State};

handle_call({find_by_email, Email}, _From, State) ->
    Res = mongo:find_one(State#state.conn, State#state.db, {<<"email">>, Email}),
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

