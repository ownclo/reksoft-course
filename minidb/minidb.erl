-module(minidb).
-behaviour(gen_server).

%% GEN_SERVER Exports
-export([ init/1
        , handle_call/3  % RPC-style
        , handle_cast/2  % Async
        , terminate/2    % Aborting?
        , handle_info/2  % If server is called directly
        , code_change/3  % MAGIC
        ]).

%% Public API
-export([ get/2
        , put/3
        , delete/2
        , sum/1
        , start/0
        , stop/1
        ]).

%% API
start()                  -> gen_server:start(?MODULE, go, []).
stop(Server)             -> gen_server:cast(Server, stop).
get(Server, Name)        -> gen_server:call(Server, {get, Name}).
sum(Server)              -> gen_server:call(Server, {sum}).
put(Server, Name, Value) -> gen_server:cast(Server, {put, Name, Value}).
delete(Server, Name)     -> gen_server:cast(Server, {delete, Name}).

% TODO: - implement 'delete'. cast
%       - implement 'sum'. Returns a sum of values in the list.
%         is_number(Var) -> true, false. Ignore non-numbers.
%       - (optional). What other requests can be useful.

%% GEN_SERVER Callbacks
% 'go' is for configuration (not used)
init(go) -> {ok, []}.

% XXX: Do not use _From directly,
% but the possibility is here.
% {reply, result, new_state}
handle_call({get, Name}, _From, DB) ->
    {reply, kvlist:lookup(DB, Name), DB};

handle_call({sum}, _From, DB) ->
    {reply, kvlist:sum(DB), DB}.

handle_cast({put, Name, Value}, DB) ->
    {noreply, kvlist:insert(DB, Name, Value)};

handle_cast({delete, Name}, DB) ->
    {noreply, kvlist:delete(DB, Name)};

handle_cast(stop, DB) -> {stop, normal, DB}.

% terminate will be called after handle_cast(stop).
% it is needed for cleaning resources.
terminate(_Reason, _DB) -> ok.

handle_info(_Message, DB) -> {noreply, DB}.
code_change(_Prev, State, _Extra) -> {ok, State}.
