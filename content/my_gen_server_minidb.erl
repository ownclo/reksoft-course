-module(my_gen_server_minidb).

-export([ handle_sync/2
        , handle_async/2
        , init/0
        ]).

-export([ start/0
        , stop/1
        , get/2
        , get_all/1  % for testing purposes
        , put/2
        ]).

%% erlc -pa . client.erl
-behaviour(my_gen_server).

handle_sync(State, {get, Name}) ->
    {lookup(Name, State), State};

handle_sync(State, get_all) ->
    {State, State}.

handle_async(State, {put, Name, Value}) ->
    insert({Name, Value}, State).

start() -> server:start(?MODULE).
stop(Server) -> Server ! stop.

init() -> [].

put(Server, {Name, Value}) ->
    call_async(Server, {put, Name, Value}).

get(Server, Name) ->
    call_sync(Server, {get, Name}).

get_all(Server) ->
    call_sync(Server, get_all).

%%% utility functions (pure)

% NOTE: these are replaceable by lists API
%       or by using Data.Map instead of [(Key, Value)].
% XXX:  Was written with no internet access, so
%       just hand-rolled.

%% lookup :: Key -> Store -> Maybe Value
%% O(n).
lookup(_Key, []) -> not_found;
lookup(Key, [{Key, Value} | _Rest]) -> Value;
lookup(Key, [ _ | Rest ]) -> lookup(Key, Rest).

%% insert :: Key -> Value -> Store -> Store
%% O(n) if filtering out, O(1) if not.
insert({NewKey, NewVal}, KVList) ->
    Filtered = [KV || ({Key, _} = KV) <- KVList, 
                      Key =/= NewKey
               ],
    [ {NewKey, NewVal} | Filtered ].
