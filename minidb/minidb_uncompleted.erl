-module(minidb).
%% TODO: Finish that.

-export([ start/0
        , stop/1
        , get/2
        , get_all/1  % for testing purposes
        , put/2
        ]).

-behaviour(my_gen_server).
%% erlc -pa . client.erl

handle_sync(State, {get, Name}) ->
    {get(State, Name), State}.

handle_async(State, {put, Name, Value}) ->
    put(State, {Name, Value}).

start() -> server:start(?MODULE).

init() -> [].

put(Server, {Name, Value}) ->
    handle_async( %%%%%%%% TODO TODO.

get(Pid, Name) ->
    Pid ! {get, Name, self()},
    receive Val -> Val end.

get_all(Pid) ->
    Pid ! {get_all, self()},
    receive Store -> Store end.

put(Pid, KeyVal) ->

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
