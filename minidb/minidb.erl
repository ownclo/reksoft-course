-module(minidb).

-export([ start/0
        , stop/1
        , get/2
        , get_all/1  % for testing purposes
        , put/2
        ]).

start() ->
    Init = [],
    spawn(fun() -> server(Init) end).

stop(Pid) ->
    Pid ! stop,
    ok.

get(Pid, Name) ->
    Pid ! {get, Name, self()},
    receive Val -> Val end.

get_all(Pid) ->
    Pid ! {get_all, self()},
    receive Store -> Store end.

put(Pid, KeyVal) ->
    Pid ! {put, KeyVal},
    ok.

%% server loop will be abstracted out by OTP
server(Store) ->
    receive
        {get, Name, Pid} ->
            Pid ! lookup(Name, Store),
            server(Store);
        {get_all, Pid} ->
            Pid ! Store,
            server(Store);
        {put, {Key, Val}} ->
            NewStore = insert({Key, Val}, Store),
            server(NewStore);
        stop -> ok
    end.


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
