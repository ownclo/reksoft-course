-module(kvlist).
-compile([export_all]).

%% LOGIC
%% O(n).
lookup([], _Key) -> not_found;
lookup([{Key, Value} | _Rest], Key) -> Value;
lookup([ _ | Rest ], Key) -> lookup(Rest, Key).

%% O(n) if filtering out, O(1) if not.
insert(KVList, NewKey, NewVal) ->
    Filtered = delete(KVList, NewKey),
    [ {NewKey, NewVal} | Filtered ].

delete(KVList, Key) ->
    % delete key = filter ((/= key) . fst)
    NotMatching = fun({Candidate, _}) ->
            Candidate =/= Key
    end,
    lists:filter(NotMatching, KVList).

sum(KVList) ->
    % sum . map snd
    Sum = fun({_Key, Elem}, Acc) ->
            if is_number(Elem) -> 
                    Elem + Acc;
                true -> Acc
            end
    end,
    lists:foldl(Sum, 0, KVList).
