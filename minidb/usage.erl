-module(main).
-mode(compile).

main(_) ->
    {ok, Pid} = minidb:start(),
    minidb:get(Pid, foo),
    minidb:insert(Pid, foo, koko),
    minidb:get(Pid, foo),

    minidb:put(Pid, bar, 21),
    minidb:remove(Pid, bar),
    minidb:get(Pid, bar),

    minidb:put(Pid, blah, 42),
    Sum = minidb:sum(Pid),
    io:format("~p~n", [Sum]).
