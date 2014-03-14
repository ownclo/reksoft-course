-module(main).
-compile([export_all]).
-mode(compile).

main(_) ->
    {ok, Pid} = minidb:start(),
    io:format("starting with an empty table...~n"),
    io:format("get foo: ~p~n",   [ minidb:get(Pid, foo)]),
    io:format("put foo: ~p~n",   [ minidb:put(Pid, foo, koko)]),
    io:format("get foo: ~p~n~n", [ minidb:get(Pid, foo)]),

    io:format("{bar, 21}:  ~p~n",   [ minidb:put(Pid, bar, 21)]),
    io:format("delete bar: ~p~n",   [ minidb:delete(Pid, bar)]),
    io:format("get bar:    ~p~n~n", [ minidb:get(Pid, bar)]),

    io:format("{blah, 42}: ~p~n", [ minidb:put(Pid, blah, 42)]),
    io:format("sum: ~p~n",        [ minidb:sum(Pid)]),
    io:format("stop: ~p~n",       [ minidb:stop(Pid)]),

    ok.
