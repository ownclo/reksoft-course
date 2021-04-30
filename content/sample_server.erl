-module(main).
-compile([export_all]).
-mode(compile).

main([]) -> client().

server(N) ->
    receive
        {get, Pid} ->
            Pid ! {answer, N},
            server(N+1);
        stop -> ok
    end.

get_n(Server) ->
    Server ! {get, self()},
    receive
        {answer, M} -> M
    after 1000 -> error % timeout в миллисекундах
    end.

start() -> spawn(fun () -> server(0) end).
stop(Server) -> Server ! stop.

%% в другом файле.
client() ->
    Pid = start(),
    % ~p -- аналог show
    io:format("First: ~p~n", [get_n(Pid)]),
    io:format("Second: ~p~n", [get_n(Pid)]),
    stop(Pid).
