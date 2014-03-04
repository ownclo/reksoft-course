-module(main).
-compile([export_all]).
-mode(compile).

main([]) -> my_func().

other_process() ->
    receive
        {get, Pid} -> Pid ! "this"
    end.

my_func() ->
    Pid = spawn(fun other_process/0),
    Pid ! {get, self()},
    receive
        Message ->
            io:format("Got a message: ~s.~n", [Message])
    end.
