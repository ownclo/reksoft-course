-module(usage).
-export([main/1]).
-mode(compile).

main(_) ->
    Policy = simple_one_for_one,
    {ok, Sup} = server_sup:start_link(Policy),
    note("Sup PID: ", Sup),

    note("Child: ", server_sup:new_child_simple(Sup)),

    flush(),
    ok.

flush() ->
    receive
        M -> note("Mailbox: ", M),
            flush()
    after 0 -> ok
    end.

note(Note, Val) ->
    io:format("~s~p~n", [Note, Val]).
