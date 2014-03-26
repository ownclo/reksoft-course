-module(usage).
-export([main/1]).
-mode(compile).

main(_) ->
    register(main, self()),
    % XXX: For simple_one_for_one strategy,
    % no processes will be started at start of
    % supervisor.
    Policy = one_for_all,
    {ok, Sup} = server_sup:start_link(Policy),
    note("Sup PID: ", Sup),

    {ok, Fst} = server_sup:new_child(Sup, Policy, foo),
    % {ok, _Snd} = server_sup:new_child(Sup, Policy, bar),

    space(),
    note("Mailbox contains:"),
    flush(),

    % space(),
    % note("Crashing dynamic child: ", server_sup:crash(Fst)),

    % space(),
    % note("Mailbox contains:"),
    % flush(),

    space(),
    note("Crashing static child: ", supervisor:terminate_child(Sup, bob)),

    space(),
    note("Mailbox contains:"),
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

note(String) ->
    io:format("~s~n", [String]).

space() ->
    io:format("~n").
