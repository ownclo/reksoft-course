-module(usage).
-export([main/1]).
-mode(compile).

maybe_ok() ->
    random:seed(erlang:now()),
    Rnd = random:uniform(),
    io:format("Number = ~p~n", [Rnd]),
    if Rnd >= 0.5 ->
            io:format("Dying...~n"),
            exit(dead);
        true -> io:format("Success...~n"), ok
    end.

main(_) ->
    _OkFunction = fun() -> io:format("Hi there!~n"), ok end,
    ExitFunction = fun() -> io:format("Dying...~n"), exit(dead) end,

    {ok, TaskId} = task:start(fun maybe_ok/0, 10),
    note("Subscription: ", task:subscribe(TaskId)),
    % note("Unsubscription: ", task:unsubscribe(TaskId)),

    note("TaskStatus: ", task:get_status(TaskId)),

    timer:sleep(5),
    note("TaskStatus: ", task:get_status(TaskId)),

    task:cancel(TaskId),
    timer:sleep(10),  % for cancel message to arrive.
    flush(),

    io:format("~n~n~nSync Call...~n"),
    note("Waiting call: ", task:start_wait(ExitFunction, 10)),
    ok.

flush() ->
    receive
        M -> note("Mailbox: ", M),
            flush()
    after 0 -> ok
    end.

note(Note, Val) ->
    io:format("~s~p~n", [Note, Val]).
