-module(usage).
-export([main/1]).
-mode(compile).

main(_) ->
    _OkFunction = fun() -> io:format("Hi there!~n"), ok end,
    ExitFunction = fun() -> io:format("Dying...~n"), exit(dead) end,

    {ok, TaskId} = task:start(ExitFunction, 10),
    note("TaskId: ", TaskId),

    Status = task:get_status(TaskId),
    note("TaskStatus: ", Status),

    timer:sleep(5),
    note("TaskStatus: ", task:get_status(TaskId)),
    task:cancel(TaskId),
    ok.

note(Note, Val) ->
    io:format("~s~p~n", [Note, Val]).
