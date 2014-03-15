-module(usage).
-export([main/1]).
-mode(compile).

main(_) ->
    _OkFunction = fun() -> io:format("Hi there!~n"), ok end,
    ExitFunction = fun() -> io:format("Dying...~n"), exit(dead) end,

    {ok, Pid} = taskmaster:start(),

    TaskId = taskmaster:schedule(Pid, ExitFunction, 10),
    note("TaskId: ", TaskId),

    Status = taskmaster:get_status(TaskId),
    note("TaskStatus: ", Status),

    taskmaster:stop(Pid),
    ok.

note(Note, Val) ->
    io:format("~s~p~n", [Note, Val]).
