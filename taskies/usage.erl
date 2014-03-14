-module(usage).
-export([main/1]).
-mode(compile).

main(_) ->
    OkFunction = fun() -> io:format("Hi there!~n"), ok end,

    {ok, Pid} = taskmaster:start(),
    TaskId = taskmaster:schedule(Pid, OkFunction),
    note("TaskId: ", TaskId),

    Status = taskmaster:get_status(TaskId),
    note("TaskStatus: ", Status),

    taskmaster:stop(Pid),
    ok.

note(Note, Val) ->
    io:format("~s~p~n", [Note, Val]).
