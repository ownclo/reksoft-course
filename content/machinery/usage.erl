-module(usage).
-export([main/1]).
-mode(compile).

main(_) ->
    {ok, Id} = roomy:start(),

    note("Id: ", Id),
    note("Status: ", roomy:status(Id)),

    note("Leave on empty: ", roomy:leave(Id, 50)),
    note("Status: ", roomy:status(Id)),

    note("Reservation: ", roomy:reserve(Id, 5)),
    note("Status: ", roomy:status(Id)),

    note("One more reservation attempt: ", roomy:reserve(Id, 42)),
    note("Status: ", roomy:status(Id)),

    note("One left: ", roomy:leave(Id, 1)),
    note("Status: ", roomy:status(Id)),

    spawn(fun() ->
        io:format("Another process tries to reserve_wait...~n"),
        roomy:reserve_wait(Id, 42),
        note("Status in another process: ", roomy:status(Id))
    end),

    timer:sleep(50), % want to see the wait queue populated.
    note("Status before all left: ", roomy:status(Id)),
    note("All left: ", roomy:leave(Id, 4)),
    % if that status is 'free', then we are left before another
    % process tried to receive_wait. That happens if debug message
    % "Status before all left" and 'sleep' are commented out.
    note("Status after all left: ", roomy:status(Id)),

    roomy:stop(Id).

note(Note, Val) ->
    io:format("~s~p~n", [Note, Val]).
