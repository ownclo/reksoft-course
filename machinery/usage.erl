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

    note("All left: ", roomy:leave(Id, 4)),
    note("Status: ", roomy:status(Id)).

note(Note, Val) ->
    io:format("~s~p~n", [Note, Val]).
