-module(db).
-export([start/2, stop/1]).

start(normal, go) ->
    io:format("db start~n"),
    {ok, Pid} = db_server:start(),
    {ok, Pid}.

% note that there is no db_server:stop().
stop([]) -> io:format("db stop~n").
