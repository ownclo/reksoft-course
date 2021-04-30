-module(sample_links).
-export([test/0, subprocess/0]).

loop() ->
    io:format("A"),
    loop().

subprocess() ->
    timer:sleep(10),
    exit(boo).

% trap_exit(true)
test() ->
    spawn(fun() ->
        process_flag(trap_exit, true),
        Pid = spawn(?MODULE, subprocess, []),
        link(Pid),
        % we can link too late.
        % in that case, if no process_flag was set
        % AND the Pid is local, then 'link' will fail.
        % Otherwise, link() will send a noproc.
        %
        % if process just finished,
        % 'normal' will be the message.
        receive
            A -> io:format("~p~n", [A])
        end
        % loop()
    end).
