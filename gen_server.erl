-module(gen_server).
-compile([export_all]).

stop(Server) -> Server ! stop.

%% if 'init' function is not present in the module,
%% the SPAWNED process will die (lazy eval of fun).
start(Module) ->
    spawn(fun() -> server(Module, Module:init()) end).

call_sync(Server, Message) ->
    Server ! {sync, Message, self()},
    receive
        {ok, Reply} -> Reply
    after 1000 -> error
    end.

call_async(Server, Message) -> Server ! {async, Message}.

%% MODULES are First-Class Values.
server(Module, State) ->
    receive
        stop -> ok;
        {sync, Message, Pid} ->
            {Reply, NewState} = Module:handle_sync(State, Message),
            Pid ! {ok, Reply},
            server(Module, NewState);
        {async, Message} ->
            NewState = Module:handle_async(State, Message),
            server(Module, NewState)
    end.
