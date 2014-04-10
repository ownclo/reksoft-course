-module(simple).
-export([init/1, handle_call/3]).

init(_) -> {ok, 0}.

handle_call(val, _From, State) ->
    {reply, State, State+1}.
