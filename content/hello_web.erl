-module(hello_web).
-export([say/3]).

say(SessionId, _Env, _Input) ->
    mod_esi:deliver(SessionId,
        "Content-Type: text/html\r\n\r\n"),
    mod_esi:deliver(SessionId, "<html><body>Hello, "),
    mod_esi:deliver(SessionId, "World!</body></html>\r\n").
