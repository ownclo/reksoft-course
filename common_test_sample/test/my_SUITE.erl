-module(my_SUITE).
-compile(export_all).

all() -> [first_test].

init_per_suite(Config) ->
    {ok, P} = gen_server:start(simple, go, []),
    [{server, P} | Config].

end_per_suite(Config) -> Config.

first_test(Config) ->
    P = proplists:get_value(server, Config),
    V1 = gen_server:call(P, val),
    V2 = gen_server:call(P, val),
    true = V2 =:= V1.
