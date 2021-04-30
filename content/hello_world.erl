%% run with: escript hello_world.erl
-module(main).
%%-mode(compile).

%% main
main([]) -> hello("World");
main(["case"]) -> foo();
main(_) -> usage().

%% Say hello
-spec(hello/1 :: (String :: list()) -> ok).
hello(String) ->
    io:format("Hello, ~s!~n", [String]),
    ok.

-spec(usage/0 :: () -> any()).
usage() ->
    io:format("Usage: ./hello_world.erl~n").

foo() ->
    A = {foo, 1, "answer"},
    K = foo,
    B = case A of
        {bar, _, _} -> "first";
        {K, _} -> "second";
        {foo, 1, C} -> "fourth " ++ C;
        _ -> "error"
    end,
    io:format("~s~n", [B]),
    ok.
