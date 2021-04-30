-module(summation).
-export([sum/1]).

%% compile: erlc summation.erl
%% compile:file("hello_world.erl"),

sum(List) ->
    do_summ(0, List).

do_summ(N, []) -> N;
do_summ(N, [X | XS]) ->
    do_summ(N+X, XS).
