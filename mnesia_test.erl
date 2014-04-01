-module(mnesia_test).
-export([test/0]).

-record(test, {
        key :: atom(),
        foo :: integer(),
        bar :: atom()
        }).

test() ->
    mnesia:start(),
    mnesia:create_table(test,
        [{attributes, record_info(fields, test)}]),

    mnesia:transaction(fun() ->
        mnesia:write(#test{key = aaa, foo = 1}) end),
    mnesia:transaction(fun() ->
        mnesia:select(test, [{#test{key = '$1', foo = '$2'},
            [{'>', '$2', 0}], ['$1']}])
        end).

% mnesia:foldl(
%   fun(Record, Acc) -> [Record | Acc] end,
%   [], test)
