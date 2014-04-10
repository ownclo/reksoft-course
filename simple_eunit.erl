-module(simple_eunit).
-include_lib("eunit/include/eunit.hrl").

first_test() -> ?assert(2 + 2 =:= 4).
second_test() -> ?assert(1 + 2 =:= 3).

% Note the trailing underscore
third_test_() ->
    [fun () -> ?assert(0 + 1 =/= 0) end,
     ?_test(?assert(1 + 1 =/= 1)),
     ?_assert(2 + 1 =/= 2)
     | [?_assert(N + 1 =/= N) || N <- [3,4,5,6]]
    ].
