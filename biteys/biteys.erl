-module(biteys).
-export([from_term/1
        ,to_term/1
        ]).

-define(INTFLAG, 0).
-define(TPLFLAG, 1).
-define(LSTFLAG, 2).

from_term(Int) when is_integer(Int) ->
    <<?INTFLAG, Int:32>>;

from_term(Tuple) when is_tuple(Tuple) ->
    Size = size(Tuple), % max 256
    Items = from_list_items(tuple_to_list(Tuple)),
    <<?TPLFLAG, Size, Items/binary>>;

from_term(Lst) when is_list(Lst) ->
    Size = length(Lst), % max (2 << 32) - 1
    Items = from_list_items(Lst),
    <<?LSTFLAG, Size:32, Items/binary>>.

to_term(_Binary) ->
    ok.


from_list_items([ X | Lst]) ->
    Curr = from_term(X),
    Rest = from_list_items(Lst),
    << Curr/binary, Rest/binary >>;

from_list_items([]) ->
    <<>>.
