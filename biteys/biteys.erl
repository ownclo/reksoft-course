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


to_term(<<?INTFLAG, Int:32, Rest/binary>>) ->
    {Int, Rest};

to_term(<<?LSTFLAG, Size:32, Items/binary>>) ->
    to_list_items(Items, Size);

to_term(<<?TPLFLAG, Size, Items/binary>>) ->
    {Lst, Rest} = to_list_items(Items, Size),
    {list_to_tuple(Lst), Rest}.


%%% Internal Functions

% IMO fold will not simplify things here.
from_list_items([ X | Lst]) ->
    Curr = from_term(X),
    Rest = from_list_items(Lst),
    << Curr/binary, Rest/binary >>;

from_list_items([]) ->
    <<>>.


% this is unfolding a binary to a
% list of items.
to_list_items(Binary, 0) -> {[], Binary};

to_list_items(Binary, N) ->
    {Curr, Rest} = to_term(Binary),
    {Next, Left} = to_list_items(Rest, N - 1),
    {[Curr | Next], Left}.
