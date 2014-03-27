-module(usage).
-export([main/1]).
-mode(compile).

main(_) ->
    Int = 42,
    List = [41, 42, 43],
    Tuple = {41, 42, 43},

    _NestedList = [41, {42, 43}, 44],
    _NestedTuple = {41, [42, 43], 44},

    note("IntBinary: ", _IntTerm = biteys:from_term(Int)),
    note("ListBinary: ", _LstTerm = biteys:from_term(List)),
    note("TupleBinary: ", _TupleTerm = biteys:from_term(Tuple)),

    ok.

note(Note, Val) ->
    io:format("~s~p~n", [Note, Val]).
