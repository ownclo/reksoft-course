-module(usage).
-export([main/1]).
-mode(compile).

main(_) ->
    % QuickCheck would be of great help here.
    Int = 42,
    List = [41, 42, 43],
    Tuple = {41, 42, 43},

    NestedList = [41, {42, 43}, 44],
    NestedTuple = {41, [42, 43], 44},

    note("IntBinary: ", IntTerm = biteys:from_term(Int)),
    note("IntTerm: ", {NewInt, <<>>} = biteys:to_term(IntTerm)),
    io:format("Int matches: ~p~n~n", [NewInt =:= Int]),

    note("ListBinary: ", LstTerm = biteys:from_term(List)),
    note("ListTerm: ", {NewLst, <<>>} = biteys:to_term(LstTerm)),
    io:format("List matches: ~p~n~n", [NewLst =:= List]),

    note("TupleBinary: ", TupleTerm = biteys:from_term(Tuple)),
    note("TupleTerm: ", {NewTuple, <<>>} = biteys:to_term(TupleTerm)),
    io:format("Tuple matches: ~p~n~n", [NewTuple =:= Tuple]),

    note("NestedList: ", NestLstTerm = biteys:from_term(NestedList)),
    note("NestedLstTerm: ", {NewNLT, <<>>} = biteys:to_term(NestLstTerm)),
    io:format("Nested list matches: ~p~n~n", [NewNLT =:= NestedList]),

    note("NestedTuple: ", NestTplTerm = biteys:from_term(NestedTuple)),
    note("NestedTupleTerm: ", {NewNTPL, <<>>} = biteys:to_term(NestTplTerm)),
    io:format("Nested tuple matches: ~p~n~n", [NewNTPL =:= NestedTuple]),

    ok.

note(Note, Val) ->
    io:format("~s~p~n", [Note, Val]).
