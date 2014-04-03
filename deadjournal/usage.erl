-module(usage).
-export([main/1, main_web/1]).
-mode(compile).

main_web(_) ->
    note("Starting... ", deadweb:start()),
    %% note("Stopping... ", deadweb:stop()),
    ok.

main(_) ->
    note("Starting... ", deadjournal:start()),

    note("Inserting post... ", deadjournal:new_post(cats, "I love cats")),
    note("Posts: ", deadjournal:get_posts()),
    note("Cat post: ", deadjournal:get_post(cats)),

    note("Inserting comment... ", deadjournal:new_comment(cats, "Me too!")),
    note("Cat comments: ", deadjournal:get_comments(cats)),

    note("Delete post... ", deadjournal:delete_post(cats)),
    note("Posts: ", deadjournal:get_posts()),

    flush(),
    ok.

flush() ->
    receive
        M -> note("Mailbox: ", M),
            flush()
    after 0 -> ok
    end.

note(Note, Val) ->
    io:format("~s~p~n", [Note, Val]).
