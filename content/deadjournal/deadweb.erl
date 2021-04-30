-module(deadweb).

-export([start/0
        ,stop/0
        ,req_handler/1]).

start() ->
    deadjournal:start(),
    mochiweb_http:start([{loop, fun req_handler/1},
                         {port, 8080}]),
    ok.

stop() ->
    mochiweb_http:stop(),
    deadjournal:stop(),
    ok.

req_handler(Req) ->
    Path = Req:get(path),
    case Req:get(method) of
        'GET'    -> handle_get    (Path, Req);
        'POST'   -> handle_post   (Path, Req);
        'DELETE' -> handle_delete (Path, Req)
    end.


handle_get("/", Req) ->
    Posts = deadjournal:get_posts(),
    dump_plain(Posts, Req);

% NOTE: RESTfully, comments would be accessible
% by GET /:PostId/comments.
% In the abscence of path dispatcher in mochiweb,
% dealing with regexps of some sort is needed. So
% prefix scheme is used instead.
handle_get("/comments/" ++ Name, Req) ->
    PostName = list_to_atom(Name),
    Comments = deadjournal:get_comments(PostName),
    dump_plain(Comments, Req);

handle_get("/" ++ Name, Req) ->
    PostName = list_to_atom(Name),
    Post = deadjournal:get_post(PostName),

    case Post of
        none -> Req:not_found();
        Post -> dump_plain(Post, Req)
    end.


handle_post("/comments/" ++ Name, Req) ->
    PostName = list_to_atom(Name),
    Form = mochiweb_multipart:parse_form(Req),
    Comment = proplists:get_value("comment", Form),

    deadjournal:new_comment(PostName, Comment),
    dump_plain(ok, Req);

% XXX: If the URL is known in advance, PUT will
% be more suitable given its idempotency assumptions.
% But if the name will be auto-generated and/or the
% blog entry is submitted via a form, POST is
% unavoidable >:c
handle_post("/" ++ Name, Req) ->
    PostName = list_to_atom(Name),
    Form = mochiweb_multipart:parse_form(Req),
    Content = proplists:get_value("content", Form),

    deadjournal:new_post(PostName, Content),
    dump_plain(ok, Req).


% XXX: There may be some complications in usage of
% DELETE in a context of web forms. Anyway, idiomatic
% REST is preferred whenever possible.
handle_delete("/" ++ Name, Req) ->
    PostName = list_to_atom(Name),
    deadjournal:delete_post(PostName),
    dump_plain(ok, Req).


%% Internal functions
dump_plain(Response, Req) ->
    Content = io_lib:format("~p~n", [Response]),
    Req:respond({200, [{"Content-Type", "text/plain"}],
                 Content}).
