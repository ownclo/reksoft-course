-module(deadweb).

-export([start/0
        ,stop/0
        ,loop/1]).

start() ->
    deadjournal:start(),
    mochiweb_http:start([{loop, fun loop/1},
                         {port, 8080}]),
    ok.

stop() ->
    deadjournal:stop(),
    mochiweb_http:stop(),
    ok.

loop(Req) ->
    Path = Req:get(path),
    case Req:get(method) of
        'GET'  -> deadweb_get(Path, Req);
        'POST' -> deadweb_post(Path, Req)
    end.

deadweb_get("/", Req) ->
    Posts = deadjournal:get_posts(),
    dump_plain(Posts, Req);

deadweb_get("/" ++ Name, Req) ->
    PostName = list_to_atom(Name),
    Post = deadjournal:get_post(PostName),
    case Post of
        none -> Req:not_found();
        Post -> dump_plain(Post, Req)
    end.

deadweb_post("/" ++ Name, _Req) ->
    _PostName = list_to_atom(Name),
    _Content = 'TODO',
    ok.


dump_plain(Response, Req) ->
    Content = io_lib:format("~p~n", Response),
    Req:respond({200, [{"Content-Type", "text/plain"}],
                 Content}).
