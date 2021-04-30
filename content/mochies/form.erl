-module(form).
-export([form/1]).

form(Req) ->
    case mochiweb_request:get(method, Req) of
        'GET' ->
            mochiweb_request:serve_file("form.html", ".", Req);
        'POST' ->
            Fields = mochiweb_multipart:parse_form(Req),
            Text = list:flatten(io_lib:format("~p", [Fields])),
            Encoded = mochiweb_html:escape(Text),
            Reply = ["<pre>", Encoded, "</pre>"],
            mochiweb_request:respond({200, [], Reply}, Req)
    end.
