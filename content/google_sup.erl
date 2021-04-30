-module(google_sup).
-behaviour(supervisor).
-export([init/1]).

init(go) ->
    {ok,
     {{one_for_one, 5, 60},  % strategy
      [{web_server,
        {web_server_main, start, [{port, 80}]},
        permanent,  % | temporary | transient
        % timeout | infinity | brutal_kill
        5000,  % 5 secs wait to respond
        worker,  % | supervisor
        [web_server_main] % list of modules used | dynamic
       }
       % ...
      ]
     }
    }.
