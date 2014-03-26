-module(server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
        ,start_link/1
        ,new_child/3
        ,shutdown/1
        ,crash/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Policy) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Policy).

new_child(Server, simple_one_for_one, _Name) ->
    supervisor:start_child(Server, []);  % array of additional parameters

new_child(Server, _Policy, Name) ->
    supervisor:start_child(Server, dummy_spec(Name)).

shutdown(Worker) ->
    dummy_worker:shutdown(Worker).

crash(Worker) ->
    exit(Worker, kill).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, {{one_for_one, 5, 10},
          [dummy_spec(dummy_worker)]
         }
    };

init(Policy) ->
    {ok, {{Policy, 5, 10},
          [dummy_spec(dummy_worker)
          ,dummy_spec(blah)
          ,dummy_spec(bob)]
         }
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
dummy_spec(Name) -> ?CHILD(Name, dummy_worker, worker, []).
