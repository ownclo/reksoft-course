-module(taskmaster).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start/0,
         stop/1,
         schedule/3,
         schedule/2,
         get_status/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% NOTE: Task manager master is stateless, which is
%% a major factor in favor of architecture choosen.
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server and links to it.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start(?MODULE, [], []).

stop(Server) ->
    gen_server:cast(Server, stop).

%%--------------------------------------------------------------------
%% @doc
%% Schedules a task to perform. Takes a task closure (0 arity) and
%% a number of attempts to restart a task in case of failure.
%% Returns an Id of a task that is used for further interoperation.
%% Single attempt is the default.
%% @end
%%--------------------------------------------------------------------
-spec schedule(Server   :: pid(),
               Task     :: task_types:task_closure(),
               Attempts :: task_types:attempts_allowed())
      -> task_types:task_id().
schedule(Server, Task, Attempts) ->
    gen_server:call(Server, {schedule, Task, Attempts}).

schedule(Server, Task) -> schedule(Server, Task, 1).

%%--------------------------------------------------------------------
%% @doc
%% Get the status of a task. This is a syncronous call.
%% @end
%%--------------------------------------------------------------------
% XXX: Beware! taskmaster module just forwards get_status to
% task module. We are talking to Task, not to Master.
-spec get_status(task_types:task_id()) -> task_types:task_status().
get_status(TaskId) ->
    task:get_status(TaskId).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({schedule, Task, Attempts}, _From, State) ->
    % what is the pattern for that? Shall I check?
    % Shall I fail early?
    {ok, TaskId} = task:start(Task, Attempts),
    {reply, TaskId, State};

handle_call(_Request, _From, State) ->
    Reply = unsupported,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
