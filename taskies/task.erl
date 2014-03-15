-module(task).

% gen_server has low overhead, they said. You can always
% switch to a hand-rolled server, they said.

% NOTE: gen_fsm behaviour was considered inappropriate
% because state transitions are simple and linear.
-behaviour(gen_server).

%% API
-export([start/2
        ,stop/1
        ,get_status/1
        ,cancel/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% XXX: How can I guarantee the immutability of
%% 'task_closure' and 'attempts_allowed'? NOWAY :(
%% ReaderT TaskConfig State TaskStatus
-record(state, {
        status            :: task_types:task_status(),
        worker            :: pid(),

        % IMMUTABLE
        attempts_allowed  :: task_types:attempts_allowed(),
        task_closure      :: task_types:task_closure()
        }).

%%%===================================================================
%%% API
%%%===================================================================

start(Task, Attempts) ->
    gen_server:start(?MODULE, [Task, Attempts], []).

% will force the worker process to abort
stop(Task) ->
    gen_server:cast(Task, stop).

cancel(Task) -> stop(Task).

-spec get_status(task_types:task_id()) -> task_types:task_status().
get_status(Task) ->
    gen_server:call(Task, get_status).

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
init([Task, Attempts]) ->
    process_flag(trap_exit, true),
    Pid = spawn_task(Task),

    State = #state{status={working, 0},
                   worker=Pid, 
                   task_closure=Task,
                   attempts_allowed=Attempts},

    {ok, State}.

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
handle_call(get_status, _From, State) ->
    % TODO: Stop ourselves if finishing is reported?
    {reply, State#state.status, State};

handle_call(_Request, _From, State) ->
    {reply, unsupported, State}.

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

handle_cast({result, Result}, State) ->
    Status = {succeeded, Result},
    {noreply, State#state{status=Status}};

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

handle_info({'EXIT', _Pid, normal}, State) ->
    % nothing to do, Result is already sent to a mailbox
    {noreply, State};

handle_info({'EXIT', OldWorker, _Abnormal}, State) ->
    % If allowed, increment the number of attempts and retry.
    % NOTE: Can implement Reason reporting as a feature.
    % XXX: 'status' must be set to 'working', there is a bug otherwise.
    % XXX: Explicitly matching OldWorker Pid with whatever is stored
    %      in the state.
    #state{status = {working, OldMade},
           worker = OldWorker,
           attempts_allowed = Allowed,
           task_closure = Task
           } = State,
    Made = OldMade + 1,

    NewState = if
        Allowed =:= forever orelse Made < Allowed ->
            Worker = spawn_task(Task),
            Status = {working, Made},
            State#state{worker = Worker, status = Status};
        true ->  % Made >= Allowed
            State#state{status = fail_limit_reached}
    end,
    {noreply, NewState};

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
spawn_task(Task) ->
      spawn_link(make_reporting(self(), Task)).

make_reporting(Pid, Task) ->
    fun() ->
        Result = Task(),
        report_result(Pid, Result)
    end.

report_result(Pid, Result) ->
    gen_server:cast(Pid, {result, Result}).
