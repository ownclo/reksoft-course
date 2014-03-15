-module(task).

% gen_server has low overhead, they said. You can always
% switch to a hand-rolled server, they said.

% NOTE: gen_fsm behaviour was considered inappropriate
% because state transitions are simple and linear.
-behaviour(gen_server).

%% API
-export([start/2
        ,start/1
        ,start_wait/2
        ,stop/1
        ,get_status/1
        ,subscribe/1
        ,unsubscribe/1
        ,cancel/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type task_id() :: pid().
-type task_closure() :: fun(() -> any()).
-type attempts_allowed() :: pos_integer() | forever.
-type task_status() :: {working, non_neg_integer()}
                     | {succeeded, any()}
                     | fail_limit_reached.

%% XXX: How can I guarantee the immutability of
%% 'task_closure' and 'attempts_allowed'? NOWAY :(
%% ReaderT TaskConfig (State TaskStatus)
-record(state, {
        status :: task_status(),
        worker :: pid(),
        subscribers = sets:new() :: sets:set(pid()),

        % IMMUTABLE
        attempts_allowed :: attempts_allowed(),
        task_closure :: task_closure()
        }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Schedules a task to perform. Takes a task closure (0 arity) and
%% a number of attempts to restart a task in case of failure.
%% Returns an Id of a task that is used for further interoperation.
%% Single attempt is the default.
%% @end
%%--------------------------------------------------------------------
-spec start(Task     :: task_closure(),
            Attempts :: attempts_allowed())
    -> {ok, task_id()} | {error, Reason :: any()}.
start(Task, Attempts) ->
    gen_server:start(?MODULE, [Task, Attempts], []).

start(Task) -> start(Task, 1).

% start and immediately subscribe to all messages
% in one atomic step.
start_subscribe(Task, Attempts) ->
    gen_server:start(?MODULE, [Task, Attempts, {subscribe, self()}], []).

%%--------------------------------------------------------------------
%% @doc
%% Start a task and wait for it to complete (or overflow the number
%% of attempts).
%% @end
%%--------------------------------------------------------------------
start_wait(Task, Attempts) ->
    {ok, TID} = start_subscribe(Task, Attempts),
    loop_wait(TID).

cancel(TaskId) -> stop(TaskId).

% will force the worker process to abort
stop(TaskId) ->
    gen_server:cast(TaskId, stop).

-spec get_status(task_id()) -> task_status().
get_status(TaskId) ->
    gen_server:call(TaskId, get_status).

% XXX: Do need 'subscribe' be syncronous?
% Probably yes, we want to guarantee that all
% messages are delivered since subscription started.
subscribe(TaskId) ->
    gen_server:call(TaskId, {subscribe, self()}).

unsubscribe(TaskId) ->
    gen_server:cast(TaskId, {unsubscribe, self()}).

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

    {ok, State};

init([Task, Attempts, {subscribe, Pid}]) ->
    {ok, State} = init([Task, Attempts]),
    Subs = sets:add_element(Pid, State#state.subscribers),
    {ok, State#state{subscribers = Subs}}.

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

    % NOTE: Stop ourselves if finishing is reported?
    % That is considered a poorer design decision.
    % While easily implementable, that behaviour will
    % complicate interoperation with multiple listeners.
    % Idling when done (while explicitly canceled) need to
    % be documented at the top level, but that decision was
    % taken consciously.

    % Status = State#state.status,
    % StopResponse = {stop, normal, Status, State},
    % WorkResponse = {reply, Status, State},
    % case Status of
    %     {working, _} -> WorkResponse;
    %     _ -> StopResponse
    % end;

    {reply, State#state.status, State};

handle_call({subscribe, Pid}, _From, State) ->
    Subs = sets:add_element(Pid, State#state.subscribers),
    {reply, ok, State#state{subscribers = Subs}};

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
    NewState = State#state{status={succeeded, Result}},
    notify_subscribers(NewState),
    {noreply, NewState};

handle_cast({unsubscribe, Pid}, State) ->
    NewSubs = sets:del_element(Pid, State#state.subscribers),
    {noreply, State#state{subscribers = NewSubs}};


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
    notify_subscribers(NewState),
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

notify_subscribers(State) ->
    do_notify_subscribers(State#state.subscribers,
                          {self(), State#state.status}).

do_notify_subscribers(Subs, {TaskId, Status}) ->
    Notify = fun(Sub) -> notify(Sub, {TaskId, Status}) end,
    lists:map(Notify, sets:to_list(Subs)),
    ok.

notify(Sub, Status) ->
    Sub ! Status.

loop_wait(TID) ->
    receive
        {TID, {working, _}} -> loop_wait(TID);
        {TID, fail_limit_reached} -> fail_limit_reached;
        {TID, {succeeded, Res}} -> {succeeded, Res}
    end.
