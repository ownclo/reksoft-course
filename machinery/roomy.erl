-module(roomy).

-behaviour(gen_fsm).

%% API
-export([start_link/0
        ,start/0
        ,stop/1
        ,status/1
        ,reserve/2
        ,reserve_wait/2
        ,leave/2]).

%% gen_fsm callbacks
-export([init/1,
         free/2,
         free/3,
         reserved/2,
         reserved/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% ROOMY STATES:
%%     - free. State = []
%%     - reserved. State = {N, Waiting :: queue:queue({pid(), N})},
%%       N - number of slots reserved; Waiting - queue of processees
%%       waiting for room to be empty. In reserved state, if
%%       'reserve_wait' message arrives, Pid of callee is added to
%%       this queue. When it's time to switch to 'free' state,
%%       in case of non-empty waiters' queue, topmost waiter is
%%       popped from the queue, the process is notified, and the
%%       room gets reserved by a process.

% Key design choice was whether one should store waiters' messages
% in the mailbox or support additional internal queue instead.
% The latter option was choosen, because in that case the presense
% of waiters' queue is explicit and the performance of
% waiters-related operations is independent of mailbox size (if
% one keeps messages in a mailbox, one need to scan it every time
% a transition from 'free' to 'reserved' is about to be made. If
% no one sends 'reserve_wait' message at all, the mailbox is fully
% scanned on every such transition, which is probably a bad idea).

% Note that 'reserve_wait' cannot be a gen_fsm sync RPC call since the
% server need to process other messages while the waiting client is
% blocked. So we send a message to that process upon completion.

% Given a room is 'reserved', when further 'reserve' is attempted,
% then 'error' is returned. Use 'reserve_wait' in order to wait for
% the room to be freed.

% A new queue is allocated upon each transition from 'free' to
% 'reserved'. Chances are that this will never become a bottleneck,
% but in that case one just could hold the same mutable state
% reference

-type(waiter_elem() :: {pid(), non_neg_integer()}).
-record(reserved_state, {
        num_reserved :: non_neg_integer(),
        wait_queue = queue:new() :: queue:queue(waiter_elem())
       }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_fsm:start(?MODULE, [], []).

stop(Id) ->
    gen_fsm:send_all_state_event(Id, stop).

status(Id) ->
    gen_fsm:sync_send_all_state_event(Id, status).

reserve(Id, N) ->
    gen_fsm:sync_send_event(Id, {reserve, N}).

reserve_wait(Id, N) ->
    gen_fsm:send_event(Id, {reserve_wait, N, self()}),
    receive
        % XXX: Id is already bound, checking for message from
        % particular room.
        {reserved, Id} -> ok
    end.

leave(Id, M) ->
    gen_fsm:send_event(Id, {free, M}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, free, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
free({free, _M}, State) ->
    {next_state, free, State};

free({reserve_wait, N, Pid}, _State) ->
    notify_reserved(Pid),
    {next_state, reserved, #reserved_state{num_reserved = N}}.


reserved({free, M}, #reserved_state{num_reserved = N} = S)
        when M >= N ->

    Queue = S#reserved_state.wait_queue,

    case queue:out(Queue) of
        {empty, _Queue} -> {next_state, free, []};
        {{value, {Pid, WaitN}}, RestQ} ->
            notify_reserved(Pid),
            NewState = S#reserved_state{
                    num_reserved = WaitN,
                    wait_queue = RestQ},
            {next_state, reserved, NewState}
    end;

% M < N (see above).
reserved({free, M}, #reserved_state{num_reserved = N} = S) ->
    {next_state, reserved,
     S#reserved_state{num_reserved = N - M}};

reserved({reserve_wait, N, Pid}, State) ->
    {next_state, reserved, push_waiter({Pid, N}, State)}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
free({reserve, N}, _From, _State) ->
    % TODO: Force N to be a positive number. Or just fail fast?
    Reply = ok,
    {reply, Reply, reserved, #reserved_state{num_reserved = N}};

free(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, free, State}.

reserved({reserve, _N}, _From, State) ->
    Reply = error,
    {reply, Reply, reserved, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

% Stopping even if state is 'reserved'.
% No context, so that's probably ok.
handle_event(stop, _StateName, State) ->
    {stop, normal, State};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(status, _From, StateName, State) ->
    Reply = {StateName, State},
    {reply, Reply, StateName, State};

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

notify_reserved(Pid) ->
    Pid ! {reserved, self()}.

% I want to have a lens library here :(
% Nested updates are such a pain in Erlnag.
push_waiter(Waiter, #reserved_state{wait_queue=Q}=S) ->
    S#reserved_state{wait_queue=queue:in(Waiter, Q)}.
