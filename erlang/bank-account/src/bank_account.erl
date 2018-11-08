%%%-------------------------------------------------------------------
%%% @doc
%%% Exercism.io exercise for creating a bank account server.
%%% @end
%%%-------------------------------------------------------------------
-module(bank_account).

-behaviour(gen_server).

%% API
-export([start_link/0,
         balance/1,
         charge/2,
         close/1,
         create/0,
         deposit/2,
         withdraw/2,
         test_version/0]).

-ignore_xref([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, 1000).

-record(state, {balance = 0}).

%%%===================================================================
%%% API
%%%===================================================================

-spec balance(pid()) -> non_neg_integer() | {error, atom()}.
balance(Pid) ->
    call(Pid, {balance}).

-spec charge(pid(), integer()) -> non_neg_integer() | {error, atom()}.
charge(Pid, Amount) ->
    call(Pid, {charge, Amount}).

-spec close(pid()) -> integer().
close(Pid) ->
    case balance(Pid) of
        {error, _} = Error ->
            Error;
        Balance ->
            ok = gen_server:stop(Pid, normal, ?TIMEOUT),
            Balance
    end.

-spec create() -> pid().
create() ->
    % Calling start_link/0 directly instead of via a supervisor is
    % pretty weird, but exercism doesn't really provide the machinery
    % for uploading multiple modules so as to setup a proper
    % supervision tree.
    {ok, Pid} = start_link(),
    Pid.

-spec deposit(pid(), integer()) -> non_neg_integer() | {error, atom()}.
deposit(Pid, Amount) ->
    call(Pid, {deposit, Amount}).

-spec withdraw(pid(), integer()) -> non_neg_integer() | {error, atom()}.
withdraw(Pid, Amount) ->
    call(Pid, {withdraw, Amount}).

-spec test_version() -> integer().
test_version() ->
    1.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({balance}, _From, State) ->
    reply(do_balance(State));
handle_call({deposit, Amount}, _From, State) ->
    reply(do_deposit(State, Amount));
handle_call({withdraw, Amount}, _From, State) ->
    reply(do_withdraw(State, Amount));
handle_call({charge, Amount}, _From, State) ->
    reply(do_charge(State, Amount)).

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

call(Pid, Msg) ->
    try
        gen_server:call(Pid, Msg, ?TIMEOUT)
    catch
        exit:{noproc, _} ->
            {error, account_closed}
    end.

reply({ok, State, Amount}) ->
    {reply, Amount, State}.

do_balance(State) ->
    {ok, State, State#state.balance}.

do_deposit(State, Amount) when Amount < 0 ->
    {ok, State, 0};
do_deposit(State, Amount) ->
    {ok, State#state{balance = State#state.balance + Amount}, Amount}.

do_withdraw(State, Amount) when Amount < 0 ->
    {ok, State, 0};
do_withdraw(State, Amount) when Amount > State#state.balance ->
    {ok, #state{}, State#state.balance};
do_withdraw(State, Amount) ->
    {ok, State#state{balance = State#state.balance - Amount}, Amount}.

do_charge(State, Amount) when Amount < 0 ->
    {ok, State, 0};
do_charge(State, Amount) when Amount > State#state.balance ->
    {ok, State, 0};
do_charge(State, Amount) ->
    {ok, State#state{balance = State#state.balance - Amount}, Amount}.
