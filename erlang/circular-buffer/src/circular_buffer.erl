-module(circular_buffer).

-export([create/1,
         read/1,
         size/1,
         write/2,
         write_attempt/2,
         test_version/0]).

-record(buffer, {queue :: queue:queue(),
                 size :: pos_integer(),
                 length=0 :: non_neg_integer()}).
-type buffer() :: #buffer{}.

%% API

-spec create(pos_integer()) -> pid().
create(Size) when Size >= 1 ->
    % As I don't belive the process handling is an important part of
    % this exercise, I've implemented with a simple and fragile
    % process recieve loop. Use gen_server in production, folks.
    erlang:spawn_link(fun () -> start(Size) end).

-spec size(pid()) -> {ok, pos_integer()}.
size(Pid) ->
    call(Pid, size, {}).

-spec read(pid()) -> {ok, term()} | {error, empty}.
read(Pid) ->
    call(Pid, read, {}).

-spec write(pid(), term()) -> ok.
write(Pid, Item) ->
    call(Pid, write, {Item}).

-spec write_attempt(pid(), term()) -> ok | {error, full}.
write_attempt(Pid, Item) ->
    call(Pid, write_attempt, {Item}).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal.

start(Size) ->
    % A double ended queue, as used here, is a reasonable facsimile of
    % a functional circular buffer implementation. While it doesn't
    % guarantee the constant memory usage that a mutable array would,
    % it does provide constant time reads and writes.
    State = #buffer{queue=queue:new(), size=Size},
    loop(State).

loop(State) ->
    loop(receive
        {call, Pid, size, {}} ->
            reply(Pid, do_size(State)),
            State;
        {call, Pid, read, {}} ->
            {UpdatedState, Reply} = do_read(State),
            reply(Pid, Reply),
            UpdatedState;
        {call, Pid, write, {Value}} ->
            {UpdatedState, Reply} = do_write(State, Value),
            reply(Pid, Reply),
            UpdatedState;
        {call, Pid, write_attempt, {Value}} ->
            {UpdatedState, Reply} = do_write_attempt(State, Value),
            reply(Pid, Reply),
            UpdatedState;
        {call, Pid, _Function, _Args} ->
            reply(Pid, {error, badmatch}),
            State
    end).

call(Pid, Function, Args) ->
    Pid ! {call, self(), Function, Args},
    receive
        {reply, Value} ->
            % Helpful debugging:
            % ok = io:format("Call to ~w(~w) got ~w~n", [Function, Args, Value]),
            Value
    end.

reply(Pid, Value) ->
    Pid ! {reply, Value}.

-spec do_size(buffer()) -> {ok, pos_integer()}.
do_size(#buffer{size = Size}) ->
    {ok, Size}.

-spec do_read(buffer()) -> {buffer(), {ok, term()} | {error, empty}}.
do_read(#buffer{length=0} = State) ->
    {State, {error, empty}};
do_read(#buffer{queue=Queue, length=Length} = State) ->
    {{value, Value}, UpdatedQueue} = queue:out(Queue),
    UpdatedState = State#buffer{queue=UpdatedQueue, length=Length - 1},
    {UpdatedState, {ok, Value}}.

-spec do_write(buffer(), term()) -> {buffer(), ok}.
do_write(#buffer{queue=Queue, size=Size, length=Length} = State, Value) when Length >= Size ->
    % This is the bit that enforces the invariant for the initial size
    % of the buffer.
    UpdatedQueue = queue:drop(Queue),
    UpdatedState = State#buffer{queue=UpdatedQueue, length=Length - 1},
    do_write(UpdatedState, Value);
do_write(#buffer{queue=Queue, length=Length} = State, Value) ->
    UpdatedQueue = queue:in(Value, Queue),
    UpdatedState = State#buffer{queue=UpdatedQueue, length=Length + 1},
    {UpdatedState, ok}.

-spec do_write_attempt(buffer(), term()) -> {buffer(), ok | {error, full}}.
do_write_attempt(#buffer{size=Size, length=Length} = State, _Value) when Length >= Size ->
    {State, {error, full}};
do_write_attempt(State, Value) ->
    do_write(State, Value).
