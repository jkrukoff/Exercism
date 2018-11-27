-module(robot_simulator).

-define(TABLE_NAME, robots).

-export([advance/1,
         control/2,
         create/0,
         direction/1,
         left/1,
         right/1,
         place/3,
         position/1,
         test_version/0]).

-type direction() :: north | east | south | west | undefined.
-type position() :: {integer() | undefined, integer() | undefined}.
-type instructions() :: [ $A | $L | $R ].

-record(robot,
        {table :: ets:tid(),
         ref :: reference()}).

-record(state,
        {ref = undefined :: reference() | undefined,
         direction = undefined :: direction(),
         position = {undefined, undefined} :: position()}).

%% API

-spec advance(#robot{}) -> ok.
advance(Robot) ->
    State = lookup(Robot),
    update(Robot, do_advance(State)).

-spec control(#robot{}, instructions()) -> ok.
control(Robot, String) ->
    InitialState = lookup(Robot),
    UpdatedState = lists:foldl(
        fun (Instruction, State) ->
            do_instruction(Instruction, State)
        end,
        InitialState,
        String),
    update(Robot, UpdatedState).

-spec create() -> #robot{}.
create() ->
    % Usually we'd do this in process init.
    Table = case ets:whereis(?TABLE_NAME) of
        undefined ->
            % We're storing a record, so let's use the ref field as
            % our key.
            ets:new(?TABLE_NAME, [private, {keypos, #state.ref}]);
        Existing ->
            Existing
    end,
    Ref = make_ref(),
    true = ets:insert_new(Table, #state{ref = Ref}),
    #robot{table = Table, ref = Ref}.

-spec direction(#robot{}) -> direction().
direction(Robot) ->
    #state{direction = Direction} = lookup(Robot),
    Direction.

-spec left(#robot{}) -> ok.
left(Robot) ->
    State = lookup(Robot),
    update(Robot, do_left(State)).

-spec right(#robot{}) -> ok.
right(Robot) ->
    State = lookup(Robot),
    update(Robot, do_right(State)).

-spec place(#robot{}, direction(), position()) -> ok.
place(Robot, Direction, Position) ->
    State = #state{direction = Direction, position = Position},
    update(Robot, State).

-spec position(#robot{}) -> position().
position(Robot) ->
    #state{position = Position} = lookup(Robot),
    Position.

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

lookup(#robot{table = Table, ref = Ref}) ->
    [State] = ets:lookup(Table, Ref),
    State.

update(#robot{table = Table, ref = Ref}, State) ->
    true = ets:insert(Table, State#state{ref = Ref}),
    ok.

do_advance(#state{direction = north, position = {X, Y}} = State) ->
    State#state{position = {X, Y + 1}};
do_advance(#state{direction = east, position = {X, Y}} = State) ->
    State#state{position = {X + 1, Y}};
do_advance(#state{direction = south, position = {X, Y}} = State) ->
    State#state{position = {X, Y - 1}};
do_advance(#state{direction = west, position = {X, Y}} = State) ->
    State#state{position = {X - 1, Y}}.

do_left(#state{direction = north} = State) ->
    State#state{direction = west};
do_left(#state{direction = west} = State) ->
    State#state{direction = south};
do_left(#state{direction = south} = State) ->
    State#state{direction = east};
do_left(#state{direction = east} = State) ->
    State#state{direction = north}.

do_right(#state{direction = north} = State) ->
    State#state{direction = east};
do_right(#state{direction = east} = State) ->
    State#state{direction = south};
do_right(#state{direction = south} = State) ->
    State#state{direction = west};
do_right(#state{direction = west} = State) ->
    State#state{direction = north}.

do_instruction($A, State) ->
    do_advance(State);
do_instruction($L, State) ->
    do_left(State);
do_instruction($R, State) ->
    do_right(State);
do_instruction(_, State) ->
    State.
