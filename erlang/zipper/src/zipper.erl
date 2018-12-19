-module(zipper).

-export([new_tree/3,
         from_tree/1,
         to_tree/1,
         up/1,
         left/1,
         right/1,
         value/1,
         set_value/2,
         set_left/2,
         set_right/2,
         test_version/0]).

% The test suite heavily uses nil for both empty and error values.
% I'd consider it more idiomatic to use undefined for empty values,
% and wrap possibly erroring functions in the standard ok/error
% return tuples.

-record(branch, {value :: term(),
                 left = nil :: branch(),
                 right = nil :: branch()}).

-record(path, {path :: path(),
               direction :: left | right,
               value :: term(),
               branch :: branch()}).

-record(location, {branch :: branch(),
                   path = top :: path()}).

-type tree() :: #branch{}.
-type branch() :: #branch{} | nil.
-type path() :: #path{} | top.
-type location() :: #location{}.

%% API

-spec new_tree(term(), branch(), branch()) -> tree().
new_tree(Value, Left, Right) when
      is_record(Left, branch) orelse Left == nil,
      is_record(Right, branch) orelse Right == nil ->
    #branch{value=Value, left=Left, right=Right}.

-spec to_tree(location()) -> tree().
to_tree(#location{branch=Branch, path=top}) ->
    Branch;
to_tree(#location{} = Location) ->
    % This is my favorite bit.
    to_tree(up(Location)).

-spec from_tree(tree()) -> location().
from_tree(#branch{} = Tree) ->
    #location{branch = Tree}.

-spec value(location()) -> term().
value(#location{branch=#branch{value=Value}}) ->
    Value.

-spec set_value(location(), term()) -> location().
set_value(#location{branch=Branch} = Location, Value) ->
    Location#location{branch=Branch#branch{value=Value}}.

-spec set_left(location(), branch()) -> location().
set_left(#location{branch=Branch} = Location, Value) when
      is_record(Value, branch) orelse Value == nil ->
    Location#location{branch=Branch#branch{left = Value}}.

-spec set_right(location(), branch()) -> location().
set_right(#location{branch=Branch} = Location, Value) when
      is_record(Value, branch) orelse Value == nil ->
    Location#location{branch=Branch#branch{right = Value}}.

-spec left(location()) -> location() | nil.
left(#location{branch=#branch{left=nil}}) ->
    nil;
left(#location{branch=Branch, path=Path}) ->
    LeftPath = #path{path=Path,
                     direction=left,
                     value=Branch#branch.value,
                     branch=Branch#branch.right},
    #location{branch=Branch#branch.left, path=LeftPath}.

-spec right(location()) -> location() | nil.
right(#location{branch=#branch{right=nil}}) ->
    nil;
right(#location{branch=Branch, path=Path}) ->
    RightPath = #path{path=Path,
                      direction=right,
                      value=Branch#branch.value,
                      branch=Branch#branch.left},
    #location{branch=Branch#branch.right, path=RightPath}.

-spec up(location()) -> location() | nil.
up(#location{path=top}) ->
    nil;
up(#location{branch=Branch, path=Path}) ->
    UpBranch = case Path#path.direction of
        left ->
            #branch{value=Path#path.value, left=Branch, right=Path#path.branch};
        right ->
            #branch{value=Path#path.value, left=Path#path.branch, right=Branch}
    end,
    #location{branch=UpBranch, path=Path#path.path}.

-spec test_version() -> integer().
test_version() ->
    1.
