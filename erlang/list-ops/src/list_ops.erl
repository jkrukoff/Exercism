-module(list_ops).

-export([append/2,
         concat/1,
         filter/2,
         length/1,
         map/2,
         foldl/3,
         foldr/3,
         reverse/1]).

-spec append([_], [_]) -> [_].
append(List1, List2) ->
    Append = fun (Acc, Elem) ->
        [Elem | Acc]
    end,
    foldl(Append, List2, reverse(List1)).

-spec concat([[_]]) -> [_].
concat(List) ->
    foldl(flip(fun append/2), [], reverse(List)).

-spec filter(fun((any()) -> boolean()), [_]) -> [_].
filter(Function, List) ->
    IncludeIf = fun (Acc, Elem) ->
        case Function(Elem) of
            true -> [Elem | Acc];
            false -> Acc
        end
    end,
    reverse(foldl(IncludeIf, [], List)).

-spec length([_]) -> non_neg_integer().
length(List) ->
    Count = fun (Acc, _Elem) ->
        1 + Acc
    end,
    foldl(Count, 0, List).

-spec map(fun((any()) -> any()), [_]) -> [_].
map(Function, List) ->
    Apply = fun (Acc, Elem) ->
        [Function(Elem) | Acc]
    end,
    reverse(foldl(Apply, [], List)).

-spec foldl(fun((any(), any()) -> any()), any(), [_]) -> any().
foldl(_Function, Acc, []) ->
    Acc;
foldl(Function, Acc, [Elem | Tail]) ->
    % While not documented, the expectation that the callback function
    % given to foldl/2 takes its arguments in the opposite order
    % expected by the stdlib is validated as a side effect of the
    % foldl_direction_dependent_function_applied_to_non_empty_list_test
    % test. The divide by zero error that throws with the incorrect
    % argument order took me a while to diagnose.
    foldl(Function, Function(Acc, Elem), Tail).

-spec foldr(fun((any(), any()) -> any()), any(), [_]) -> any().
foldr(Function, Initial, List) ->
    foldl(flip(Function), Initial, reverse(List)).

-spec reverse([_]) -> [_].
reverse(List) ->
    Append = fun (Acc, Elem) ->
        [Elem | Acc]
    end,
    foldl(Append, [], List).

%% Internal

flip(Function) ->
    fun (Acc, Elem) ->
        Function(Elem, Acc)
    end.
