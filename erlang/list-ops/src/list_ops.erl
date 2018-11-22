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
    Append = fun (Elem, Acc) ->
        [Elem | Acc]
    end,
    foldl(Append, List2, reverse(List1)).

-spec concat([[_]]) -> [_].
concat(List) ->
    foldl(fun append/2, [], reverse(List)).

-spec filter(fun((any()) -> boolean()), [_]) -> [_].
filter(Function, List) ->
    IncludeIf = fun (Elem, Acc) ->
        case Function(Elem) of
            true -> [Elem | Acc];
            false -> Acc
        end
    end,
    reverse(foldl(IncludeIf, [], List)).

-spec length([_]) -> non_neg_integer().
length(List) ->
    Count = fun (_Elem, Acc) ->
        1 + Acc
    end,
    foldl(Count, 0, List).

-spec map(fun((any()) -> any()), [_]) -> [_].
map(Function, List) ->
    Apply = fun (Elem, Acc) ->
        [Function(Elem) | Acc]
    end,
    reverse(foldl(Apply, [], List)).

-spec foldl(fun((any(), any()) -> any()), any(), [_]) -> any().
foldl(_Function, Acc, []) ->
    Acc;
foldl(Function, Acc, [Head | Tail]) ->
    foldl(Function, Function(Head, Acc), Tail).

-spec foldr(fun((any(), any()) -> any()), any(), [_]) -> any().
foldr(Function, Acc, List) ->
    foldl(Function, Acc, reverse(List)).

-spec reverse([_]) -> [_].
reverse(List) ->
    Append = fun (Elem, Acc) ->
        [Elem | Acc]
    end,
    foldl(Append, [], List).
