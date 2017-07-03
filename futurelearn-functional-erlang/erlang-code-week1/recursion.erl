-module(recursion).
-export([fib/1, fib_tail/3, fib_tail/1, pieces/1, loop/2, get_max/2, get_sum/2, perfect/1]).

-author("Krzysztof Ignasiak").

% fibernaci using direct recursion:
fib(0) ->
    0;
fib(1) ->
    1;
fib(N) ->
    fib(N-1) + fib(N-2).
% fib(4)
% = fib(2) + fib(3)
% = fib(2) + (fib(1) + fib(2))


% fibernaci using tail recursion:
fib_tail(0, P, _C) ->
    P;
fib_tail(N, P, C) ->
    fib_tail(N-1, C, P + C).

fib_tail(N) ->
    fib_tail(N, 0, 1).

pieces(0) ->
    1;
pieces(N) ->
    N + pieces(N-1).

% Max inclusive
loop(N, Max) when N < Max ->
    io:format("~p~n", [N]),
    loop(N+1, Max);
loop(N, _Max) ->
    N.

get_max([], Max) ->
    Max;
get_max([Head|Tail], Max) ->
    case Head > Max of
        true -> get_max(Tail, Head);
        false -> get_max(Tail, Max)
    end.


get_sum([Head | Tail], Total) ->
    get_sum(Tail, Head + Total);
get_sum([], Total) ->
    Total.

perfect(0) ->
    false;
perfect(N) when N > 0 ->
    perfect(1, N, []).
perfect(Count, N, List) when Count < N ->
    case N rem Count of
        0 -> perfect(Count + 1, N,  List ++ [Count]);
        _ -> perfect(Count + 1, N,  List)
    end;
perfect(_Count, N, List) ->
    case get_sum(List, 0) =:= N of
        true -> true;
        false -> false
    end.

