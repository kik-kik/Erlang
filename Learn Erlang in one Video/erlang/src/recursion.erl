-module(recursion).
-export([find_factorial/1, sum/1, sum_diff/2]).
-export([for/2]).

factorial(Num) when Num == 0 ->
  1;
factorial(Num) when Num > 0 ->
  Num * factorial(Num - 1).

find_factorial(Num) ->
  X = factorial(Num),
  io:fwrite("Factorial: ~p~n", [X]).

% 1st: 3 -> 3 * f(2) == 3 * 2 = 6
% 2nd: 2 -> 2 * f(1) == 2 * 1 (send above)
% 3rd: 1 -> 1 * f(0) == 1 * 1 (send above)

sum([]) -> 0;
sum([Head|Tail]) ->
  Head + sum(Tail).

% sum([1,2,3])
% 1 + sum([2,3])
% 1 + 2 + sum([3])
% 1 + 2 + 3 + sum(0)
% 1 + 2 + 3 + 0
% result equals to 6

% another way of doing it:
sum_diff([], Sum) -> Sum;
sum_diff([H|T], Sum) ->
  io:fwrite("Current sum : ~p~n", [Sum]),
  sum_diff(T, H + Sum).
% recursion:sum_diff([1,2,3,4], 0).

% =================================================
% Erlang does not have a for loop.
% We can make one:
for(0, _) ->
  ok;
% as long as max is more than 0 we continue doing what we're doing.
for(Max, Min) when Max > 0 ->
  io:fwrite("Num : ~p~n", [Max]),
  for(Max - 1, Min).
