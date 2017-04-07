-module(listSum).
-export([sum/1]).

sum([H|T]) ->
  H + sum(T);
sum([]) ->
  0.

% listSum:sum([1, 2, 4]).
