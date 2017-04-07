-module(mapExample).
-export([map/2]).

% nothing returned if an empty list is given as arg.
map(_, []) ->
  [];
% takes a function and a list as args
% splits the list into H and T, H gets passed into the function and the function gets called
% again with the T[ail]. This continues till nothing is left in T.
map(F, [Head | Tail]) ->
  [F(Head) | map(F, Tail)].

% mapExample:map(fun(N) -> 3 * N end, [1, 4, 2, 1]).
