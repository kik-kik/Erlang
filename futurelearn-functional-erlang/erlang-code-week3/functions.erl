-module(functions).
-export([add/1]).
-export([add_two_to_all/1, add_to_all/2]).


% Add = functions:add(2).
% add(2) -> fun(Y) -> 2 + Y end.
% Add(4):
% add(2) -> fun(4) -> 2 + 4 end.
add(X) ->
    fun(Y) -> X + Y end.


% function that adds 2 to all members of a list:
add_two_to_all(List) ->
    lists:map(add(2), List).

add_to_all(N, List) ->
    lists:map(add(N), List).
