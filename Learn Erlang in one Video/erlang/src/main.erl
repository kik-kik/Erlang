-module(main).
-export([main/0, var_stuff/0, atom_stuff/0, generate_random_num/1]).

main() ->
  io:fwrite("Inside function called 'main'").

var_stuff() ->
  Num = 1,
  Num. % returns 1

atom_stuff() ->
  an_atom. % returns an_atom

%% @spec generate_random_num(integer()) -> integer()
generate_random_num(Max) ->
  rand:uniform(Max).

equal(A, B) ->
  A =:= B. % strict equality the same type and value. 4 =:= 4.0 will return false.
  % A =/= B - strict inequality.
  % A == B - 4 == 4.0 will return true.
