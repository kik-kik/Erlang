-module(tuples).
-export([random_tuple/0, another_tuple/0]).

random_tuple() ->
  A_tuple = {3, 4, 5, 6}, % stores the tuple in A_tuple
  A_tuple, % prints the tuple.

  % we can also unpack the tuple into multiple variables:
  {A, B, C, D} = A_tuple,
  % we can access individual elements of the tuple now using the new variables.
  A, % returns 3

  % also if we only care about a single value, we can only store that specific value.
  % for example:
  {_, X, _, Y} = A_tuple,
  X + Y. % returns 4 + 6 => 10

another_tuple() ->
  Data = {height, 6.25},
  % unpacking the data into variables:
  {height, Height} = Data,
  Height. % returns 6.25
