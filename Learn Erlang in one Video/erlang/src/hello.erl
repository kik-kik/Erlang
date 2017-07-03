-module(hello).
-export([hello_world/0, add/2, add/3]).

hello_world() ->
  io:fwrite("Hello World\n").

add(X, Y) ->
  hello_world(),
  X + Y.

add(X, Y, Z) ->
  hello_world(),
  X + Y + Z.
