-module(concurrency).
-export([spawn_function/4, say_something/2]).


spawn_function(Value1, Value2, Value3, Times) ->
  spawn(concurrency, say_something, [Value1, Times]),
  spawn(concurrency, say_something, [Value2, Times]),
  spawn(concurrency, say_something, [Value3, Times]).

say_something(_, 0) ->
  io:format("Done ~n");
say_something(Value, Times) ->
  io:format("~s ~n", [Value]),
  say_something(Value, Times - 1).
