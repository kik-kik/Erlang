-module(hello).
-export([hello_world/0, hello_world/1]).

hello_world() ->
  io:format("Hello erlang~n").

hello_world(Text) ->
  io:format("~s~n", [Text]).

% ~n for new line.s
% = is pattern matching operator.

% from terminal
% erlc hello.erl <- compiles hello.erl to .beam
% erl -noshell -s hello hello_world -s init stop <- runs the code and exits the shell.

% inside shell
% c(hello). <- to compile this module in the shell.
% module_name:function(args) <- to run a function from an external module.
% in this case: hello:hello_world().
