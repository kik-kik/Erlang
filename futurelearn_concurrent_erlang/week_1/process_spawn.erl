-module(process_spawn).
-export([some_process/0]).

% to spawn this process run: spawn(process_spawn, some_process, []). in the erlang shell.
some_process() ->
    timer:sleep(500),
    io:format("a process has been spawned.~n"),
    io:format("working....~n"),
    io:format("Process finished~n").
