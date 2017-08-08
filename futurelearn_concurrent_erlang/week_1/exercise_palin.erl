-module(exercise_palin).
-export([is_palin/2]).


is_palin(Server, String) ->
    Server ! {self(), String},
    receive
        {_Server, stopped} ->
            io:format("INFO: Server stopped...");
        {Server, Result} ->
            case Result of
                true  -> io:format("Result: is palin.~n");
                false -> io:format("Result: is not palin.~n")
            end
    end.
