-module(sending_message).
-export([sample_process/0, send/2]).

% spawn this function:
% Target = spawn(sending_message, sample_process, []).
sample_process() ->
    io:format("Waiting for a message~n"),
    receive
        stop -> io:format("Stopping..."); % Pid ! stop.
        Msg ->
            io:format("Message received:~n"),
            io:format("~p~n", [Msg]),
            sample_process()
        end.

send(Target, stop) ->
    Target ! stop;
send(Target, Txt) ->
    Target !  Txt.

