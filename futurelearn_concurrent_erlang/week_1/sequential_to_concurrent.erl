-module(sequential_to_concurrent).
-export([area/1]).
-export([area/0]).
-export([spawn_process_with_message/1]).

% An example of a simple sequential function to calculate an area.
area({square, X}) ->
    X * X;
area({rectangle, X, Y}) ->
    X * Y.

% to turn the area function into a concurrent function we need to wrap these functions in a receive statement.
% i.e. we turn it into a process code.
area() ->
    receive
        {From, {square, X}} -> % From is the identify of the process that sent the received message.
            From ! {self(), X * X};
        {From, {rectangle, X, Y}} ->
            From ! {self(), X * Y}
    end,
    area().
% to use it now we need to spawn a new process:
spawn_process_with_message(Num) ->
    Pid = spawn(sequential_to_concurrent, area, []),
    Pid ! {self(), {square, Num}}, % sends a message with the (self) process id so that the process that receives it knows who to send it back to.
    receive
        % we only collect the message send back by the process which we sent a message to previously.
        {Pid, Response} ->
            Response
    % However what if a process does does not respond, we could end up getting a process stuck.
    % not to worry, we have after keyword which will enable a timeout after specified time.
    after
        5000 ->
            io:format("Connection timed out..")
    end.
