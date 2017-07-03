-module(chapter1).
-export([start/0]).

start() ->
  Joe       = spawn(person, init, ["Joe"]),
  Susannah  = spawn(person, init, ["Susannah"]),
  Dave      = spawn(person, init, ["Dave"]),
  Andy      = spawn(person, init, ["Andy"]),
  Rover     = spawn(dog,    init, ["Rover"]),
  Rabbit    = spawn(rabbit, init, ["Flopsy"]).
  % spawn is a primitive that creates a concurrent process and returns a PID.
  % PID <- process identifier. Can be used to interact with the process.

% now we can use:
% Joe ! {self(), "Who let the dogs out?"}

% Syntax: PID ! message
% sends the message to the specified PID.
% self() specifies the process sending the message.

% Joe, in this case, receives the message.
receive
  {From, Message} ->
    ...
% var From will contain the sender so that current process knows who it came from.
% Message will contain the message sent.
