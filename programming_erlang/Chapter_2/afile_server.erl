-module(afile_server).
-export([start/1, loop/1]).

start(Dir) ->
  spawn(afile_server, loop, [Dir]).

loop(Dir) ->
  receive
    {Client, list_dir} ->
      Client ! {self(), file:list_dir(Dir)};
    {Client, {get_file, File}} ->
      Full = filename:join(Dir, File),
      Client ! {self(), file:read_file(Full)}
  end,
  loop(Dir).

% infinite loop, we wait until we receive a command, once we do wo do something
% and call ourselves again.

% process is a lightweight virtual machine that can communicate with other processes
% only by sending and receiving messages. To do something, you can send
% a message and wait for a reply.


% FileServer = afile_server:start(".").
% FileServer ! {self(), list_dir}.
% receive X -> X end.
