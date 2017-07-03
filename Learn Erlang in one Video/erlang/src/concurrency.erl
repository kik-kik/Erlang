-module(concurrency).
-export([run_spawner/0, run_for_spawner/0]).


% ============================================
%
% ============================================
% starts up 3 processes.
run_spawner() ->
  spawner(),
  spawner(),
  spawner().

get_id(ID) ->
  io:fwrite("ID : ~p~n", [ID]).

% spawns a process
spawner() ->
  % spawn spawns a process which passes it's process it as an argument to get_id function.
  spawn(fun() -> get_id(self()) end).

% ============================================
%
% ============================================
% the processes will take turns performing the specified action.
run_for_spawner() ->
  for_spawner(50, 1),
  for_spawner(100, 51).

for(0, _) ->
  ok;
for(Max, Min) when Max > 0 ->
  io:fwrite("Num : ~p~n", [Max]),
  for(Max - 1, Min).

for_spawner(Max, Min) ->
  spawn(fun() -> for(Max, Min) end).
