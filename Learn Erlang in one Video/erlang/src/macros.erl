-module(macros).
-export([macro_stuff/2]).

% =========================================
% Macros provide inline code replacement.
% =========================================

% defining a macro: add which gets 2 parameters X and Y after , we specify what to do with X and Y.
-define(add(X, Y), {X + Y}).

macro_stuff(X, Y) ->
  % we use ? to indicate that we are using a macro.
  io:fwrite("~p~n", [?add(X, Y)]).
