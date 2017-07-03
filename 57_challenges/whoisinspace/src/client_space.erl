-module(client_space).
-export([sendURL/2]).

% client_space:sendURL(Server, "http://api.open-notify.org/astros.json").
sendURL(Server, URL) ->
  Server ! {self(), list_astronauts, URL},
  receive
    {Server, Response} ->
      io:fwrite("~nServer response: ~s~n", [Response])
  end.
