-module(server_space).
-export([start/0, loop/0]).

% Server = server_space:start().

start() ->
  spawn(server_space, loop, []).

loop() ->
  receive
    {Sender, list_astronauts, URL} ->
      Response = makeGetRequest(URL),
      Decoded = decodeResponse(Response, Sender)
  end,
  loop().


makeGetRequest(URL) ->
  Response = httpc:request(get, {URL, []}, [], []),
  getBody(Response).

getBody(Response) ->
  Body = responseBody(Response).

responseBody({ok, { _, _, Body}}) ->
  Body.

decodeResponse(Response, Sender) ->
  Decoded = jiffy:decode(Response, [return_maps]),
  getListofPeople(Decoded, Sender).

getListofPeople(Decoded, Sender) ->
  SpacePeople = maps:get(<<"people">>, Decoded),
  printLabels(),
  getAstroDetails(SpacePeople, Sender).

getAstroDetails([], Sender) ->
  %io:fwrite("~nfinished printing...~n");
  Sender ! {self(), "finished"};
getAstroDetails([H|T], Sender) ->
  Name = maps:get(<<"name">>, H),
  Craft = maps:get(<<"craft">>, H),
  printAstro(Name, Craft),
  getAstroDetails(T, Sender).

printLabels() ->
  io:fwrite("~s\t\t|\t~s~n", ["Name", "Craft"]),
  io:fwrite("~s~n", ["----------------------------------------"]).

printAstro(Name, Craft) ->
  io:fwrite("~s\t|\t~s~n", [Name, Craft]).
