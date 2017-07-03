-module(single_module_whoisinspace).
-export([start/0, makeGetRequest/1, decodeResponse/1]).

%-record(person, {name, craft}).

start() ->
  inets:start().

makeGetRequest(URL) ->
  Response = httpc:request(get, {URL, []}, [], []),
  getBody(Response).

getBody(Response) ->
  Body = responseBody(Response).

responseBody({ok, { _, _, Body}}) ->
  Body.

decodeResponse(Response) ->
  Decoded = jiffy:decode(Response, [return_maps]),
  getListofPeople(Decoded).

getListofPeople(Decoded) ->
  SpacePeople = maps:get(<<"people">>, Decoded),
  printLabels(),
  getAstroDetails(SpacePeople).

getAstroDetails([]) ->
  io:fwrite("~nfinished printing...~n");
getAstroDetails([H|T]) ->
  Name = maps:get(<<"name">>, H),
  Craft = maps:get(<<"craft">>, H),
  printAstro(Name, Craft),
  getAstroDetails(T).

printLabels() ->
  io:fwrite("~s\t\t|\t~s~n", ["Name", "Craft"]),
  io:fwrite("~s~n", ["----------------------------------------"]).

printAstro(Name, Craft) ->
  io:fwrite("~s\t|\t~s~n", [Name, Craft]).

% Response = whoisinspace:makeGetRequest("http://api.open-notify.org/astros.json").
% whoisinspace:decodeResponse(Response).

% interested in values from keys name, and craft

%print_list() ->
%  io:format("Name:|Craft:~n________|____"), % temp
  % io:format("~s~s~n",[Name, Craft]) % prints each astronauts name and craft.
