-module(readFileExample).
-export([
      readFile/1,
      showFileLength/1
        ]).

readFile(File) ->
  io:format("~nLoading File : ~p~n", [File]),
  {ok, FileData} = file:read_file(File),
  CharacterList = unicode:characters_to_list(FileData).

showFileLength(List) ->
  length(List).

% readFileExample:readFile("FILENAME").
% readFileExample:showFileLength(CharacterList).
