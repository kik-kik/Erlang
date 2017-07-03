-module(file_io).
-export([write_text/2, append_text/2, read_file/1]).

% ============================================
% Writing text to file (overwritting)
% ============================================
% file_io:write_text("test.txt","written to file...").
write_text(File, Text) ->
  % opens the specified file, if it doesn't exist it's created.
  {ok, Data} = file:open(File, [write]), % [write] -> contents of the file get overwritten.
  % writitng the contents of Text to Data file object.
  file:write(Data, Text).

% ============================================
% Appending text to file
% ============================================
% file_io:append_text("test.txt","text appended").
append_text(File, Text) ->
  {ok, Data} = file:open(File, [append]), % [append] -> Text will be added at the end of current data.
  file:write(Data, Text).

% ============================================
% Reading file
% ============================================
read_file(File) ->
  {ok, Data} = file:open(File, [read]),
  Contents = file:read(Data, 1024*1024),
  io:fwrite("Contents of the file ~p : ~p~n", [File, Contents]).
