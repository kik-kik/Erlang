-module(string_stuff).
-import(string, [len/1, concat/2, chr/2, substr/3, str/2, to_lower/1, to_upper/1]).
-export([print_string/1, print_string/2, print_string/3]).
-export([concat_two_strings/2, get_char_index/2, contains_string/2, contains_char/2]).
-export([upper/1, lower/1]).

print_string(String) ->
  io:fwrite(String).

print_string(String1, String2) ->
  io:fwrite("String1: ~p ; String2: ~p~n", [String1, String2]). % includes "" around the strings when printing.

print_string(String1, String2, String3) ->
  Str = io_lib:format("~s ~s ~s~n", [String1, String2, String3]),
  io:fwrite(Str). % the same as above, but now no "" around Strings added.
  % len(Str). returns the length of the specified string.

concat_two_strings(String1, String2) ->
  concat(String1, String2). % concatinates two strings together.

get_char_index(String, Character) -> % argument ("string", $char)
  CharIndex = chr(String, Character),
  CharIndex.

contains_char(String, Character) -> % argument ("string", $char).
  chr(String, Character). % returns either true or false.

contains_string(String1, String2) ->
  str(String1, String2). % checks at what index the second string starts.

upper(String) ->
  to_upper(String). % converts the string to upper case.

lower(String) ->
  to_lower(String). % converts the string to lower case.
