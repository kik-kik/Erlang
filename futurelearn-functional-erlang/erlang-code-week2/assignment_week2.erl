-module(assignment_week2).
-export([get_file_contents/1,show_file_contents/1]).
-export([main/2]).
-export([is_member/2]).
-export([search_file/2, search_line/2]).

%%% Week 2 Assignment: Indexing a file.
%%% See Functional_Erlang_Wk2_Assignment.pdf file for instructions.

main(Word, File) ->
    FileContents = get_file_contents(File),
    ListOfResults = search_file(Word, FileContents),
    % [get_tuples(X) || X <-ListOfResults],
    %{Word, {Tuple}}.
    % {Word , {ListOfResults}}. % should look like this: {"foo", [{[3,3],[4,5],[10,33]}}
    {Word, [get_tuples(X) || X <-ListOfResults]}.

get_tuples([]) ->
    [];
get_tuples(Item) ->
    Item.

search_file(Word, FileContents) ->
    search_file(Word, FileContents, 1, [], []).

search_file(_Word, [], _LineNum, _Acc, Res) ->
    Res;
search_file(Word, [Line | Lines], LineNum, Acc, Res) ->
    io:format("On line: ~p~n", [LineNum]),
    case search_line(Word, Line) of
        true ->
            io:format("On line: ~p~n", [LineNum]),
            search_file(Word, Lines, LineNum + 1, Acc ++ [LineNum], Res);
        false when Acc =/= [] ->
            search_file(Word, Lines, LineNum + 1, [], Res ++ [{[hd(Acc)] ++ [lists:nth(length(Acc), Acc)]}]);
        false ->
            search_file(Word, Lines, LineNum + 1, Acc, Res)
    end.

% Checking if a word is a member of a line:
search_line(Word, Line) ->
    StrippedLine = split_line(strip_line(Line)),
    StrippedWord = strip_line(Word),
    case is_member(StrippedWord, StrippedLine) of
        true -> true;
        false -> false
    end.

strip_line(Line) ->
    re:replace(string:to_lower(Line), "[^a-z0-9\s]", "", [global,{return,list}]).

split_line(Line) ->
    string:tokens(Line, " ").

is_member(_Word, []) ->
    false;
is_member(Word, [First | Rest]) ->
    case Word == First of
        true -> true;
        false -> is_member(Word, Rest)
    end.



%%%---------------
%%% functions pre-defined for the assignment to read text files
%%% and show the contents.
%%%---------------
% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.
get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
    lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.
get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.
show_file_contents(Lines) ->
    show_file_contents(Lines, 1).


show_file_contents([], _LineNum) ->
    ok;
show_file_contents([L|Ls], LineNum) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls, LineNum + 1).



%%%-------------------------
%%% Unit Tests
%%%-------------------------
