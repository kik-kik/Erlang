-module(exercise_server).
-export([pal_check/0, palindrome_check/1]).

% Server = spawn(exercise_server, pal_check, []).
pal_check() ->
    receive
        {Sender, stop} ->
            io:format("SERVER_LOG: Stopping server...~n"),
            Sender ! {self(), stopped};
        {Sender, String} ->
            io:format("SERVER_LOG: Received string: ~s~n", [String]),
            io:format("SERVER_LOG: Checking if the string is a palin...~n"),
            Res = palindrome_check(String),
            Sender ! {self(), Res},
            pal_check()
    end.

%%%
% Helper functions given for this exercise
%%%
% remove special chars and punctuation.
rem_punct(String) -> lists:filter(fun (Ch) ->
                                      not(lists:member(Ch,"\"\'\t\n "))
                                    end,
                                    String).

% to lowercase.
to_small(String) ->
    lists:map(fun(Ch) ->
        case ($A =< Ch andalso Ch =< $Z) of
            true -> Ch+32;
            false -> Ch
        end
    end,
    String).

% check if palindrome.
palindrome_check(String) ->
    Normalise = to_small(rem_punct(String)),
    io:format("~s~n", [Normalise]),
    lists:reverse(Normalise) == Normalise.
