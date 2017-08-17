-module(frequency_server).
-export([init/0]).

% Server = spawn(frequency_server, init, []).
% Server ! {request, self(), allocate}.
% receive {reply, Msg} -> Msg end.
% Server ! {request, self(), {deallocate, Freq}}.
% receive {reply, Reply} -> Reply end.

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

% hard coded
get_frequencies() ->
    [10, 11, 12, 13, 14, 15].

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            loop(NewFrequencies);
        {request, Pid, {deallocate, Freq}} ->
            NewFrequencies = deallocate(Frequencies, Freq),
            Pid ! {reply, ok},
            loop(NewFrequencies);
        {request, Pid, stop} ->
            Pid ! {reply, stopped}
    end.




%%%%%%%%%
%%% |Internal functions
%%%%%%%%%

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated},
     {error, no_frequency}};
allocate({[Freq | Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid} | Allocated]},
     {ok, Freq}}.


deallocate({Free, Allocated}, Freq) ->
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {[Freq | Free], NewAllocated}.
