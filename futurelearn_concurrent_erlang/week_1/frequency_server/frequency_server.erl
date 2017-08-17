-module(frequency_server).
-export([loop/1]).


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
