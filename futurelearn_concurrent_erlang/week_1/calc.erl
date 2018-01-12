-module(calc).
-export([start/0, stop/0, execute/1]).
-export([init/0]).

start() ->
    spawn(calc, init, []).

stop() ->
    calc ! stop.

init() ->
    io:format("Starting...~n"),
    register(calc, self()),
    loop().

loop()->
    receive
        {request, From, Expr} ->
            From ! {reply, expr:eval(Expr)},
            loop();
        stop ->
            io:format("Terminating...~n")
    end.

execute(X) ->
    calc ! {request, self(), X},
    receive
        {reply, Reply} -> Reply
    end.
