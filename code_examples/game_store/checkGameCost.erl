-module(checkGameCost).
-export([gamePrice/1]).

gamePrice('door kickers') -> 3;
gamePrice('garrys mod')   -> 2;
gamePrice('gothic')       -> 1;
gamePrice('fallout')      -> 10;
gamePrice('total war')    -> 8;
gamePrice(Game)           -> io:format("~p is not currently available~n", [Game]);
gamePrice(_)              -> io:format("Invalid input~n").
