-module(totalCheck).
-export([calculateTotal/1]).
%-import(checkGameCost, [gamePrice/1]).

% if a list of tuples passed, do this:
calculateTotal([{Title, Quantity} | T]) ->
  checkGameCost:gamePrice(Title) * Quantity + calculateTotal(T);
% if a single typle is passed, do this:
calculateTotal({Title, Quantity}) ->
  checkGameCost:gamePrice(Title) * Quantity;
% if an empty list is passed, do this:
calculateTotal([]) ->
  0;
% wrong format input, do this:
calculateTotal(_) ->
  io:format("Wrong input format~n").
