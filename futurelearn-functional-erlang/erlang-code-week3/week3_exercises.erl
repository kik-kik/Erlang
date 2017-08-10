-module(week3_exercises).
-export([doubleAll/1, evens/1, product/1]).
-export([double_all/1]).
-export([evens_filter/1]).
-export([product_foldr/1]).
-export([zip/2, zip_with/3, zip_with_map/3]).

%%%%%%%%%
%% Using higher-order functions
%%%%%%%%%
% define double_all, evens, and product using higher order functions
% lists:filter, lists:map, and lists:foldr
%%%%%%%%%
doubleAll([]) -> [];
doubleAll([X|Xs]) ->
    [ 2*X | doubleAll(Xs) ].

evens([]) -> [];
evens([X|Xs]) when X rem 2 == 0 ->
    [X | evens(Xs) ];
evens([_|Xs]) ->
    evens(Xs).

product([]) -> 1;
product([X|Xs]) -> X * product(Xs).
%%%%%%%%%
% Using HOF's
%%%%%%%%%
% double_all()
double_all(List) -> lists:map(fun double/1, List).

double(X) ->
    X * 2.

% evens()
evens_filter(List) -> lists:filter(fun is_even/1, List).

is_even(X) when X rem 2 == 0 -> true;
is_even(_X) -> false.

% product
product_foldr([]) -> 1;
product_foldr(List) ->
    lists:foldr(fun mult/2, 1, List).

mult(X, Y) ->
    X * Y.

% product(L) -> lists:foldr(fun(X, Acc) -> X * Acc end, 1, L).


%%%%%%%%%
%% Zipping
%%%%%%%%%
% a) Define a function zip/2 that “zips together” pairs of elements from two lists like this:
%    zip([1,3,5,7], [2,4]) = [ {1,2}, {3,4} ]
%%%%%%%%%
zip([L1 | L1Rest], [L2 | L2Rest]) ->
    [{L1, L2} | zip(L1Rest, L2Rest)];
zip(_, _)-> [].
%%%%%%%%%
% b) Define a function zip_with/3 that “zips together” pairs of elements from two lists using the function in the first argument, like this:
%    zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]) = [ 3, 7 ]
%%%%%%%%%
zip_with(F, [Item1 | TList1], [Item2 | TList2]) ->
    [F(Item1, Item2) | zip_with(F, TList1, TList2)];
zip_with(_F, _, _) -> [].
% example: week3_exercises:zip_with(fun(X, Y) -> X + Y end, [1,2,3], [4,5,6]). returns [5,7,9]

%%%%%%%%%
% c) Re-define the function zip_with/3 using zip and lists:map.
%%%%%%%%%
zip_with_map(F, List1, List2) ->
    lists:map(fun({X, Y}) -> F(X, Y) end, zip(List1, List2)).



%%%%%%%%%
% d) Re-define zip/2 using zip_with/3.
%%%%%%%%%

