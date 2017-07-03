-module(list_stuff).
-export([sum/1, sum_tail/1]).
-export([get_max/1, get_max_tail/1]).
-export([get_product/1, get_product_tail/1]).
-export([get_even_number_list/1]).
-export([get_circles/1]).
-export([double/1, even_numbers/1]).
-export([get_modes/1, get_median/1]).
-export([sort_list/1]).

%%--------------
%% Calculating a sum of a list
%% using direct recursion.
%%--------------
sum([]) -> 0;
sum([H|T]) ->
    H + sum(T).

%%--------------
%% Calculating a sum of a list
%% using tail recursion.
%%--------------
sum_tail(List) ->
    sum_tail(List, 0).
sum_tail([], Sum) -> % Sum is an accumulator.
    Sum;
sum_tail([H|T], Sum) ->
    sum_tail(T, H + Sum).


%%--------------
%% Getting the max value in a list (direct recursion)
%%--------------
get_max([]) ->
    0;
get_max([H|T]) ->
    max(H, get_max(T)).

%%--------------
%% Getting the max value in a list (tail recursion)
%%--------------
get_max_tail(List) -> get_max_tail(List, 0).
get_max_tail([], Max) -> Max;
get_max_tail([H | T], Max) -> get_max_tail(T, max(H, Max)).


%%--------------
%% Getting a product for a list of numbers (direct recursion).
%%--------------
get_product([]) ->
    1;
get_product([H | T]) ->
    H * get_product(T).

%%--------------
%% Getting a product for a list of numbers (tail recursion).
%%--------------
get_product_tail(List) ->
    get_product_tail(List, 1).
get_product_tail([], P) ->
    P;
get_product_tail([H | T], P) ->
    get_product_tail(T, H * P).

%%--------------
%% Making a list of even numbers:
%%--------------
get_even_number_list([]) ->
    [];
get_even_number_list([H | T]) ->
    case H rem 2 =:= 0 of
        true -> [H | get_even_number_list(T)];
        false -> get_even_number_list(T)
    end.

%%--------------
%% Making a list of cicles:
%%--------------
% Shapes = [ {circle, {3, 2}}, {square, {3, 3}}, {reclangle, {3, 4}}, {circle, {2, 3}}  ].
% Could also use a case statement:
get_circles([]) ->
    [];
get_circles([{circle, {_, _}} = Circle | Tail]) ->
    [Circle | get_circles(Tail)];
get_circles([{_, {_, _}} | Tail]) ->
    get_circles(Tail).


%%--------------
%% transforming list elements, doubling each element:
%%--------------
double(List) when is_list(List)->
    [X * 2|| X <- List].

%%--------------
%% filtering lists, extracting even numbers from a list:
%%--------------
even_numbers(List) when is_list(List) ->
    [X || X <- List, X rem 2 == 0].

%%--------------
%% median, modes
%%--------------
get_median(List) ->
    SortedList = sort_list(List),
    ListLength = length(SortedList),
    case ListLength rem 2 == 0 of
        true -> (lists:nth(ListLength div 2, SortedList) + lists:nth(ListLength div 2 + 1, SortedList)) div 2;
        false -> lists:nth(ListLength div 2 + 1, SortedList)
    end.

% TODO
get_modes(_List) ->
    ok.

%%--------------
%% sorting a list
%%--------------
sort_list([]) ->
    [];
sort_list([Pivot | T]) ->
    sort_list([ X || X <- T, X < Pivot ])
    ++ [Pivot] ++
    sort_list([ X || X <- T, X >= Pivot ]).

% TODO
%%--------------
%% how many occurances:
%%--------------

