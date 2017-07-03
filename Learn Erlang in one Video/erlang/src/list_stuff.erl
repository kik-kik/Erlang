-module(list_stuff).
-export([random_list/0, get_list_head/1, get_list_tail/1, get_head_and_tail/1]).
-export([double_each_element/1, get_even_numbers/1, is_even/1]).
-export([tuple_list/0]).

random_list() ->
  List1 = [1, 2, 3],
  List2 = [4, 5, 6],
  List3 = List1 ++ List2, % combines the lists together.
  List3, % returns the list. [1,2,3,4,5,6]
% ===========================================
  List4 = List3 -- List1, % subtracts list 1 from list 3.
  List4. % returns list4 [4,5,6]

% @spec get_list_head(list()) -> hd(list()).
get_list_head(List) ->
  hd(List). % returns the first element of the list.

% @spec get_list_head(list()) -> tl(list()).
get_list_tail(List) ->
  tl(List). % returns all elements of the list except for the first one.

% gets head and tail of the list.
get_head_and_tail([Head|Tail]) ->
  ReturnVal = io_lib:format("Head: ~p | Tail: ~p~n", [Head, Tail]),
  io:fwrite(ReturnVal).

% ============================================
%% List Comprehension

% @desc doubles the value of each element in the list.
double_each_element(List) ->
  % [ expression/actions to be applied || grabbing an element to perform operations on]
  NewList = [2 * X || X <- List]. % X is the temporary name for each List item.

% @desc returns even numbers from the list.
get_even_numbers(List) ->
  % [ Element || Element <- List, conditions]
  Evens = [ X || X <- List, X rem 2 =:= 0],
  Evens.

% @desc checks if element is even and returns true or false.
is_even(List) ->
  % returns true or false for each element.
  Results = [ X rem 2 =:= 0 || X <- List].

% ============================================
% List of tuples

tuple_list() ->
  % makes a tuple
  City_weather = [{belfast, 10}, {london, 14}, {dublin, 12}],
  % goes through the list of tuples and grabs the tuples which have Temperature value of more than 11.
  Temp_over_11 = [{City, Temperature} || {City, Temperature} <- City_weather, Temperature > 11],
  % returns the results.
  Temp_over_11.
