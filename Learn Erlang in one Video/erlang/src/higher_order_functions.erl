-module(higher_order_functions).
-export([do_math2/1, do_math3/1, fun_stuff/1, fun_stuff2/2]).

% ===================================
% Higher order functions are functions which either take a function
% as an argument or return a function.
% ===================================

double(X) -> X * 2.
triple(X) -> X * 3.

do_math2(List) ->
  lists:map(fun double/1, List).

do_math3(List) ->
  lists:map(fun triple/1, List).


fun_stuff(Name) ->
  Fun_stuff = fun() -> io:fwrite("Hello ~p~n", [Name]) end,
  % Fun_stuff() is returned.
  Fun_stuff().


fun_stuff2(X, Y) ->
  % z becomes a function which returns the sum of X + Y.
  Z = fun() ->
        io:fwrite("Sum : ~p~n", [X+Y])
      end,
  % Z() gets returned.
  Z().
