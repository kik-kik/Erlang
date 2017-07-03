-module(pattern_matching).
-export([max_three/3, how_many_equal/3, double/1]).

max_three(X, Y, Z) ->
    List = [X, Y, Z],
    get_max(List, 0).

get_max([], Max) ->
    Max;
get_max([Head | Tail], Max) ->
    case Head >= Max of
        true -> get_max(Tail, Head);
        false -> get_max(Tail, Max)
    end.

how_many_equal(X, X, X) -> 3;
how_many_equal(X, _, X) -> 2;
how_many_equal(X, X, _) -> 2;
how_many_equal(_, X, X) -> 2;
how_many_equal(_, _, _) -> 0.

mult(X,Y) ->

    X*Y.

double(X) ->

    mult(2,X).
