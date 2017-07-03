-module(second).
-export([hypotenuse/2, perimeter/2, area_two_sides/2]).

hypotenuse(X, Y) ->
    math:sqrt(first:mult(X, X) + first:mult(Y, Y)).

perimeter(A, B) ->
    C = hypotenuse(A, B),
    A + B + C.

area_two_sides(A, B) ->
    C = hypotenuse(A, B),
    first:area(A, B, C).
