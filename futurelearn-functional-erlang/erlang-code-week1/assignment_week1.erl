-module(assignment_week1).
-export([perimeter/1, area/1, enclose/1]).
-export([bits/1, bit/1]).


%%%%%%%
% Shapes
%%%%%%%
% functions to workout perimeter value for a specific shape.
perimeter({rectangle, {X, Y}}) ->
    ((X * 2) + (Y * 2));
perimeter({square, {X}}) ->
    (X * 4);
perimeter({triangle, {X, Y}}) ->
    Z = hypotenuse(X, Y),
    X + Y + Z.

% functions to workout the area for a specific shape.
area({rectangle, {X, Y}}) ->
    (X * Y);
area({square, {X}}) ->
    (X * 2);
area({triangle, {X, Y}}) ->
    Z = hypotenuse(X, Y),
    area({triangle, {X, Y, Z}});
area({triangle, {X, Y, Z}}) ->
    S = (X + Y + Z) / 2,
    math:sqrt(S * (S - X) * (S - Y) * (S - Z)).

% magic
enclose({triangle, {X, Y}}) ->
    Z = hypotenuse(X, Y),
    Side = erlang:max(X, Y),
    io:format("Smallest enclosing rectangle for this triangle has the following 4 sides: ~p, ~p, ~p, ~p.", [Z, Side, Z, Side]);
enclose({circle, {_R}}) ->
    1.


%%%%%%%%
% Summing the bits
%%%%%%%%
bits(N) ->
    bits(N, 0).
bits(N, Bits) when N < 1 ->
    Bits;
bits(N, Bits) when N rem 2 == 1 ->
    bits((N-1) div 2, Bits + 1);
bits(N, Bits) ->
    bits(N div 2, Bits).

bit(0) ->
    0;
bit(N) ->
    case N rem 2 ==  0 of
        true  -> bit(N div 2);
        false -> 1 + bit((N - 1) div 2)
    end.


%%%%%%%%%
% Private Functions
%%%%%%%%%
hypotenuse(X, Y) ->
    round(math:sqrt((X * X) + (Y * Y))).
