-module(generics).
-export([all_areas/1, all_areas_2/1, area/1]).
-export([map/2]).
-export([circles/1]).
-export([get_circles/1]).
-export([sum/1]).
-export([sum_reduce/1]).

%%%%%%%%%%%%%%%%%%
%%% Map function
%%%%%%%%%%%%%%%%%%
%%--------
% area()
%%--------
all_areas([]) -> [];
all_areas([X|Xs]) -> [area(X) | all_areas(Xs)].

area(X) ->
    X * X.

%%--------
% generic map function
%%--------
map(_F, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].

%% putting it all together:
all_areas_2(Shapes) -> map(fun area/1, Shapes).


%%%%%%%%%%%%%%%%%%
%%% Filter function
%%%%%%%%%%%%%%%%%%
%%---------
% circles()
%%---------
circles([]) -> [];
circles([{circle, {X, Y}, R} | Tail]) ->
    [{circle, {X, Y}, R} | circles(Tail)];
circles([{rectangle, {_, _}, _, _} | Tail]) ->
    circles(Tail).

%%--------
% generic filter function
%%--------
filter(_P, []) -> [];
filter(P, [X | Tail]) ->
    case P(X) of
        true -> [X | filter(P, Tail)];
        false -> filter(P, Tail)
    end.

is_circle({circle, {_, _}, _}) -> true;
is_circle({rectangle, {_, _}, _, _}) -> false.

%% putting it all together:
get_circles(Circles) ->
    filter(fun is_circle/1, Circles).

%%%%%%%%%%%%%%%%%%
%%% Reduce/Fold function
%%%%%%%%%%%%%%%%%%

%%---
% Sum function
%%---
sum([]) -> 0;
sum([H | Tail]) ->
    H + sum(Tail).

%%---
% Sum using reduce
%%---
reduce(_Combine, Start, []) ->
    Start;
reduce(Combine, Start, [H|Tail]) ->
    Combine(H, reduce(Combine, Start, Tail)).

sum_reduce(Nums) -> reduce(fun plus/2, 0, Nums).

plus(X, Y) ->
    X + Y.

