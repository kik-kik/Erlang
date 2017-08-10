-module(rock_paper_scissors).

-export([beats/1, lose/1]).
-export([beats_test/0, lose_test/0]).
-export([result/2, result_test/0, result_conversion_test/0]).
-export([enum_test/0, val_test/0]).
-export([run_all_tests/0]).
-export([tournament/2, tournament_test/0]).

beats(rock) ->      paper;
beats(scissors) ->  rock;
beats(paper) ->     scissors;
beats(_) ->         unknown.

lose(rock) ->       scissors;
lose(paper) ->      rock;
lose(scissors) ->   paper;
lose(_) ->          unknown.

% result()
result(rock, rock)          -> draw;
result(rock, scissors)      -> win;
result(rock, paper)         -> lose;
result(scissors, scissors)  -> draw;
result(scissors, paper)     -> win;
result(scissors, rock)      -> lose;
result(paper, paper)        -> draw;
result(paper, rock)         -> win;
result(paper, scissors)     -> lose.

result_conversion(win)  ->  1;
result_conversion(draw) ->  0;
result_conversion(lose) -> -1.

tournament(Player1Moves, Player2Moves) ->
    % lists:zipwith results in [win,lose, win] format,
    % lists:map then converts these into computable digits,
    % sum then sum up those numbers.
   lists:sum(lists:map(fun result_conversion/1,
             lists:zipwith(fun result/2, Player1Moves, Player2Moves))).

% we could also represent rock, paper, and scissors using numeric values.
enum(0) -> rock;
enum(1) -> paper;
enum(2) -> scissors.

val(0) -> rock;
val(1) -> paper;
val(2) -> scissors.

%%%%%%%%%%%%%
%%% TESTS
%%%%%%%%%%%%%
run_all_tests() ->
    beats_test(),
    lose_test(),
    result_test(),
    result_conversion_test(),
    tournament_test(),
    enum_test(),
    val_test(),
    ok.

% beats() tests:
beats_test() ->
    paper       = beats(rock),
    rock        = beats(scissors),
    scissors    = beats(paper),
    unknown     = beats(123),
    ok.


% lose() tests:
lose_test() ->
    paper       = lose(scissors),
    rock        = lose(paper),
    scissors    = lose(rock),
    unknown     = lose(wrong),
    ok.

% result() test:
result_test() ->
    draw = result(rock, rock),
    win  = result(rock, scissors),
    lose = result(rock, paper),
    draw = result(scissors, scissors),
    win  = result(scissors, paper),
    lose = result(scissors, rock),
    draw = result(paper, paper),
    win  = result(paper, rock),
    lose = result(paper, scissors),
    ok.

result_conversion_test() ->
    1  = result_conversion(win),
    0  = result_conversion(draw),
    -1 = result_conversion(lose),
    ok.

tournament_test() ->
    1  = rock_paper_scissors:tournament([paper,paper,rock],[rock,rock,paper]),
    -1 = rock_paper_scissors:tournament([rock,rock,rock],[rock,rock,paper]),
    0  = rock_paper_scissors:tournament([paper,rock,rock],[rock,rock,paper]),
    ok.

enum_test() ->
    rock     = enum(0),
    paper    = enum(1),
    scissors = enum(2),
    ok.

val_test() ->
    rock     = val(0),
    paper    = val(1),
    scissors = val(2),
    ok.

