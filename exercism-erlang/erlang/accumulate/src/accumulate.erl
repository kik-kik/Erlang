-module(accumulate).
-export([accumulate/2]).
-export([test_version/0]).

accumulate(Fn, Ls) ->
    [Fn(X) || X <- Ls].

test_version() ->
  1.
