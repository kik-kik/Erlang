-module(what_language).
-export([origin_hello/1]).

origin_hello(Hello) ->
  % tries to pattern match the argument to a pattern.
  case Hello of
    hallo -> "German";
    hello -> "English";
    bonjour -> "French";
    czesc -> "Polish";
    _ -> "unknown" % acts as a default option.
end. % ends case expression.
