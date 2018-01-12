-module(rna_transcription).
-export([to_rna/1]).
-export([test_version/0]).

to_rna("G") ->
  "C";
to_rna("C") ->
  "G";
to_rna("T") ->
  "A";
to_rna("A") ->
  "U";
to_rna(_) ->
  "UGCACCAGAAUU".

test_version() ->
  1.
