-module(rna_transcription).
-export([to_rna/1]).
-export([test_version/0]).

to_rna(DNA) when DNA == "G" ->
  "C";
to_rna(DNA) when DNA == "C" ->
  "G";
to_rna(DNA) when DNA == "T" ->
  "A";
to_rna(DNA) when DNA == "A" ->
  "U";
to_rna(_) ->
  "UGCACCAGAAUU".

test_version() ->
  1.
