-module(shopExample2).
-export([total/1]).
-import(lists, [map/2, sum/1]). % imports those functions from lists module and we dont have to specify module name: before using map or sum.

total(L) ->
		sum(map(fun({What, N}) -> shopExample:cost(What) * N end, L)).
