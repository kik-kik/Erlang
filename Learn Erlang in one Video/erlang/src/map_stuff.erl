-module(map_stuff).
-export([mapping/0]).

% map is a grouping of key value pairs.

mapping() ->
  % defining a key value pair:
  % Var_name = #{key=> value, key=> value}
  Tom = #{f_name=> 'Tom', l_name=> 'Lee'},

  io:fwrite("1st name : ~p~n", [maps:get(f_name, Tom)]),
  io:fwrite("last name : ~p~n", [maps:get(l_name, Tom)]),

  % shows all keys found within Tom
  io:fwrite("Keys : ~p~n", [maps:keys(Tom)]),

  % shows all the values within Tom
  io:fwrite("Values : ~p~n", [maps:values(Tom)]),

  % removes key f_name and its value from Tom
  io:fwrite("After Removing : ~p~n", [maps:remove(f_name, Tom)]),

  % checking if l_name exists inside Tom and returns it's value, if not it returns error:
  io:fwrite("Key l_name exists inside Tom : ~p~n", [maps:find(l_name, Tom)]),

  % adding a key and a value to Tom
  io:fwrite("Adding a key and a value to Tom : ~p~n", [maps:put(address, "Street", Tom)]).
