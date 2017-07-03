-module(record_stuff).
-export([make_customer_records/0]).

-record(customer, {name = "", balance = 0.00}).

make_customer_records() ->
  % creating a new record using customer type and giving a value to different keys.
  John = #customer{name="John", balance = 52.20},

  % grabbing details from John variable, modifying value of the key balance and storing it in a new variable.
  John2 = John#customer{balance = 20.20},

  % printing the value of the key balance found in John2 record.
  io:fwrite("~p has balance of: £~p~n", [John2#customer.name, John2#customer.balance]).
