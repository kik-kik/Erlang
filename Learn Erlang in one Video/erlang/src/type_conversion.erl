-module(type_conversion).
-export([type_stuff/0]).

type_stuff() ->
  is_atom(an_atom),
  is_float(3.14),
  is_integer(24),
  is_boolean(true),
  is_list([1,2,3]),
  is_tuple({atom, 6.24}). % is_type checks type.

% ===============================================
%
% We can convert from one type to another using
% type_to_type
% atom_to_binary, atom_to_list, binary_to_atom,
% binary_to_list, bitstring_to_list, binary_to_term,
% float_to_list, fun_to_list, integer_to_list,
% integer_to_list, iolist_to_binary, iolist_to_atom,
% list_to_atom, list_to_binary, list_to_bitstring,
% list_to_float, list_to_integer, list_to_pid,
% list_to_tuple, pid_to_list, port_to_list, ref_to_list,
% term_to_binary, term_to_binary, tuple_to_list
