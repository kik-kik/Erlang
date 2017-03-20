#Erlang notes

##Origins:
Created by three guys:
  - Joe Armstrong
  - Robert Virding
  - Mike Williams

Elang was created in Ericsson for telocom switches in 1986 and went open source in 1998.

##Erlang shell:
Type '__erl__' to run erlang shell.
To quit the shell, press Ctrl + g then press q || type in q().

##Syntax:
__.__ <- the primary expression terminator.
__%%__ <- for comments (the number of % symbols for different scoping. For example: __%%%__ for file declaration, % for end of line comment).

### Predefined Data Types (also known as Terms):
  - Number
  - Atom (we have atoms true and false)
  - Bit strings and Binaries
  - References
  - Functional Identifier
  - Port Identifier
  - Pid
  - List
  - Tuple
  - Map (since release 17)

### Pattern Matching

### Variables (kind of)
  - Can only be bound once (they are immutable). Start with a capital letter.
### Function Clauses
Functions are identified by their name and the number of arguments they take. Pattern matching is used to match the clause, order is important because the first match wins.


Defining a function:
  -module(my_list).   _<- module declaration, defines the name of the module (actual file name - .erl extenstion)._
  -export([sum/1]).   _<- exporting function sum that takes 1 argument._

  sum(List) ->        <- _clause head_
      sum(List, 0).   <- _clause body_

Calling a function:
inside the same module:
  sum([1, 2, 3, 4, 5]). <- _function_name(arguments)._
outside of the module:
  my_list:sum([1, 2, 3, 4, 5]). <- _module_name:function_name(arguments)._

### Modules
Inside modules we also have exports. Allowed to have multiple export declarations. Any function that is declared in your module and is not declared in the export statement is a private function.

Example:
-module(my_math). _<- module declaration, defines the name of the module (actual file name - .erl extenstion)._
-export([sum/1]). _<- export([function_name/N_of_arguments_taken])._
