# Erlang notes

## Origins:
Created by three guys:
  - Joe Armstrong
  - Robert Virding
  - Mike Williams

Elang was created in Ericsson for telocom switches in 1986 and went open source in 1998.

## Erlang shell:
- Type '__erl__' to run erlang shell.
- To quit the shell, press __Ctrl + g__ then press __q__ || type in __q().__

## Syntax:
- Erlang is a dynamic language.
- __.__ <- the primary expression terminator.
- __%%__ <- for comments (the number of % symbols for different scoping. For example: __%%%__ for file declaration, % for end of line comment).
- Whitespace insensitive, but convention is 4 spaces for indentation.

### ; , or .
- __,__ separates expressions, but only the result of the last one is shown. Also separates function arguments, data constructors, and patterns.
- __;__ separates clauses, for example: functions with the same name and arity and in case, if, try...catch, and receive expressions.
- __.__ terminates expressions and function clauses.

_Example:_
```erlang
some_function(function1) ->
  io:fwrite("Inside a function"),
  % we continue the execution after the , until we hit ; or .
  io:fwrite("still here!");
some_function(function2) ->
  io:fwrite("Oh look, we're in the other function!").

% _Explore pattern matching to understand how the decision to is made to run a specific function._
```

## Predefined Data Types (also known as Terms):
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

### Numbers
-

### atoms
- atom is an atom, name is also its value. starts with lower case or lives inside ''.
__Important!__
> _there are pitfalls to using atoms for too many things: an atom is referred to in an "atom table" which consumes memory (4 bytes/atom in a 32-bit system, 8 bytes/atom in a 64-bit system). The atom table is not garbage collected, and so atoms will accumulate until the system tips over, either from memory usage or because 1048577 atoms were declared._ (Starting Out (for real) - learn you some Erlang).

- Reserved words which cannot be used as an atom:

```erlang
after and andalso band begin bnot bor bsl bsr bxor case catch cond div end fun if let not of or orelse query receive rem try when xor
```

### Lists


### Tuples


### Booleans
- Boolean algebra includes: _and, or, xor, not, not (true and true)_.
- for short-circuit operators which only evaluates right-side argument only if needed use: _andalso_ and _orelse_.
- __Important!__ -> Erlang does not have actual booleans, the terms true and false are actually atoms:
```erlang
0 == false. % returns false
1 < false.  % returns true
```
This is because of the following ordering of each element in a comparison:
```erlang
number < atom < reference < fun < port < pid < tuple < list < bit string
```

### Equality and inequality:
```erlang
9 =:= 9.   % true  test for exact equality
3 =:= 2.   % false
5 =/= 1.   % true
2 =:= 2.0. % false
2 == 2.0.  % true
5 /= 5.0.  % false
```

### Comparison operators:
```erlang
3 < 4.  % true
3 < 3.  % false
2 >= 2. % true
4 =< 4. % true
```

## Pattern Matching
__Important!__
- __=__ is actually not an assignment operator, it is a pattern matching operator!
- clause order matters, for example: only the first function to match a pattern will get executed.


## Variables (kind of)
  - Can only be bound once (they are immutable). Start with a capital letter.
## Function Clauses
- Functions are identified by their name and the number of arguments they take.
- Pattern matching is used to match the clause, order is important because the first match wins.

## Modules
- Modules contain functions, building blocks which can be used together to solve larger problems. They can help to reduce code complexity and promote code reusability.
- Files with .erl extension which contain basic units of code in Erlang, and have to be compiled before use. Compiled files have .beam file extension.
- Inside modules we also have exports. Allowed to have multiple export declarations. Any function that is declared in your module and is not declared in the export statement is a private function.

Example:
```erlang
-module(my_math). % _<- module declaration, defines the name of the module (actual file name - .erl extenstion)._
-export([function1/0, function2/1]). % _<- export declaration, export([function_name/N_of_arguments_taken])._

function1() ->
  %% some expression[s]

function2(Arg) ->
  %% some expression[s]
```

## Functions
- Each function clause has a head and a body separated by ->
- Head consists of a function name, followed by zero or more patterns.
- Function body consists of a sequence of expressions.
- If functions head is successfully pattern matched then the expressions inside its body are executed.
- No return keyword, the return value of a function is the result of the last expression inside the function.
- Functions which are not exported are essentially private, they cannot be called from outside the module.

Defining a function:
```erlang  
  -module(my_list).   
  -export([sum/1]).   % <- _exporting function sum that takes 1 argument._

  sum(List) ->        % <- _clause head_
      sum(List, 0).   % <- _clause body_
```
Calling a function:
inside the same module:
  ```erlang
  sum([1, 2, 3, 4, 5]). % <- _function_name(arguments)._
  ```
outside of the module: % <- _when using a function from an external module._
```erlang
  my_list:sum([1, 2, 3, 4, 5]). % <- _module_name:function_name(arguments)._
```

### Higher-order functions (aka __funs__)
- Functions that return functions or take other functions as an argument.
- Functions that manipulate functions.
- 
- Example fun:

```erlang
Double = fun(X) -> X * 2.
Double(2) % returns 4
```

### Built-in commands:

- pwd() - prints current working directory.
- ls() - lists names of files in current directory.
- cd(Dir) - changes the current directory to Dir.

## Accumulators
