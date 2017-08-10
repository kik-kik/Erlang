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

### Built-in commands:

- pwd() - prints current working directory.
- ls() - lists names of files in current directory.
- cd(Dir) - changes the current directory to Dir.

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
- Lists can contain anything: numbers, atoms, tuples, other lists.
- We can add items to a list using `++` operator and remove items from a list using the `--` operator.
```erlang
[1,2,3] ++ [4,5] % results in: [1,2,3,4,5]
[1,2,3,4,5] -- [1,2,3] % returns: [4,5]
[2,4,2] -- [2,4,2] % produced []
```
- Both `++` and `--` are right associative. This means the elements of multiple -- or ++ operations will be done right to left:
```erlang
[1,2,3] -- [1,2] -- [2]. % we get: [1,2,3] -- [1]. Then: [2,3].
[1,2,3] -- [1,2] -- [3]. % outcome: [1,2,3] -- [1,2]. Then: [3].
```

- It is so common for operations in  erlang to work with a head of a list first, this is why it provides an easier way to separate the head from the tail of a list:
```erlang
List = [1,2,3,4].
[1,2,3,4]
[Head | Tail] = List.
% Head -> 1, Tail -> [2,3,4]
```
- `|` operator is called the **cons operator** (constructor). Lists can be build using only cons operators and values: `[3 | [2 | [1 | []]]]` will result in list [3,2,1] being constructed.
Another example to demonstrate this:
```erlang
[a, b, c, d]
[a, b, c, d | []]
[a, b | [c, d]]
[a, b | [c | [d]]]
[a | [b | [c | [d]]]]
[a | [b | [c | [d | []]]]]
```
All of the above result in the same list being constructed: `[a, b, c, d]`
- The form `[1 | 2]` gives what is called an improper list.
- Proper lists end with an empty list as their last cell.

### Tuples
- A tuple is a way of grouping together a set number of terms.
- It is written in a form of `{Element1, Element2, ...}`.
An example of a tuple in action:
```erlang
X = 10, Y = 2.
Point = {X, Y}.
Point. % -> {10, 2}
```
- Point in this case is made up of two terms.
- To unpack or retrieve a specific value from a tuple we can use pattern matching, let's say the value of X:
```erlang
{Num1, _} = Point.
Num1. % is now bound to 10
```
- Pattern matching to unpack a tuple will only work if the number of elements is the same. For example this would not work: `{X} = Point.`.
- A tuple containing an atom with an element following it is called a *tagged tuple*. Example: `{phone, {"S8", 799}}.

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

### Bit / binary data
- Erlang bit syntax encloses binary data between `<<` and `>>` and splits it into readable segments.
- Each segment is comma separated.
- A segment is a sequence of bits of a binary.
- Pattern matching is also possible on bit strings.
- We can even specify how many bits a variable will hold: `<<R:8, G:8, B:8>> = <<Pix1:24>>.`.
- We don't have to unpack all values at once:
```erlang
Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.
<<R:8, Rest/binary>> = Pixels.
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
- `\_` / underscore is the _don't care variable_. It does not store the value that would usually be placed there.
- `\_` is always seen as unbound and acts as a wildcard for pattern matching.
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

### Built-in functions (aka __BIFs__)
- BIFs are usually functions that could not be implemented in pure Erlang, and hence are defined in C.
- Some BIFs which could have been implemented in erlang were still implemented in C to provide more speed to common operations. `length(List)` is one example.

### Higher-order functions (aka __funs__)
- Functions that return functions or take other functions as an argument.
- Functions that manipulate functions.
- 
- Example fun:

```erlang
Double = fun(X) -> X * 2.
Double(2) % returns 4
```
## List Comprehensions
- List comprehensions are ways to build and modify lists.
- They can also make programs shorter and easier to understand compared to other ways of manipulating lists.
- They are based on the mathematical idea of `set notation`. Like set notation, list comprehensions are about building sets from other sets.
- For example, given the set `{2n : n <- L}`, where L is the list `[1,2,3]` this would read as "for all n values in [1,2,3], give me n*2". The set build from this would be [2,4,6].
- In erlang this would look as follows: `[ N\*2 || N <- [1,2,3]].` This ouputs `[2,4,6]`.
- Each number gets pattern matched to N and then an operation N * 2 is carried out on every N element from the list.
- Another thing we can do in a list comprehension is to add a constraint by using operators that return Boolean values:
```erlang
[X || X <- [1,2,3,4], X rem 2 =:= 0]. 
```
- The part after the `,` perform the Boolean expression and if it returns true the item X is returned, otherwise it is ignored.
- The output in this case is: `[2,4]`.
- The Pattern <- List part is called a `generator expression`.
- We can use more than one generator expression: `[X + Y || X <- [1,2], Y <- [3,4]].`
- It is also possible to combine the generator expression with pattern matching which can act as a filter `[X || {circle, X} <- List]`

## Accumulators
