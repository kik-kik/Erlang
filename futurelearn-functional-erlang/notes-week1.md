# FutureLearn - Functional Programming with Erlang
## Week 1 - Notes:

## An Introduction to Erlang:
### Origins:
Developed by Ericsson researchers:
- Joe Armstrong
- Mike Williams
- Robert Virding

The goal was to develop a language which would enable them to build complex hardware and software systems (like telocom switches). Systems which lived in a highly concurrent environments where they could spawn hundreds of thousands of threads, while remaining failure tolerant. They also wanted to enable hot code upgrade, meaning modifying code in a running system without the need to take down the whole system.

Their philosophy was `let it fail` and instead of expecting components to deal with their own failure build systems which handle component failure. So they made a new language with that in mind.

Key features of erlang:
- scalable
- robust
- highly available
- fault-tolerant
- hot code changes
- concurrent
- functional language

An example of a system where erlang is in use is WhatsApp. Erlang allows for millions of simultaneous messages to be processed.

### Erlang and functional programming:
- Erlang is a functional language, it evaluates expressions and returns the result. We only have values and not things like references, pointers, etc.
- Functional programming also has no side effects, objects are immutable (their state never changes).
- However, erlang does allow some side-effects (e.g. communication which is a side effect of an evaluation).
- Other examples of functional languages are: Haskell, Miranda, ML, OCaml, F#
- There are no for loops in erlang.

- An example of a function as expression: `(fun(X) -> X + X end)(99)` (this results in value ____198_ being produced). This is also known as a ___`_lambda_` (anonymous function).

## Programs in Erlang:
- .erl file contains erlang source code.
- .beam file contains compiled erlang code.

### 6 values in erlang:
- numbers (integers and floats)
- atoms (a piece of data which just signifies itself)
- booleans (two special atoms true and false)
- tuples and lists (with lists we can iterate over its elements, we cannot do that with a tuple).
- strings (list of characters )
- functions (values themselves, can be arguments and values of functions too - functions are first class citizens).

### Variables:
- In erlang if a variable does not have a value then it is `not bound`.
- `=` / pattern match operator is used to compare values if they are the same, if a value is compared against an unbound variable then that value gets assigned to it.
- pattern match operator checks if the value on the right hand side (RHS) is the same as the value on the left hand side (LHS).
- Once a value is bound to a variable, it can never change.i
- Variables with undersore character before them will indicate to the interpreter that we do not care about their values. Example:
```erlang
function_definition(Var1, _Var2) -> % `This prevents 'variable Var2 is unused' warning message on compilation.
    Var1 + Var1.
```

### Pattern matching
- Pattern matching is used to determine which function clause should be executed. Matching is done on the functions head (name and the arity).
- Function which is matched first is the one that is executed, this is why the order in which functions are declared is important.
- `_` - `underscore` is a special character which will match anything.

For example:
```erlang
is_zero(0) ->
    true;
is_zero(_) ->
    false.

is_zero(5). % will match the second is_zero function definition.
```
- Guards are part of pattern matching. for example `is_more_than_10(N) when N > 10 -> true.`
- In this case  is_more_than_10(N) function will only match if the value passed is greated than 10.

### Recursion
There are different types of recursion:
    - Direct recursion - keep current result and keep going down the stack recursively to get all the values, then we walk back the stack and add the values 1 by 1.
    - Tail recursion - pass all required values to the next function call and previous stack call can be discarded as the new one has all the essential informations for its computation.



