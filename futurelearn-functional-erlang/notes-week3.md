# FutureLearn - Functional Programming with Erlang
## Week 3 - Notes:

### Higher Order Functions
- Functions which return or take a function as an argument.

### Map, filter, and fold/reduce functions
- **reduce**(or **fold** (in erlang terms: **foldr**)) all elements into one (e.g. using + to give the sum of list).
- **map** all elements of a list (e.g. using area to give the area of each of the shapes).
- **filter** all the elements of a list with a particular property (e.g. select the circles from a list of the shapes).

### Generic functions
- Why define generic functions?
    - Once we define a generic function we can reuse forever.
    - it also gets easier to understand the code, once we see a definition of a function, for example map function, we know exactly what it is doing.

### Partially applied functions
- Representiation of functions, where a multi-argument function takes one argument at time, this is called **Curried** representation.
- This is is honour of Haskell B. Curry, the lambda calculus pioneer, after whom the Haskell language is also named.
- A function takes one argument which then returns a function which takes another argument.

Example:
```erlang
add(X) ->
    fun(Y) -> X + Y end.
```

### Composing functions
- Often function composition is used to define functions.
- First apply this function, then apply a different function.
```erlang
compose(F, G) ->
    fun(X) -> G(F(X)) end.
```
- The result of compose is a function, that *composes* its arguments.
