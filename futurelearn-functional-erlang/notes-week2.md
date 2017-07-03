# FutureLearn - Functional Programming with Erlang
## Week 2 - Notes:

### What is a list?
- A list is a collection of elements. For example: [1,2,3,4].
- The order and multiplicity (how many times a velue is repeated) inside a list matters. For example [1,2,3,4,4] != [1,2,3,4] != [4,4,3,2,1].
- [Head | Tail] - Head is the first element of a list, Tail is a list of the remaining elements of a list.
- We can match more than just 1 value by specifying multiple variables on the left side of the |. For example: [First, Second | Rest].
- We can generate a new list using a similar technique. For example:
```erlang
List = [1, 2, 3],
List2 = [4, 5, 6],
NewList = [List | List2],
Newlist. % This will now evaluate to [1, 2, 3, 4, 5, 6].
```

### Case expresions:
- Case expression is

Example of case expressions:
```erlang
case [2,3,4] of % this case expression will return X + Y -> 2 + 3 -> 5.
 [X,Y|_] -> X+Y;
 [S] -> S;
 _ -> 0
end.

case [6] of % this case expression will return S -> 6.
 [X,Y|_] -> X+Y;
 [S] -> S;
 _ -> 0
end.

case [] of % this case expression will return match on _ and return 0.
 [X,Y|_] -> X+Y;
 [S] -> S;
 _ -> 0
end.
```
Calculating the sum of a list:
```erlang
sum([]) -> 0;
sum([H|T]) ->
    H + sum(T).
```

### More differences between Lists and Tuples:
- Lists: are constructured or deconstructed element by element:
```erlang
foo([]) -> ...;
foo([H | T]) -> ... foo(T) ... .
```
- Tuples: constructed or deconstructed all at once:
```erlang
area({circle, {X, Y}, R}) -> math:pi()*R*R; ...
```


