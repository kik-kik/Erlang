-module(week2_exercises).
-export([take/2, take_bif/2, take_split_modified/2]).
-export([nub/1, bun/1]).
-export([palindrome/1, palindrome_vid/1, palim/1, shunt/2, reverse_shunt/1]).
-export([join/2, another_join/2, more_join/2]).
-export([is_member/2, is_mem/2]).
-export([merge_sort/1, quick_sort/1, insertion_sort/1]).
-export([perms/1]).

%%%%%%%%%%%%%%%%%%%%%
%% Example #1: take
%%%%%%%%%%%%%%%%%%%%%
-spec take(integer(), [T]) -> [T].
take(0, _List) ->
    [];
take(_, []) ->
    [];
take(N, [Head | Tail]) when N > 0 ->
    [ Head | take(N-1, Tail) ].

%% We can also use BIF lists:split to achieve a similar result:
take_bif(Chars, List) ->
    {Front, _Back} = lists:split(Chars, List),
    Front.

%% Modified version of lists:split function:
take_split_modified(N, List) ->
    take_split_modified(N, List, []).
take_split_modified(0, _L, R) ->
    lists:reverse(R, []);
take_split_modified(N, [H|T], R) ->
    take_split_modified(N-1, T, [H|R]);
take_split_modified(_, [], R) ->
    lists:reverse(R, []).

%%%%%%%%%%%%%%%%%%%%%
%% Example #2: nub function (returns a list of numbers without number replication)
%%%%%%%%%%%%%%%%%%%%%
%% first occurance:
nub([]) ->
    [];
nub([Item | Items]) ->
    [ Item | nub(remove_all_items(Item, Items)) ].

remove_all_items(_Item, []) ->
    [];
remove_all_items(Item, [Item | Items]) ->
    remove_all_items(Item, Items);
remove_all_items(Item, [NextItem | Items]) ->
    [NextItem | remove_all_items(Item, Items)].

%% last occurance of an element:
bun([]) ->
    [];
bun([Item | Items]) ->
    case member(Item, Items) of
        true -> bun(Items);
        false -> [Item | bun(Items)]
    end.

%% lists:member rewritten:
member(_, []) ->
    false;
member(Item, [Item | _Items]) ->
    true;
member(Item, [_NewItem | Items]) ->
    member(Item, Items).

%%%%%%%%%%%%%%%%%%%%%
%% Example #3: the palindrome function
%%%%%%%%%%%%%%%%%%%%%
palindrome(String) ->
    FormattedString = re:replace(string:to_lower(String), "[^a-z0-9]", "", [global,{return,list}]),
    ReversedString = lists:reverse(FormattedString),
    FormattedString =:= ReversedString.

%%%%%%
%% Video solution to palindrome problem:
%%%%%%
palindrome_vid(String) ->
    palim(nocaps(nopunct(String))).

nopunct([]) ->
    [];
nopunct([Char | Chars]) ->
    case member(Char, ".,/\ ;:\t\n\'\"") of
        true -> nopunct(Chars);
        false -> [ Char | nopunct(Chars) ]
    end.

nocaps([]) ->
    [];
nocaps([Char | Chars]) ->
    [nocap(Char) | nocaps(Chars)].

nocap(Char) ->
    case Char >= $A andalso Char =< $Z of
        true -> Char + 32;
        false -> Char
    end.

% Below is literal palindrome (case and punctuation sensitive)
palim(String) ->
    String == reverse(String).

% own implementation of lists:reverse built-in-function
reverse([]) ->
    [];
reverse([Item | Items]) ->
    reverse(Items) ++ [Item].

% reverse implementation using shunt:
reverse_shunt(Items) ->
    shunt(Items, []).

% This will add the items from the first list to the start of List2.
% Items from the first list will be reversed in order, List2 items will keep their ordering.
shunt([], List2) ->
    List2;
shunt([Item | Items], List2) ->
    shunt(Items, [Item | List2]).


%%%%%%%%%%%%%%%%%%
%%% Consolidation: functions over lists
%%%%%%%%%%%%%%%%%

%%%%
% Joining lists together
%%%%
join(List1, List2) ->
    List1 ++ List2.

another_join(List1, List2) ->
    lists:flatten([List1 | List2]).

more_join(List, []) ->
    List;
more_join(List1, [Head | Tail]) ->
    more_join(List1 ++ [Head], Tail).


%%%
% Testing membership
%%%
is_member(_Item, []) ->
    false;
is_member(Item, [H | Tail]) ->
    case Item == H of
        true -> true;
        false -> is_member(Item, Tail)
    end.

% is_member using guards
is_mem(_Item, []) ->
    false;
is_mem(Item, [H | _Tail]) when Item == H ->
    true;
is_mem(Item, [_H | Tail]) ->
    is_mem(Item, Tail).


%%%
% Sorting lists
%%%
% Merge sort - split list in half, sort each half and methe the results.
merge_sort(List) ->
   {List1, List2} = lists:split(length(List) div 2, List),
    quick_sort(List1) ++ quick_sort(List2).

% Quicksort
quick_sort([]) ->
    [];
quick_sort([Pivot | Tail]) ->
    quick_sort([X || X <- Tail, X < Pivot])
    ++ [Pivot] ++
    quick_sort([X || X <- Tail, X >= Pivot]).

% Insertion sort - sort the tail, then insert the head of the list in the correct place.
insertion_sort([]) ->
    [];
insertion_sort([H | Tail]) ->
    insertion_sort(H, quick_sort(Tail)).
insertion_sort(Val, []) ->
    [Val];
insertion_sort(Val, [H | _Tail] = SortedList) when Val =< H ->
    [Val | SortedList];
insertion_sort(Val, [H | Tail]) ->
    [H | insertion_sort(Val, Tail)].


%%%
% Permutations
%%%
% Used programming erlangi book for this solution:
perms([]) ->
    [[]];
perms(List) ->
    [ [H | T] || H <- List, T <- perms(List -- [H]) ].
