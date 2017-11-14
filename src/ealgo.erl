-module(ealgo).
-include("ealgo.hrl").
-export([sign/1, boole/1, unit_step/1]).
-export([cartesian_product/1]).
-export([combinations/1, combinations/2]).
-export([next_permutation/1, permutations/1]).


%% Gives -1, 0, or 1 depending on whether X is negative, zero, or positive.
-spec sign(X :: number()) ->
    S :: -1 | 0 | 1.
sign(X) when is_integer(X); is_float(X) ->
    if
        X < 0 -> -1;
        X > 0 ->  1;
        true  ->  0
    end.


%% Yields 1 if Expr is true and 0 if it is false.
-spec boole(Expr :: boolean()) ->
    R :: 0 | 1.
boole(true) -> 1;
boole(false) -> 0.


%% Represents the unit step function, equal to 0
%% for X < 0 and 1 for X >= 0.
-spec unit_step(X :: number()) ->
    R :: 0 | 1.
unit_step(X) when is_integer(X); is_float(X) ->
    if
        X >= 0 -> 1;
        true   -> 0
    end.


%% https://en.wikipedia.org/wiki/Cartesian_product
%% Gives the cartesian product of a list of list.
-spec cartesian_product(L :: [list()]) ->
    R :: [list()].
cartesian_product([     ]) -> [[]];
cartesian_product([A | T]) ->
    B = cartesian_product(T),
    [[X | Y] || X <- A, Y <- B].


%% Gives all combinations of L. 
-spec combinations(L :: [term()]) ->
    R :: [list()].
combinations([     ]) -> [[]];
combinations([H | T]) ->
    A = B = combinations(T),
    A ++ [[H | Y] || Y <- B].


%% Gives all combinations of L containing exactly N elements. 
-spec combinations(L :: [term()], N :: non_neg_integer()) ->
    R :: [list()].
combinations([     ], _) -> [[]];
combinations(   _   , 0) -> [[]];
combinations([H | T], N) when N > 0 ->
    A = combinations(T, N    ),
    B = combinations(T, N - 1),
    A ++ [[H | Y] || Y <- B].



%% Gives the next permutation of L.
-spec next_permutation(L :: [term()]) ->
    {true, R :: [term()]} | false.
next_permutation(L) when is_list(L) ->
    case next_p_split(lists:reverse(L), []) of
    {A, I, B} ->
        {true, A ++ next_p_swap(I, B, [])};
    false -> false
    end.
next_p_split([P, I | T], B) when I < P ->
    {lists:reverse(T), I, lists:reverse(B, [P])};
next_p_split([H | T], B) -> next_p_split(T, [H | B]);
next_p_split(_, _) -> false.
next_p_swap(I, [J | T], C) when I < J ->
    [J | lists:reverse(C, [I | T])];
next_p_swap(I, [H | T], C) -> next_p_swap(I, T, [H | C]).


%% generates a list of all possible permutations of the elements in L.
-spec permutations(L :: [term()]) ->
    R :: [list()].
permutations(L) when is_list(L) ->
    permutations_helper({true, lists:sort(L)}, []).
permutations_helper(false, Acc) ->
    lists:reverse(Acc);
permutations_helper({true, L}, Acc) ->
    permutations_helper(next_permutation(L), [L | Acc]).















