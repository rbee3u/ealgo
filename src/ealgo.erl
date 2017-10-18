-module(ealgo).
-include("ealgo.hrl").
-export([
      cartesian_product/1
    , combinations/1
    , combinations/2
    , permutations/1
    , permutations/2
    , next_permutation/1
    , rabin_karp/2
    , rabin_karp/3
    , sgn/1
    , boole/1
    , ustep/1
    , id/1
    , shuffle/1
    , select_by_weight/1
    , select_amount_by_weight/2
    , get_rand_elem/1
    , get_rand_elems/2
]).


%% https://en.wikipedia.org/wiki/Cartesian_product
%% Gives the cartesian product of a list of list.
-spec cartesian_product(L :: [[term()]]) ->
    R :: [[term()]].
cartesian_product([     ]) -> [[]];
cartesian_product([A | T]) ->
    B = cartesian_product(T),
    [[X | Y] || X <- A, Y <- B].


%% Gives all combinations of L.
-spec combinations(L :: [term()]) ->
    R :: [[term()]].
combinations([     ]) -> [[]];
combinations([H | T]) ->
    A = B = combinations(T),
    A ++ [[H | Y] || Y <- B].


%% Gives all combinations of L containing exactly N elements.
-spec combinations(L :: [term()], N :: non_neg_integer()) ->
    R :: [[term()]].
combinations(L, N) when not is_list(L)
                      ; not is_integer(N)
                      ; not (N >= 0) ->
    erlang:error(badarg);
combinations(   _   , 0) -> [[]];
combinations([     ], _) -> [  ];
combinations([H | T], N) ->
    A = combinations(T, N - 1),
    B = combinations(T, N    ),
    [[H | X] || X <- A] ++ B.


%% Gives all permutations of L.
-spec permutations(L :: [term()]) ->
    R :: [[term()]].
permutations([]) -> [[]];
permutations(L) when is_list(L) ->
    permutations_h1(L, []).

permutations_h1([     ], _) -> [];
permutations_h1([H | T], C) ->
    A = permutations(lists:reverse(C, T)),
    B = permutations_h1(T, [H | C]),
    [[H | X] || X <- A] ++ B.


%% Gives all permutations of L containing exactly N elements.
-spec permutations(L :: [term()], N :: non_neg_integer()) ->
    R :: [[term()]].
permutations(L, N) when not is_list(L)
                      ; not is_integer(N)
                      ; not (N >= 0) ->
    erlang:error(badarg);
permutations( _ , 0) -> [[]];
permutations([ ], _) -> [  ];
permutations( L , N) ->
    permutations_h2(L, N, []).

permutations_h2([     ], _, _) -> [];
permutations_h2([H | T], N, C) ->
    A = permutations(lists:reverse(C, T), N - 1),
    B = permutations_h2(T, N, [H | C]),
    [[H | X] || X <- A] ++ B.


%% Gives the next permutation of L.
-spec next_permutation(L :: [term()]) ->
    {true, R :: [term()]} | false.
next_permutation(L) when is_list(L) ->
    next_permutation_h1(lists:reverse(L), []).

next_permutation_h1([J, I | A], B) when I < J ->
    S = next_permutation_h2([I | lists:reverse(B, [J])], []),
    {true, lists:reverse(A, S)};
next_permutation_h1([H | A], B) ->
    next_permutation_h1(A, [H | B]);
next_permutation_h1(_, _) -> false.

next_permutation_h2([I, J | A], B) when I < J ->
    [J | lists:reverse(B, [I | A])];
next_permutation_h2([I, H | A], B) ->
    next_permutation_h2([I | A], [H | B]).


%% https://en.wikipedia.org/wiki/Rabin%E2%80%93Karp_algorithm
%% Gives a list of the starting character positions at which
%% Pattern appears as a substring of String.
-spec rabin_karp(String :: string(), Pattern :: string()) ->
    PositionList :: [pos_integer()].
rabin_karp(String, Pattern) when is_list(String), is_list(Pattern) ->
    rabin_karp(String, Pattern, -1).


%% Includes the first N occurrences of Pattern, all if N < 0.
-spec rabin_karp(String :: string(), Pattern :: string(), N :: integer()) ->
    PositionList :: [pos_integer()].
rabin_karp(String, Pattern, N) when  length(String) < length(Pattern), is_integer(N) ->
    [];
rabin_karp(String, Pattern, N) when is_list(String), is_list(Pattern), is_integer(N) ->
    MSB = ealgo_ntl:power_mod(Base = 257, PatSize = length(Pattern), Modu = 1299709),
    Iterate = fun(Hash, Out, In) ->
        ealgo_ntl:remainder((Hash * Base) - (Out * MSB) + In, Modu)
    end,
    PatHash = lists:foldl(fun(X, Acc) -> Iterate(Acc, 0, X) end, 0, Pattern),
    Accept = fun(OutList, WinHash) ->
        (PatHash =:= WinHash) andalso lists:prefix(Pattern, OutList)
    end,
    {Window, InList} = lists:split(PatSize, OutList = String),
    WinHash = lists:foldl(fun(X, Acc) -> Iterate(Acc, 0, X) end, 0, Window),
    rabin_karp_h1(Iterate, Accept, 1, OutList, WinHash, Accept(String, WinHash), InList, N, []).

rabin_karp_h1(_, _,   _, _, _,     _,  _, 0, Acc) -> lists:reverse(Acc, [   ]);
rabin_karp_h1(_, _,   _, _, _, false, [], _, Acc) -> lists:reverse(Acc, [   ]);
rabin_karp_h1(_, _, Pos, _, _,  true, [], _, Acc) -> lists:reverse(Acc, [Pos]);
rabin_karp_h1(Iterate, Accept, Pos, [Out | OutList2], WinHash, IsMatch, [In | InList2], N, Acc) ->
    WinHash2 = Iterate(WinHash, Out, In), IsMatch2 = Accept(OutList2, WinHash2),
    case IsMatch of
        false -> Pos2 = Pos + 1, N2 = N - 0, Acc2 =        Acc;
        true  -> Pos2 = Pos + 1, N2 = N - 1, Acc2 = [Pos | Acc]
    end,
    rabin_karp_h1(Iterate, Accept, Pos2, OutList2, WinHash2, IsMatch2, InList2, N2, Acc2).


%% https://en.wikipedia.org/wiki/Sign_function
%% Gives -1, 0, or 1 depending on whether X is negative, zero, or positive.
-spec sgn(X :: number()) ->
    R :: -1 | 0 | 1.
sgn(X) when not is_integer(X)
          , not is_float(X) ->
    erlang:error(badarg);
sgn(X) when X > 0 ->  1;
sgn(X) when X < 0 -> -1;
sgn(_)            ->  0.


%% https://en.wikipedia.org/wiki/Indicator_function
%% Yields 1 if X is true and 0 if it is false.
-spec boole(X :: boolean()) ->
    R :: 0 | 1.
boole( true) -> 1;
boole(false) -> 0.


%% https://en.wikipedia.org/wiki/Heaviside_step_function
%% Represents the unit step function, equal to 0
%% for X < 0 and 1 for X >= 0.
-spec ustep(X :: number()) ->
    R :: 0 | 1.
ustep(X) when not is_integer(X)
            , not is_float(X) ->
    erlang:error(badarg);
ustep(X) when X >= 0 -> 1;
ustep(_)             -> 0.


%% https://en.wikipedia.org/wiki/Identity_function
%% Gives X (the identity operation).
-spec id(X :: term()) ->
    R :: term().
id(X) -> X.


-spec shuffle(L :: list()) ->
    R :: any().
shuffle(L) ->
    L2 = [{rand:uniform(100), Item} || Item <- L],
    [ShuffledItem || {_, ShuffledItem} <- lists:sort(L2)].

-type key() :: any().
-type weight() :: integer.
-type weight_item() :: [{key(), weight()}].
-spec select_by_weight(List :: list(weight_item())) ->
    R :: {ok, key()} | {error, _}.
select_by_weight(List) ->
    AllWeight = lists:sum([Weight || {_Item, Weight} <- List, Weight >= 0]),
    case AllWeight > 0 of
        true ->
            RandomWeight = rand:uniform(AllWeight),
            select_by_weight(List, RandomWeight);
        false ->
            {error, <<"AllWeightMustGreaterThanZero">>}
    end.

select_by_weight(List, RandomWeight) ->
    [{Item, Weight} | RestList] = List,
    if
        RandomWeight =< Weight ->
            {ok, Item};
        RandomWeight > Weight ->
            select_by_weight(RestList, RandomWeight - Weight)
    end.


-spec select_amount_by_weight(List :: list(weight_item()), Amount :: integer) ->
    R :: {ok, list(any)} | {error, _}.
select_amount_by_weight(_List, Amount) when Amount =< 0 ->
    {error, <<"AmountMustGreaterThanZero">>};
select_amount_by_weight(List, Amount) when length(List) == Amount  ->
    SelectedKeys = [Key || {Key, _} <- List],
    {ok, SelectedKeys};
select_amount_by_weight(List, Amount) when length(List) < Amount ->
    {error, <<"ListLengthMustGreaterThanAmount">>};
select_amount_by_weight(List, Amount) ->
    select_amount_by_weight(List, Amount, []).

select_amount_by_weight(_List, 0, SelectedKeys) ->
    SelectedKeys;
select_amount_by_weight([], _Amount, SelectedKeys) ->
    SelectedKeys;
select_amount_by_weight(List, Amount, SelectedKeys) ->
    {ok, SelectedKey} = select_by_weight(List),
    SelectedList = lists:keydelete(SelectedKey, 1, List),
    select_amount_by_weight(SelectedList, Amount-1, [SelectedKey | SelectedKeys]).


-spec get_rand_elem(List :: list()) ->
    R :: undefined | any().
get_rand_elem([]) ->
    undefined;
get_rand_elem(List) ->
    {ok, [Elem]} = get_rand_elems(List, 1),
    Elem.


-spec get_rand_elems(List :: list(), Amount :: integer()) ->
    R :: {ok, list()} | {error, _}.
get_rand_elems([], _Amount) ->
    {error, <<"ListEmpty">>};
get_rand_elems(_List, Amount) when Amount =< 0 ->
    {error, <<"ArgAmountIllegal">>};
get_rand_elems(List, Amount) when length(List) == Amount ->
    {ok, List};
get_rand_elems(List, Amount) when length(List) < Amount ->
    {error, <<"NoEnoughListElems">>};
get_rand_elems(List, Amount) ->
    get_rand_elems(Amount, List, length(List), [], 0).

get_rand_elems(_Amount, [], _ListLen, Elems, _ElemsAmount) ->
    {ok, Elems};
get_rand_elems(Amount, _List, _ListLen, Elems, ElemsAmount) when ElemsAmount >= Amount ->
    {ok, Elems};
get_rand_elems(Amount, List, ListLen, Elems, ElemsAmount) ->
    Index = rand:uniform(ListLen),
    Elem = lists:nth(Index, List),
    RestList = lists:delete(Elem, List),
    NewElems = [Elem | Elems],
    get_rand_elems(Amount, RestList, ListLen-1, NewElems, ElemsAmount+1).
