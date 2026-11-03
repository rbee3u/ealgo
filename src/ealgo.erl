-module(ealgo).
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
%% Returns the Cartesian product of the input lists. An empty input returns
%% [[]]; any empty input list makes the product empty.
-spec cartesian_product(L :: [[term()]]) ->
    R :: [[term()]].
cartesian_product([     ]) -> [[]];
cartesian_product([Choices | Rest]) ->
    TailProducts = cartesian_product(Rest),
    [[Choice | Product] || Choice <- Choices, Product <- TailProducts].


%% Returns all combinations of L, preserving input order within each result.
%% Positions are distinct, so repeated values can produce duplicate results.
-spec combinations(L :: [term()]) ->
    R :: [[term()]].
combinations([     ]) -> [[]];
combinations([H | T]) ->
    TailCombinations = combinations(T),
    TailCombinations ++ [[H | Combination] || Combination <- TailCombinations].


%% Returns combinations of L with exactly N elements, preserving input order.
%% Positions are distinct. N = 0 returns [[]]; N > length(L) returns [].
-spec combinations(L :: [term()], N :: non_neg_integer()) ->
    R :: [[term()]].
combinations(L, N) when not is_list(L)
                      ; not is_integer(N)
                      ; N < 0 ->
    erlang:error(badarg);
combinations(   _   , 0) -> [[]];
combinations(L, N) ->
    combinations(L, N, length(L)).

combinations(_, N, Remaining) when N > Remaining -> [];
combinations(_, 0, _) -> [[]];
combinations(L, N, N) -> [L];
combinations([H | T], N, Remaining) ->
    TailCombinations = combinations(T, N - 1, Remaining - 1),
    WithoutHead = combinations(T, N, Remaining - 1),
    [[H | Combination] || Combination <- TailCombinations] ++ WithoutHead.


%% Returns all permutations of L. Positions are distinct, so repeated values
%% can produce duplicate results. The empty list returns [[]].
-spec permutations(L :: [term()]) ->
    R :: [[term()]].
permutations([]) -> [[]];
permutations(L) when is_list(L) ->
    permutations_from_choices(L, []).

permutations_from_choices([], _ReversedSkipped) -> [];
permutations_from_choices([H | T], ReversedSkipped) ->
    RemainingChoices = lists:reverse(ReversedSkipped, T),
    RemainingPermutations = permutations(RemainingChoices),
    OtherPermutations = permutations_from_choices(T, [H | ReversedSkipped]),
    [[H | Permutation] || Permutation <- RemainingPermutations] ++ OtherPermutations.


%% Returns permutations of L with exactly N elements, treating positions as
%% distinct. N = 0 returns [[]]; N > length(L) returns [].
-spec permutations(L :: [term()], N :: non_neg_integer()) ->
    R :: [[term()]].
permutations(L, N) when not is_list(L)
                      ; not is_integer(N)
                      ; N < 0 ->
    erlang:error(badarg);
permutations( _ , 0) -> [[]];
permutations( L , 1) -> [[X] || X <- L];
permutations([ ], _) -> [  ];
permutations( L , N) ->
    permutations_from_choices(L, N, []).

permutations_from_choices([], _N, _ReversedSkipped) -> [];
permutations_from_choices([H | T], N, ReversedSkipped) ->
    RemainingChoices = lists:reverse(ReversedSkipped, T),
    RemainingPermutations = permutations(RemainingChoices, N - 1),
    OtherPermutations = permutations_from_choices(T, N, [H | ReversedSkipped]),
    [[H | Permutation] || Permutation <- RemainingPermutations] ++ OtherPermutations.


%% Returns {true, Next} for the next lexicographic permutation under Erlang
%% term ordering, or false when no successor exists.
-spec next_permutation(L :: [term()]) ->
    {true, R :: [term()]} | false.
next_permutation(L) when is_list(L) ->
    next_permutation_find_pivot(lists:reverse(L), []).

next_permutation_find_pivot([Next, Pivot | ReversedPrefix], DescendingSuffix) when Pivot < Next ->
    AscendingSuffix = lists:reverse(DescendingSuffix, [Next]),
    NextPivotAndSuffix = next_permutation_swap_pivot(Pivot, AscendingSuffix, []),
    {true, lists:reverse(ReversedPrefix, NextPivotAndSuffix)};
next_permutation_find_pivot([H | ReversedPrefix], DescendingSuffix) ->
    next_permutation_find_pivot(ReversedPrefix, [H | DescendingSuffix]);
next_permutation_find_pivot(_, _) -> false.

next_permutation_swap_pivot(Pivot, [Successor | Rest], ReversedSkipped) when Pivot < Successor ->
    [Successor | lists:reverse(ReversedSkipped, [Pivot | Rest])];
next_permutation_swap_pivot(Pivot, [H | Rest], ReversedSkipped) ->
    next_permutation_swap_pivot(Pivot, Rest, [H | ReversedSkipped]).


%% https://en.wikipedia.org/wiki/Rabin%E2%80%93Karp_algorithm
%% Returns one-based match positions in ascending order, including overlaps.
%% Positions count character-list elements. An empty pattern matches at every
%% position from 1 through length(String) + 1.
-spec rabin_karp(String :: string(), Pattern :: string()) ->
    PositionList :: [pos_integer()].
rabin_karp(String, Pattern) when is_list(String), is_list(Pattern) ->
    rabin_karp(String, Pattern, -1).


%% As rabin_karp/2, but returns at most N matches. N < 0 returns all matches;
%% N = 0 returns [].
-spec rabin_karp(String :: string(), Pattern :: string(), N :: integer()) ->
    PositionList :: [pos_integer()].
rabin_karp(String, Pattern, N) when is_list(String), is_list(Pattern), is_integer(N) ->
    Base = 257,
    PatternSize = length(Pattern),
    Modulus = 1299709,
    OutgoingFactor = ealgo_ntl:power_mod(Base, PatternSize, Modulus),
    RollHash = fun(Hash, Out, In) ->
        ealgo_ntl:remainder((Hash * Base) - (Out * OutgoingFactor) + In, Modulus)
    end,
    PatternHash = lists:foldl(fun(X, Acc) -> RollHash(Acc, 0, X) end, 0, Pattern),
    Matches = fun(WindowStart, WindowHash) ->
        (PatternHash =:= WindowHash) andalso lists:prefix(Pattern, WindowStart)
    end,
    case rabin_karp_window(String, PatternSize, RollHash, 0) of
        false -> [];
        {WindowHash, Unread} ->
            rabin_karp_scan(RollHash, Matches, 1, String, WindowHash,
                            Unread, N, [])
    end.

%% Build the initial hash without scanning the unused text or copying a window.
rabin_karp_window(Unread, 0, _RollHash, WindowHash) -> {WindowHash, Unread};
rabin_karp_window([], _Size, _RollHash, _WindowHash) -> false;
rabin_karp_window([H | T], Size, RollHash, WindowHash) ->
    rabin_karp_window(T, Size - 1, RollHash, RollHash(WindowHash, 0, H)).

rabin_karp_scan(_, _, _, _, _, _, 0, ReversedPositions) -> lists:reverse(ReversedPositions);
rabin_karp_scan(RollHash, Matches, Pos, WindowStart, WindowHash,
                Unread, Remaining, ReversedPositions) ->
    {NextRemaining, NextReversedPositions} = case Matches(WindowStart, WindowHash) of
        false -> {Remaining, ReversedPositions};
        true -> {Remaining - 1, [Pos | ReversedPositions]}
    end,
    case Unread of
        [] -> lists:reverse(NextReversedPositions);
        [In | RestUnread] ->
            [Out | NextWindowStart] = WindowStart,
            NextWindowHash = RollHash(WindowHash, Out, In),
            rabin_karp_scan(RollHash, Matches, Pos + 1, NextWindowStart, NextWindowHash,
                            RestUnread, NextRemaining, NextReversedPositions)
    end.


%% https://en.wikipedia.org/wiki/Sign_function
%% Gives -1, 0, or 1 depending on whether X is negative, zero, or positive.
-spec sgn(X :: number()) ->
    R :: -1 | 0 | 1.
sgn(X) when not is_number(X) ->
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
%% Represents the unit step function, equal to 0 for X < 0 and 1 for X >= 0.
-spec ustep(X :: number()) ->
    R :: 0 | 1.
ustep(X) when not is_number(X) ->
    erlang:error(badarg);
ustep(X) when X >= 0 -> 1;
ustep(_)             -> 0.


%% https://en.wikipedia.org/wiki/Identity_function
%% Gives X (the identity operation).
-spec id(X :: term()) ->
    R :: term().
id(X) -> X.


%% Returns a uniformly shuffled list, preserving duplicate elements. Uses the
%% calling process's rand state, as do the other random selection functions.
-spec shuffle(L :: list()) ->
    R :: list().
shuffle(L) ->
    Size = length(L),
    sample_array(Size, array:from_list(L), Size, []).

%% Partial Fisher-Yates: choose an active slot and replace it with the last one.
sample_array(0, _Array, _ActiveSize, Selected) -> Selected;
sample_array(Remaining, Array, ActiveSize, Selected) ->
    Index = rand:uniform(ActiveSize) - 1,
    Elem = array:get(Index, Array),
    LastActiveElem = array:get(ActiveSize - 1, Array),
    NextArray = array:set(Index, LastActiveElem, Array),
    sample_array(Remaining - 1, NextArray, ActiveSize - 1, [Elem | Selected]).

-type key() :: any().
-type weight() :: integer().
-type weight_item() :: {key(), weight()}.
%% Returns {ok, Key} with probability proportional to its weight. The caller
%% must supply nonnegative integer weights; zero-weight entries are not drawn.
%% A zero total returns {error, <<"AllWeightMustGreaterThanZero">>}.
-spec select_by_weight(List :: [weight_item()]) ->
    R :: {ok, key()} | {error, _}.
select_by_weight(List) ->
    TotalWeight = lists:sum([Weight || {_Key, Weight} <- List, Weight >= 0]),
    case TotalWeight > 0 of
        true ->
            RandomWeight = rand:uniform(TotalWeight),
            select_by_weight(List, RandomWeight);
        false ->
            {error, <<"AllWeightMustGreaterThanZero">>}
    end.

select_by_weight(List, RandomWeight) ->
    [{Key, Weight} | RestList] = List,
    case RandomWeight =< Weight of
        true ->
            {ok, Key};
        false ->
            select_by_weight(RestList, RandomWeight - Weight)
    end.


%% Returns {ok, Keys} from weighted sampling without replacement. Supply keys
%% distinct under == (1 and 1.0 count as the same key) and nonnegative integer
%% weights. Partial sampling needs at least Amount positive-weight entries or
%% it raises a badmatch error, and returns keys in reverse draw order. Taking
%% the whole list returns keys in input order regardless of weights. Amount
%% must be a positive integer no greater than length(List); out-of-range
%% integers return {error, Reason}.
-spec select_amount_by_weight(List :: [weight_item()], Amount :: integer()) ->
    R :: {ok, [key()]} | {error, _}.
select_amount_by_weight(_List, Amount) when Amount =< 0 ->
    {error, <<"AmountMustGreaterThanZero">>};
select_amount_by_weight(List, Amount) when length(List) == Amount  ->
    SelectedKeys = [Key || {Key, _} <- List],
    {ok, SelectedKeys};
select_amount_by_weight(List, Amount) when length(List) < Amount ->
    {error, <<"ListLengthMustGreaterThanAmount">>};
select_amount_by_weight(List, Amount) ->
    {ok, select_amount_by_weight(List, Amount, [])}.

select_amount_by_weight(_List, 0, SelectedKeys) ->
    SelectedKeys;
select_amount_by_weight([], _Amount, SelectedKeys) ->
    SelectedKeys;
select_amount_by_weight(List, Amount, SelectedKeys) ->
    {ok, SelectedKey} = select_by_weight(List),
    RemainingList = lists:keydelete(SelectedKey, 1, List),
    select_amount_by_weight(RemainingList, Amount - 1, [SelectedKey | SelectedKeys]).


%% Returns a uniformly chosen element, or undefined if List is empty.
-spec get_rand_elem(List :: list()) ->
    R :: undefined | any().
get_rand_elem([]) ->
    undefined;
get_rand_elem(List) ->
    {ok, [Elem]} = get_rand_elems(List, 1),
    Elem.


%% Returns {ok, Elements} by uniformly sampling positions without replacement;
%% duplicate values can remain. Taking the whole list preserves input order.
%% Amount must be a positive integer no greater than length(List). Empty input
%% or an out-of-range integer amount returns {error, Reason}.
-spec get_rand_elems(List :: list(), Amount :: integer()) ->
    R :: {ok, list()} | {error, _}.
get_rand_elems([], _Amount) ->
    {error, <<"ListEmpty">>};
get_rand_elems(_List, Amount) when Amount =< 0 ->
    {error, <<"ArgAmountIllegal">>};
get_rand_elems([_, _ | _] = List, 1) ->
    {ok, [lists:nth(rand:uniform(length(List)), List)]};
get_rand_elems(List, Amount) when length(List) == Amount ->
    {ok, List};
get_rand_elems(List, Amount) when length(List) < Amount ->
    {error, <<"NoEnoughListElems">>};
get_rand_elems(List, Amount) ->
    Array = array:from_list(List),
    {ok, sample_array(Amount, Array, array:size(Array), [])}.
