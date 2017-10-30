-module(ealgo_combinatorics).
-include("ealgo.hrl").
-export([cartesian_product/2, cartesian_product/1, cartesian_power/2]).
-export([subsets/2, subsets/1, next_permutation/1, permutations/1]).


%% https://en.wikipedia.org/wiki/Cartesian_product
%% Gives the cartesian product of list A and list B.
-spec cartesian_product(A :: [term()], B :: [term()]) ->
    R :: [list()].
cartesian_product(A, B) when is_list(A), is_list(B) ->
    [[X, Y] || X <- A, Y <- B].

%% Gives the cartesian product of a list of list.
-spec cartesian_product(LL :: [L :: [term()]]) ->
    R :: [list()].
cartesian_product([]) ->
    [];
cartesian_product([H]) when is_list(H) ->
    [[X] || X <- H];
cartesian_product([H | T]) when is_list(H) ->
    [[X | Y] || X <- H, Y <- cartesian_product(T)].

%% Gives the N'th cartesian power of list L.
-spec cartesian_power(L :: [term()], N :: non_neg_integer()) ->
    R :: [list()].
cartesian_power(L, N) when is_list(L), is_integer(N), ?GTE(N) ->
    cartesian_product(lists:duplicate(N, L)).


%% Gives all subsets of L containing exactly N elements. 
-spec subsets(L :: [term()], N :: non_neg_integer()) ->
    R :: [list()].
subsets(L, N) when is_list(L), is_integer(N), ?GTE(N) ->
    case erlang:length(L) of
        R when R > N -> [];
        R -> subsets(L, N, R)
    end.
subsets(_, 0, _) -> [[ ]];
subsets(L, N, N) -> [ L ];
subsets([H | T], N, R) ->
    A = subsets(T, N - 1, R - 1),
    B = subsets(T, N    , R - 1),
    [[H | E] || E <- A] ++ B.

%% Gives all subsets of L. 
-spec subsets(L :: [term()]) ->
    R :: [list()].
subsets(L) when is_list(L) ->
    lists:append(lists:map(fun(N) ->
        subsets(L, N)
    end, lists:seq(0, erlang:length(L)))).

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

