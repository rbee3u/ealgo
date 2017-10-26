-module(ealgo_combinatorics).
-include("ealgo.hrl").
-export([cartesian_product/2, cartesian_product/1, cartesian_power/2]).


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


