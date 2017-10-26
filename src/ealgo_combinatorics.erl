-module(ealgo_combinatorics).
-include("ealgo.hrl").
-export([cartesian_product/2, cartesian_product/1]).


%% https://en.wikipedia.org/wiki/Cartesian_product
%% Gives the cartesian product of list A and list B.
-spec cartesian_product([term()], [term()]) -> [list()].
cartesian_product(A, B) when is_list(A), is_list(B) ->
    [[X, Y] || X <- A, Y <- B].

%% Gives the cartesian product of a list of list.
-spec cartesian_product([[term()]]) -> [list()].
cartesian_product([]) ->
    [];
cartesian_product([H]) when is_list(H) ->
    [[X] || X <- H];
cartesian_product([H | T]) when is_list(H) ->
    [[X | Y] || X <- H, Y <- cartesian_product(T)].





