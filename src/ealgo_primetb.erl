-module(ealgo_primetb).
-export([new/1, list/1]).
-export([get/2, size/1]).
-define(MAXM, 16#10000).


-export_type([pt/0]).
-type pt() :: {Size :: non_neg_integer()
                     , Grid :: tuple()}.


-spec new(N :: pos_integer()) ->
    PT :: pt().
new(N) when is_integer(N), N > 0 ->
    Size = erlang:length(L = list(N)),
    {Size, list_to_tuple(split(Size, L))}.


-spec list(N :: pos_integer()) ->
    PL :: [pos_integer()].
list(N) when is_integer(N), N > 0 ->
    sieve(N, lists:seq(2, N)).


-spec get(Idx :: pos_integer(), PT :: pt()) ->
    PrimeNumber :: pos_integer().
get(Idx, {Size, Grid}) when is_integer(Idx)
                    , Idx > 0, Idx =< Size ->
    Row = (Idx - 1) div ?MAXM + 1,
    Col = (Idx - 1) rem ?MAXM + 1,
    element(Col, element(Row, Grid)).


-spec size(PT :: pt()) ->
    Size :: non_neg_integer().
size({Size, _Grid}) ->
    Size.


split(Rest, L) when Rest =< ?MAXM ->
    [erlang:list_to_tuple(L)];
split(Rest, L) ->
    {L1, L2} = lists:split(?MAXM, L),
    [erlang:list_to_tuple(L1)
            | split(Rest - ?MAXM, L2)].


sieve(N, [H | T]) when H * H =< N ->
    [H | sieve(N, [X || X <- T
                , X rem H /= 0])];
sieve(_N, L) -> L.

