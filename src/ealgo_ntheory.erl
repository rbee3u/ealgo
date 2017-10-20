-module(ealgo_ntheory).
-export([quotient/2, remainder/2, quotient_remainder/2]).
-export([is_divisible/2, is_odd/1, is_even/1]).
-export([gcd/2, gcd/1, lcm/2, lcm/1]).


%% Definition of integer division:
%%      N = M * Q + R  (M ≠ 0)
%%      Q = floor(N / M)
%%      0 ≤ R < M  (when M > 0)
%%      M < R ≤ 0  (when M < 0)
%%
%% Gives the integer quotient of N and M.
-spec quotient(N :: integer(), M :: integer()) ->
    Q :: integer().
quotient(N, M) when is_integer(N)
                  , is_integer(M)
                  , M =/= 0 ->
    (N - remainder(N, M)) div M.

%% Gives the remainder on division of N by M.
-spec remainder(N :: integer(), M :: integer()) ->
    R :: integer().
remainder(N, M) when is_integer(N)
                   , is_integer(M)
                   , M =/= 0 ->
    case N rem M of
    R when R > 0, M < 0 -> R + M;
    R when R < 0, M > 0 -> R + M;
    R                   -> R
    end.

%% Gives the quotient and remainder from division of N by M.
-spec quotient_remainder(N :: integer(), M :: integer()) ->
    {Q :: integer(), R :: integer()}.
quotient_remainder(N, M) when is_integer(N)
                            , is_integer(M)
                            , M =/= 0 ->
    R = remainder(N, M), Q = (N - R) div M,
    {Q, R}.


%% Gives true if N is divisible by M, and false if not.
-spec is_divisible(N :: integer(), M :: integer()) ->
    P :: boolean().
is_divisible(N, M) when is_integer(N)
                      , is_integer(M)
                      , M =/= 0 ->
    (N rem M) == 0.

%% Gives true if Z is an odd integer, and false otherwise.
-spec is_odd(Z :: integer()) ->
    P :: boolean().
is_odd(Z) when is_integer(Z) ->
    (Z band 1) =:= 1.

%% Gives true if Z is an even integer, and false otherwise.
-spec is_even(Z :: integer()) ->
    P :: boolean().
is_even(Z) when is_integer(Z) ->
    (Z band 1) =:= 0.


%% https://en.wikipedia.org/wiki/Greatest_common_divisor
%% Gives the greatest common divisor of N and M.
%% note: we define gcd(0, 0) = 0 for convenience.
-spec gcd(N :: integer(), M :: integer()) ->
    GCD :: integer().
gcd(N, M) when is_integer(N), is_integer(M) ->
    euclid(abs(N), abs(M)).
euclid(N, 0) -> N;
euclid(N, M) -> euclid(M, N rem M).

%% Gives the greatest common divisor of a list of integers.
%% If the list has only one integer X, it returns gcd(X, 0);
%% If the list is empty, for this corner case, it returns 0.
-spec gcd(L :: [integer()]) ->
    GCD :: integer().
gcd(L) when is_list(L) -> lists:foldl(fun gcd/2, 0, L).

%% https://en.wikipedia.org/wiki/Least_common_multiple
%% Gives the least common multiple of N and M.
%% note: we define lcm(X, Y) = 0 (when X = 0 or y = 0).
-spec lcm(N :: integer(), M :: integer()) ->
    LCM :: integer().
lcm(N, M) when is_integer(N), is_integer(M) ->
    if N =:= 0; M =:= 0 -> 0;
    true ->
        abs(N * M) div gcd(N, M)
    end.

%% Gives the least common multiple of a list of integers.
%% If the list has only one integer X, it returns lcm(X, 1);
%% If the list is empty, for this corner case, it returns 1.
-spec lcm(L :: [integer()]) ->
    LCM :: integer().
lcm(L) when is_list(L) -> lists:foldl(fun lcm/2, 1, L).

