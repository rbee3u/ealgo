-module(ealgo_ntheory).
-export([is_divisible/2, is_odd/1, is_even/1, multiplicity/2]).
-export([quotient/2, remainder/2, quotient_remainder/2]).
-export([gcd/2, gcd/1, lcm/2, lcm/1, power/2, power_mod/3]).
-export([is_coprime/2, jacobi_symbol/2]).


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

%% Gives the highest power of A that divides N.
-spec multiplicity(N :: integer(), A :: pos_integer()) ->
    Y :: non_neg_integer().
multiplicity(N, A) when is_integer(N)
                      , is_integer(A)
                      , N =/= 0
                      , A  >  1 ->
    multiplicity(N, A, 1, A, 0, 0).
multiplicity(N, A, 1, _M, _PM, Y) when (N rem A) =/= 0 ->
    Y div 2;
multiplicity(N, A, X, M, PM, Y) when (N rem M) =/= 0 ->
    multiplicity(N div PM, A, 1, A, 0, Y + X);
multiplicity(N, A, X, M, _PM, Y) ->
    multiplicity(N, A, X + X, M * M, M, Y).


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


%% Gives A to the power X where A is an integer and X is a
%% nonnegative integer.
%% note: power(0, 0) is undefined.
-spec power(A :: integer(), X :: non_neg_integer()) ->
    POWER :: integer().
power(A, 0) when is_integer(A)
               , A =/= 0 -> 1;
power(0, X) when is_integer(X)
               , X  >  0 -> 0;
power(A, X) when is_integer(A)
               , is_integer(X)
               , X  >  0 ->
    B = power(A, X div 2),
    case is_even(X) of
        true -> B * B;
        _ -> A * B * B
    end.


%% Gives A ^ X mod M where A is an integer, X is a
%% nonnegative integer and M is a nonzero integer.
%% note: power_mod(0, 0, M) is undefined.
-spec power_mod(A :: integer(), X :: non_neg_integer(), M :: integer()) ->
    POWER_MOD :: integer().
power_mod(A, X, M) when is_integer(A)
                      , is_integer(X)
                      , is_integer(M)
                      , X >= 0
                      , M =/= 0 ->
    if
    X =:= 0, A =/= 0 -> remainder(1, M);
    X  >  0, A =:= 0 -> 0;
    X  >  0 ->
        B = power_mod(A, X div 2, M),
        case is_even(X) of
            true -> remainder(B * B, M);
            _ -> remainder(A * B * B, M)
        end
    end.

%% https://en.wikipedia.org/wiki/Coprime_integers
%% Gives true if N and M are relatively prime, and false otherwise. 
%% relation: is_coprime(N, M) = true   <=>   gcd(N, M) = 1
-spec is_coprime(N :: integer(), M :: integer()) ->
    P :: boolean().
is_coprime(N, M) when is_integer(N), is_integer(M) ->
    gcd(N, M) =:= 1.


%% https://en.wikipedia.org/wiki/Jacobi_symbol
%% Gives the Jacobi symbol (A / N).
-spec jacobi_symbol(A :: integer(), N :: integer()) ->
    J :: -1 | 0 | 1.
jacobi_symbol(A, 1) when is_integer(A) ->
    1;
jacobi_symbol(A, N) when is_integer(A)
                       , is_integer(N)
                       , (N rem 2) > 0 ->
    B = remainder(A, N),
    case is_coprime(B, N) of
    false -> 0; true ->
        jacobi_helper(B, N, 1)
    end.
jacobi_helper(1, _N, C) ->
    C;
jacobi_helper(A, N, C) when (A rem 2) =:= 0 ->
    D = case is_even(S = multiplicity(A, 2)) of
        true -> C; false -> case (N rem 8) of
            R when R =/= 3, R =/= 5 -> C; _ -> -C
        end end,
    jacobi_helper(A bsr S, N, D);
jacobi_helper(A, N, C) ->
    D = case {A rem 4, N rem 4} of
        {3, 3} -> -C; _ -> C
        end,
    jacobi_helper(N rem A, A, D).

