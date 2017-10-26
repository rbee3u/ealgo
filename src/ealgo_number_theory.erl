-module(ealgo_number_theory).
-include("ealgo.hrl").
-export([divisible/2, is_odd/1, is_even/1, multiplicity/2]).
-export([quotient/2, remainder/2, quotient_remainder/2]).
-export([gcd/2, gcd/1, lcm/2, lcm/1, power/2, power_mod/3]).
-export([is_coprime/2, jacobi_symbol/2, is_prime/1, sign/1]).
-export([bit_length/1]).


%% Gives true if N is divisible by M, and false if not.
-spec divisible(N :: integer(), M :: integer()) ->
    P :: boolean().
divisible(N, M) when is_integer(N)
                   , is_integer(M)
                   , ?NEQ(M) ->
    ?EVEN(N, M).

%% Gives true if N is an odd integer, and false otherwise.
-spec is_odd(N :: integer()) ->
    P :: boolean().
is_odd(N) when is_integer(N) ->
    ?ODD(N).

%% Gives true if N is an even integer, and false otherwise.
-spec is_even(N :: integer()) ->
    P :: boolean().
is_even(N) when is_integer(N) ->
    ?EVEN(N).

%% Gives the highest power of A that divides N.
-spec multiplicity(N :: integer(), A :: pos_integer()) ->
    Y :: non_neg_integer().
multiplicity(N, A) when is_integer(N)
                      , is_integer(A)
                      , ?NEQ(N)
                      , ?GT(A, 1) ->
    multiplicity(N, A, 0).
multiplicity(N, A, Y) when ?ODD(N, A) ->
    Y;
multiplicity(N, A, Y) ->
    multiplicity(N, A, Y, 1, A).
multiplicity(N, A, Y, X, M) when ?ODD(N, M * M) ->
    multiplicity(N div M, A, Y + X);
multiplicity(N, A, Y, X, M) ->
    multiplicity(N, A, Y, X + X, M * M).


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
                  , ?NEQ(M) ->
    (N - remainder(N, M)) div M.

%% Gives the remainder on division of N by M.
-spec remainder(N :: integer(), M :: integer()) ->
    R :: integer().
remainder(N, M) when is_integer(N)
                   , is_integer(M)
                   , ?NEQ(M) ->
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
                            , ?NEQ(M) ->
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
    case abs(N * M) of
    0 -> 0;
    P -> P div gcd(N, M)
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
power(A, X) when is_integer(A)
               , is_integer(X)
               , ?GTE(X) ->
    if
    % ?EQ(A), ?EQ(X) undef
    ?EQ(A),  ?NEQ(X) -> 0;
    ?NEQ(A),  ?EQ(X) -> 1;
    ?NEQ(A), ?NEQ(X) ->
        B = power(A, X div 2),
        case ?EVEN(X) of
            true -> B * B;
            _ -> A * B * B
        end
    end.

%% Gives A ^ X mod M where A is an integer, X is a
%% nonnegative integer and M is a nonzero integer.
%% note: power_mod(0, 0, M) is undefined.
-spec power_mod(A :: integer(), X :: non_neg_integer(), M :: integer()) ->
    POWER_MOD :: integer().
power_mod(A, X, M) when is_integer(A)
                      , is_integer(X)
                      , is_integer(M)
                      , ?GTE(X)
                      , ?NEQ(M) ->
    if
    % ?EQ(A), ?EQ(X) undef
    ?EQ(A),  ?NEQ(X) -> 0;
    ?NEQ(A),  ?EQ(X) -> remainder(1, M);
    ?NEQ(A), ?NEQ(X) ->
        B = power_mod(A, X div 2, M),
        case ?EVEN(X) of
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
    ?EQ(gcd(N, M), 1).


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
    false ->
        0;
    true ->
        jacobi_symbol(B, N, 1)
    end.
jacobi_symbol(1, _N, C) ->
    C;
jacobi_symbol(A, N, C) when (A rem 2) =:= 0 ->
    D = case is_even(S = multiplicity(A, 2)) of
        true -> C; false -> case (N rem 8) of
            R when R =/= 3, R =/= 5 -> C; _ -> -C
        end end,
    jacobi_symbol(A bsr S, N, D);
jacobi_symbol(A, N, C) ->
    D = case {A rem 4, N rem 4} of
        {3, 3} -> -C; _ -> C
        end,
    jacobi_symbol(N rem A, A, D).


%% https://en.wikipedia.org/wiki/Prime_number
%% Gives true if N is a prime number, and false otherwise.
-spec is_prime(N :: integer()) ->
    P :: boolean().
is_prime(N) when is_integer(N) ->
    is_prime_helper(N).

is_prime_helper(N) when N  <  2     -> false;
is_prime_helper(N) when N =:= 2     -> true;
is_prime_helper(N) when ?EVEN(N)    -> false;
is_prime_helper(N) when N < 1000000 -> trial_division(N);
is_prime_helper(N) ->
            trial_division(N) andalso miller_rabin(N, 2)
    andalso (not is_square(N)) andalso strong_lucas(N).


trial_division(N) -> trial_division(N, ?PRIME_LIST_1000).
trial_division(_, [     ])                  -> true;
trial_division(N, [H | _]) when H * H > N   -> true;
trial_division(N, [H | _]) when ?EVEN(N, H) -> false;
trial_division(N, [_ | T]) -> trial_division(N, T).

%% https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
miller_rabin(N, A) ->
    S = multiplicity(M = N - 1, 2),
    case power_mod(A, M bsr S, N) of
    B when ?EQ(B,     1)      -> true;
    B when ?EQ(B, N - 1)      -> true;
    B -> miller_rabin(N, B, S - 1)
    end.
miller_rabin(_, _, 0) -> false;
miller_rabin(N, A, S) ->
    case remainder(A * A, N) of
    B when ?EQ(B,     1)      -> false;
    B when ?EQ(B, N - 1)      -> true;
    B -> miller_rabin(N, B, S - 1)
    end.

%% https://en.wikipedia.org/wiki/Lucas_pseudoprime
strong_lucas(N) ->
    A = fun SearchA(X) ->
            case jacobi_symbol(X, N) of
            -1 -> X;
            _  -> SearchA(-X-2*sign(X))
            end
        end (5),
    Q = (1 - A) div 4,
    S = multiplicity(M = N + 1, 2),
    case lucas_uv(D = M bsr S, A, Q, N) of
    {0, _} -> true;
    {_, 0} -> true;
    {_, V} ->
        Q2 = power_mod(Q, D,       N),
        strong_lucas(N, V, Q2, S - 1)
    end.
strong_lucas(_, _, _, 0) -> false;
strong_lucas(N, V, Q, S) ->
    case remainder(V * V - 2 * Q, N) of
    0 -> true;
    V2 ->
        Q2 = remainder(Q * Q,       N),
        strong_lucas(N, V2, Q2, S - 1)
    end.

lucas_uv(1, _, _, _) -> {1, 1};
lucas_uv(D, A, Q, N) ->
    case lucas_uv(K = D div 2, A, Q, N) of
    UV when ?ODD(D) ->
        TEMP = lucas_double(UV, K, Q, N),
        lucas_addone(TEMP, A, N);
    UV ->
        lucas_double(UV, K, Q, N)
    end.

lucas_double({U, V}, K, Q, N) ->
    Q2 = power_mod(Q, K, N),
    {remainder(U * V, N), remainder(V*V - 2*Q2, N)}.

lucas_addone({U, V}, A, N) ->
    UD = case is_odd(UT = U + V) of
            true -> N; _ -> 0
         end,
    VD = case is_odd(VT = A * U + V) of
            true -> N; _ -> 0
         end,
    {remainder((UT + UD) div 2, N),
     remainder((VT + VD) div 2, N)}.

is_square(N) ->
    {_, R} = nthrootrem(N, 2), ?EQ(R).


%% Gives -1, 0, or 1 depending on whether N is negative, zero, or positive.
-spec sign(N :: integer()) ->
    S :: -1 | 0 | 1.
sign(N) when is_integer(N) ->
    if
        ?LT(N) -> -1;
        ?GT(N) ->  1;
        true   ->  0
    end.


%% Gives the number of binary bits necessary to represent the integer N.
%% In other words, the integer S satisfied that 2^(S-1) ≤ |N| < 2^S, and
%% we defined S = 0 when N = 0 for the corner case.
-spec bit_length(N :: integer()) ->
    S :: non_neg_integer().
bit_length(N) when is_integer(N) ->
    bit_length(erlang:abs(N), 0).
bit_length(N, R) when ?GTE(N, 16#10000000000000000) ->
    E = erlang:external_size(N) * 8 - 64,  bit_length(N bsr  E, R +  E);
bit_length(N, R) when ?GTE(N, 16#100000000) -> bit_length(N bsr 32, R + 32);
bit_length(N, R) when ?GTE(N, 16#10000)     -> bit_length(N bsr 16, R + 16);
bit_length(N, R) when ?GTE(N, 16#100)       -> bit_length(N bsr  8, R +  8);
bit_length(N, R) when ?GTE(N, 16#10)        -> bit_length(N bsr  4, R +  4);
bit_length(N, R) when ?GTE(N, 16#4)         -> bit_length(N bsr  2, R +  2);
bit_length(N, R) when ?GTE(N, 16#2)         -> bit_length(N bsr  1, R +  1);
bit_length(N, R) when ?EQ(N, 1); ?EQ(N)     -> N + R.


%% Gives the N'th root and remainder of integer X.
-spec nthrootrem(X :: non_neg_integer(), N :: pos_integer()) ->
    {Y :: non_neg_integer(), R :: non_neg_integer()}.
nthrootrem(X, N) when is_integer(X)
                    , is_integer(N)
                    , ?GTE(X)
                    , ?GT(N) ->
    if
    ?EQ(X); ?EQ(X, 1); ?EQ(N, 1) ->
        {X, 0};
    true ->
        S = (bit_length(X) - 1) div N,
        nthrootrem(X, N, 1 bsl S, 2 bsl S)
    end.
nthrootrem(X, N, L, H) when ?GT(L, H) ->
    {H, X - power(H, N)};
nthrootrem(X, N, L, H) ->
    M = (L + H) div 2,
    case X < power(M, N) of
    true ->
        nthrootrem(X, N, L, M - 1);
    false ->
        nthrootrem(X, N, M + 1, H)
    end.

