-module(ealgo_ntheory).
-export([is_divisible/2, is_odd/1, is_even/1, multiplicity/2]).
-export([quotient/2, remainder/2, quotient_remainder/2]).
-export([gcd/2, gcd/1, lcm/2, lcm/1, power/2, power_mod/3]).
-export([is_coprime/2, jacobi_symbol/2]).
-compile(export_all).


-define(ODD(N),     ((N rem 2) =/= 0)).
-define(EVEN(N),    ((N rem 2) =:= 0)).
-define(ODD(N, M),  ((N rem M) =/= 0)).
-define(EVEN(N, M), ((N rem M) =:= 0)).


%% Gives true if N is divisible by M, and false if not.
-spec is_divisible(N :: integer(), M :: integer()) ->
    P :: boolean().
is_divisible(N, M) when is_integer(N)
                      , is_integer(M)
                      , M =/= 0 ->
    (N rem M) == 0.

%% Gives true if N is an odd integer, and false otherwise.
-spec is_odd(N :: integer()) ->
    P :: boolean().
is_odd(N) when is_integer(N) ->
    (N rem 2) =/= 0.

%% Gives true if N is an even integer, and false otherwise.
-spec is_even(N :: integer()) ->
    P :: boolean().
is_even(N) when is_integer(N) ->
    (N rem 2) =:= 0.

%% Gives the highest power of A that divides N.
-spec multiplicity(N :: integer(), A :: pos_integer()) ->
    Y :: non_neg_integer().
multiplicity(N, A) when is_integer(N)
                      , is_integer(A)
                      , N =/= 0
                      , A  >  1 ->
    multiplicity(N, A, 1, A, 0, 0).
multiplicity(N, A, 1, _M, _PM, Y) when ?ODD(N, A) ->
    Y div 2;
multiplicity(N, A, X,  M,  PM, Y) when ?ODD(N, M) ->
    multiplicity(N div PM, A, 1, A, 0, Y + X);
multiplicity(N, A, X,  M, _PM, Y) ->
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
    trial_division(N) andalso miller_rabin(N, 2) andalso
    (not is_square(N)) andalso strong_lucas(N).


-define(DIVISOR_LIST, [
 2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73
,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157
,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239
,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331
,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421
,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509
,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613
,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709
,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821
,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919
,929,937,941,947,953,967,971,977,983,991,997,1009,1013,1019
]).
trial_division(N) -> trial_division(N, ?DIVISOR_LIST).
trial_division(N, []) -> true;
trial_division(N, [H | _]) when H * H > N -> true;
trial_division(N, [H | _]) when ?EVEN(N, H) -> false;
trial_division(N, [_ | T]) -> trial_division(N, T).


%% https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
miller_rabin(N, A) ->
    S = multiplicity(N - 1, 2),
    D = (N - 1) bsr S,
    B = power_mod(A, D, N),
    if
    B =:= 1     -> true;
    B =:= N - 1 -> true;
    true ->
        miller_rabin(N, B, S - 1)
    end.
miller_rabin(_N, _A, 0) ->
    false;
miller_rabin(N, A, R) ->
    B = (A * A) rem N,
    if
    B =:= 1     -> false;
    B =:= N - 1 -> true;
    true -> miller_rabin(N, B, R - 1)
    end.



%% https://en.wikipedia.org/wiki/Lucas_pseudoprime
strong_lucas(N) ->
    A = lucas_find_A(5, N),
    P = 1,
    Q = (1 - A) div 4,
    S = multiplicity(N + 1, 2),
    D = (N + 1) bsr S,
    {U, V} = lucas_UV(D, A, P, Q, N),
    if
    U =:= 0; V =:= 0 ->
        true;
    true ->
        strong_lucas(N, V, power_mod(Q, D, N), S - 1)
    end.
strong_lucas(_N, _V, _Q, 0) ->
    false;
strong_lucas(N, V, Q, R) ->
    V2 = remainder(V * V - 2 * Q, N),
    Q2 = remainder(Q * Q,         N),
    if
    V2 =:= 0 -> true;
    true -> strong_lucas(N, V2, Q2, R - 1)
    end.



lucas_find_A(A, N) ->
    case jacobi_symbol(A, N) of
    (-1) -> A; _ when A > 0 ->
         lucas_find_A(-(A+2), N);
    _ -> lucas_find_A(-(A-2), N)
    end.

lucas_UV(1, _A, P, _Q, _N) ->
    {1, P};
lucas_UV(M, A, P, Q, N) ->
    {UK,  VK}  = lucas_UV(K = (M div 2), A, P, Q, N),
    {U2K, V2K} = lucas_UV_double({UK, VK}, K, Q, N),
    case is_odd(M) of
    true ->
        lucas_UV_addone({U2K, V2K}, A, P, N);
    _ ->
        {U2K, V2K}
    end.

lucas_UV_double({UK, VK}, K, Q, N) ->
    U2K = remainder(UK * VK, N),
    V2K = remainder(VK * VK - 2*power_mod(Q, K, N), N),
    {U2K, V2K}.

lucas_UV_addone({UK, VK}, A, P, N) ->
    case is_odd(UT = P * UK + VK) of
    true ->
        UK1 = remainder((UT + N) div 2, N);
    false ->
        UK1 = remainder(UT div 2, N)
    end,
    case is_odd(VT = A * UK + P * VK) of
    true ->
        VK1 = remainder((VT + N) div 2, N);
    false ->
        VK1 = remainder(VT div 2, N)
    end,
    {UK1, VK1}.

is_square(N) when N < 0 -> false;
is_square(N) -> is_square(N, 0, N).
is_square(N, L, H) when L > H -> false;
is_square(N, L, H) ->
    M = (L + H) div 2,
    MM = M * M,
    if
    N > MM -> is_square(N, L + 1, H);
    N < MM -> is_square(N, L, H - 1);
    true -> true
    end.

