-module(ealgo_ntl).
-include("ealgo.hrl").
-import(ealgo, [sgn/1, boole/1, id/1]).
-export([
      prime_list/1
    , prime_array/1
    , divisible/2
    , is_odd/1
    , is_even/1
    , multiplicity/2
    , quotient/2
    , remainder/2
    , quotient_remainder/2
    , gcd/2
    , gcd/1
    , lcm/2
    , lcm/1
    , power/2
    , power_mod/3
    , is_coprime/2
    , jacobi_symbol/2
    , is_prime/1
    , bit_length/1
    , nthrootrem/2
    , extended_gcd/2
]).


%% Generates a prime list where any element is not greater than N.
-spec prime_list(N :: integer()) ->
    PL :: [pos_integer()].
prime_list(N) when is_integer(N) ->
    prime_list(N, lists:seq(2, erlang:max(1, N))).
prime_list(N, [H | T]) when H * H =< N ->
    [H | prime_list(N, [X || X <- T, X rem H =/= 0])];
prime_list(_, L) -> L.


%% Generates a prime array where any element is not greater than N.
-spec prime_array(N :: integer()) ->
    PT :: array:array(pos_integer()).
prime_array(N) when is_integer(N) ->
    array:from_list(prime_list(N)).


%% Gives true if N is divisible by M, and false otherwise.
-spec divisible(N :: integer(), M :: integer()) ->
    P :: boolean().
divisible(N, M) when is_integer(N)
                   , is_integer(M)
                   , M =/= 0 ->
    N rem M =:= 0.


%% Gives true if N is an odd integer, and false otherwise.
-spec is_odd(N :: integer()) ->
    P :: boolean().
is_odd(N) when is_integer(N) ->
    N rem 2 =/= 0.


%% Gives true if N is an even integer, and false otherwise.
-spec is_even(N :: integer()) ->
    P :: boolean().
is_even(N) when is_integer(N) ->
    N rem 2 =:= 0.


%% Gives the highest power of A that divides N.
-spec multiplicity(N :: integer(), A :: pos_integer()) ->
    Y :: non_neg_integer().
multiplicity(N, A) when is_integer(N)
                      , is_integer(A)
                      , N =/= 0
                      , A > 1 ->
    multiplicity(N, A, 0).
multiplicity(N, A, Y) when N rem A =/= 0 -> Y;
multiplicity(N, A, Y) -> multiplicity(N, A, Y, 1, A).
multiplicity(N, A, Y, X, M) when N rem (M * M) =/= 0 ->
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
               , X >= 0 ->
    if
    % A =:= 0, X =:= 0 -> undefined
    A =:= 0, X =/= 0 -> 0;
    A =/= 0, X =:= 0 -> 1;
    A =/= 0, X =/= 0 ->
        B = power(A, X div 2),
        case is_even(X) of
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
                      , X >= 0
                      , M =/= 0 ->
    if
    % A =:= 0, X =:= 0 -> undefined
    A =:= 0, X =/= 0 -> 0;
    A =/= 0, X =:= 0 -> remainder(1, M);
    A =/= 0, X =/= 0 ->
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
            R when R =:= 3; R =:= 5 -> (-C); _ -> C
        end end,
    jacobi_symbol(A bsr S, N, D);
jacobi_symbol(A, N, C) ->
    D = case {A rem 4, N rem 4} of
            {3, 3} -> (-C); _ -> C
        end,
    jacobi_symbol(N rem A, A, D).


%% https://en.wikipedia.org/wiki/Prime_number
%% Gives true if N is a prime number, and false otherwise.
-spec is_prime(N :: integer()) ->
    P :: boolean().
is_prime(N) when is_integer(N) ->
    is_prime_helper(N).

is_prime_helper(N) when N  <  2         -> false;
is_prime_helper(N) when N =:= 2         -> true;
is_prime_helper(N) when N rem 2 =:= 0   -> false;
is_prime_helper(N) when N < 1000000 -> trial_division(N);
is_prime_helper(N) ->
            trial_division(N) andalso miller_rabin(N, 2)
    andalso (not is_square(N)) andalso strong_lucas(N).


trial_division(N) -> trial_division(N, ?PRIME_LIST_1000).
trial_division(_, [     ])                      -> true;
trial_division(N, [H | _]) when H * H > N       -> true;
trial_division(N, [H | _]) when N rem H =:= 0   -> false;
trial_division(N, [_ | T]) -> trial_division(N, T).

%% https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
miller_rabin(N, A) ->
    S = multiplicity(M = N - 1, 2),
    case power_mod(A, M bsr S, N) of
    B when B =:=     1      -> true;
    B when B =:= N - 1      -> true;
    B -> miller_rabin(N, B, S - 1)
    end.
miller_rabin(_, _, 0) -> false;
miller_rabin(N, A, S) ->
    case remainder(A * A, N) of
    B when B =:=     1      -> false;
    B when B =:= N - 1      -> true;
    B -> miller_rabin(N, B, S - 1)
    end.

%% https://en.wikipedia.org/wiki/Lucas_pseudoprime
strong_lucas(N) ->
    A = fun SearchA(X) ->
            case jacobi_symbol(X, N) of
            -1 -> X;
            _  -> SearchA(-X-2*sgn(X))
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
    UV when D rem 2 =/= 0 ->
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
    {_, R} = nthrootrem(N, 2), R =:= 0.


%% Gives the number of binary bits necessary to represent the integer N.
%% In other words, the integer S satisfied that 2^(S-1) ≤ |N| < 2^S, and
%% we defined S = 0 when N = 0 for the corner case.
-spec bit_length(N :: integer()) ->
    S :: non_neg_integer().
bit_length(N) when is_integer(N) ->
    bit_length(erlang:abs(N), 0).
bit_length(N, R) when N >= 16#10000000000000000 ->
    E = erlang:external_size(N) * 8 - 64,  bit_length(N bsr  E, R +  E);
bit_length(N, R) when N >= 16#100000000 -> bit_length(N bsr 32, R + 32);
bit_length(N, R) when N >= 16#10000     -> bit_length(N bsr 16, R + 16);
bit_length(N, R) when N >= 16#100       -> bit_length(N bsr  8, R +  8);
bit_length(N, R) when N >= 16#10        -> bit_length(N bsr  4, R +  4);
bit_length(N, R) when N >= 16#4         -> bit_length(N bsr  2, R +  2);
bit_length(N, R) when N >= 16#2         -> bit_length(N bsr  1, R +  1);
bit_length(N, R) when N =:= 1; N =:= 0  -> N + R.


%% Gives the N'th root and remainder of integer X.
-spec nthrootrem(X :: non_neg_integer(), N :: pos_integer()) ->
    {Y :: non_neg_integer(), R :: non_neg_integer()}.
nthrootrem(X, N) when is_integer(X)
                    , is_integer(N)
                    , X >= 0
                    , N > 0 ->
    if
    X =:= 0; X =:= 1; N =:= 1 ->
        {X, 0};
    true ->
        S = (bit_length(X) - 1) div N,
        nthrootrem(X, N, 1 bsl S, 2 bsl S)
    end.
nthrootrem(X, N, L, H) when L > H ->
    {H, X - power(H, N)};
nthrootrem(X, N, L, H) ->
    M = (L + H) div 2,
    case X < power(M, N) of
    true ->
        nthrootrem(X, N, L, M - 1);
    false ->
        nthrootrem(X, N, M + 1, H)
    end.


%% Gives the extended greatest common divisor of N and M.
%% note: extended_gcd(0, 0) = {0, {0, 0}} for convenience.
-spec extended_gcd(N :: integer(), M :: integer()) ->
    {G :: integer(), {A :: integer(), B :: integer()}}.
extended_gcd(N, M) when is_integer(N), is_integer(M) ->
    {G, {A, B}} = extended_euclid(abs(N), abs(M)),
    {G, {A * sgn(N), B * sgn(M)}}.
extended_euclid(N, 0) -> {N, {1, 0}};
extended_euclid(N, M) ->
    {Q, R} = quotient_remainder(N, M),
    {G, {A, B}} = extended_euclid(M, R),
    {G, {B, A - Q * B}}.
