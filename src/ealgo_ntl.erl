-module(ealgo_ntl).
-include("ealgo.hrl").
-import(ealgo, [sgn/1]).
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


%% Returns primes up to and including N in ascending order; N < 2 returns [].
-spec prime_list(N :: integer()) ->
    PL :: [pos_integer()].
prime_list(N) when is_integer(N) ->
    prime_list(N, lists:seq(2, erlang:max(1, N))).
prime_list(N, [H | T]) when H * H =< N ->
    [H | prime_list(N, [X || X <- T, X rem H =/= 0])];
prime_list(_, L) -> L.


%% Returns prime_list(N) as a zero-based array of prime values, not flags.
-spec prime_array(N :: integer()) ->
    PT :: array:array(pos_integer()).
prime_array(N) when is_integer(N) ->
    array:from_list(prime_list(N)).


%% Returns whether N is divisible by M. M must be nonzero.
-spec divisible(N :: integer(), M :: integer()) ->
    P :: boolean().
divisible(N, M) when is_integer(N)
                   , is_integer(M)
                   , M =/= 0 ->
    N rem M =:= 0.


%% Returns whether integer N is odd. Non-integer input raises function_clause.
-spec is_odd(N :: integer()) ->
    P :: boolean().
is_odd(N) when is_integer(N) ->
    N rem 2 =/= 0.


%% Returns whether integer N is even. Non-integer input raises function_clause.
-spec is_even(N :: integer()) ->
    P :: boolean().
is_even(N) when is_integer(N) ->
    N rem 2 =:= 0.


%% Returns the largest exponent K such that A^K divides N. Requires N =/= 0
%% and A > 1; for example, multiplicity(72, 2) returns 3.
-spec multiplicity(N :: integer(), A :: pos_integer()) ->
    Y :: non_neg_integer().
multiplicity(N, A) when is_integer(N)
                      , is_integer(A)
                      , N =/= 0
                      , A > 1 ->
    multiplicity(N, A, 0).
multiplicity(N, A, AccExponent) when N rem A =/= 0 -> AccExponent;
multiplicity(N, A, AccExponent) -> multiplicity(N, A, AccExponent, 1, A).
multiplicity(N, A, AccExponent, PowerExponent, Power) when N rem (Power * Power) =/= 0 ->
    multiplicity(N div Power, A, AccExponent + PowerExponent);
multiplicity(N, A, AccExponent, PowerExponent, Power) ->
    multiplicity(N, A, AccExponent, PowerExponent + PowerExponent, Power * Power).


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


%% Returns the remainder for the floor division defined in quotient/2. M must
%% be nonzero; unlike Erlang's rem, a nonzero result has the sign of M.
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


%% Returns {Q, R} using the floor division defined in quotient/2. M must be
%% nonzero; for example, quotient_remainder(-10, 3) returns {-4, 2}.
-spec quotient_remainder(N :: integer(), M :: integer()) ->
    {Q :: integer(), R :: integer()}.
quotient_remainder(N, M) when is_integer(N)
                            , is_integer(M)
                            , M =/= 0 ->
    R = remainder(N, M),
    Q = (N - R) div M,
    {Q, R}.


%% https://en.wikipedia.org/wiki/Greatest_common_divisor
%% Returns the nonnegative greatest common divisor of N and M, with
%% gcd(0, 0) = 0.
-spec gcd(N :: integer(), M :: integer()) ->
    GCD :: integer().
gcd(N, M) when is_integer(N), is_integer(M) ->
    euclid(abs(N), abs(M)).
euclid(N, 0) -> N;
euclid(N, M) -> euclid(M, N rem M).


%% Returns the nonnegative greatest common divisor of a list of integers.
%% A singleton [X] returns abs(X); an empty list returns 0.
-spec gcd(L :: [integer()]) ->
    GCD :: integer().
gcd(L) when is_list(L) -> lists:foldl(fun gcd/2, 0, L).


%% https://en.wikipedia.org/wiki/Least_common_multiple
%% Returns the nonnegative least common multiple of N and M, or 0 if either
%% argument is zero.
-spec lcm(N :: integer(), M :: integer()) ->
    LCM :: integer().
lcm(N, M) when is_integer(N), is_integer(M) ->
    case abs(N * M) of
    0 -> 0;
    P -> P div gcd(N, M)
    end.


%% Returns the nonnegative least common multiple of a list of integers.
%% A singleton [X] returns abs(X); an empty list returns 1.
-spec lcm(L :: [integer()]) ->
    LCM :: integer().
lcm(L) when is_list(L) -> lists:foldl(fun lcm/2, 1, L).


%% Returns A^X for a nonnegative exponent X. The unsupported case power(0, 0)
%% raises an if_clause error.
-spec power(A :: integer(), X :: non_neg_integer()) ->
    POWER :: integer().
power(A, X) when is_integer(A)
               , is_integer(X)
               , X >= 0 ->
    if
    A =:= 0, X =/= 0 -> 0;
    A =/= 0, X =:= 0 -> 1;
    A =/= 0, X =/= 0 ->
        HalfPower = power(A, X div 2),
        case is_even(X) of
            true -> HalfPower * HalfPower;
            false -> A * HalfPower * HalfPower
        end
    end.

%% Returns remainder(A^X, M) for a nonnegative exponent X and nonzero modulus M.
%% A nonzero result has the sign of M. For valid M, power_mod(0, 0, M) raises
%% an if_clause error.
-spec power_mod(A :: integer(), X :: non_neg_integer(), M :: integer()) ->
    POWER_MOD :: integer().
power_mod(A, X, M) when is_integer(A)
                      , is_integer(X)
                      , is_integer(M)
                      , X >= 0
                      , M =/= 0 ->
    if
    A =:= 0, X =/= 0 -> 0;
    A =/= 0, X =:= 0 -> remainder(1, M);
    A =/= 0, X =/= 0 ->
        HalfPower = power_mod(A, X div 2, M),
        case is_even(X) of
            true -> remainder(HalfPower * HalfPower, M);
            false -> remainder(A * HalfPower * HalfPower, M)
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
%% Returns the Jacobi symbol (A / N). N must be a positive odd integer.
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
jacobi_symbol(1, _N, Sign) ->
    Sign;
jacobi_symbol(A, N, Sign) when (A rem 2) =:= 0 ->
    Twos = multiplicity(A, 2),
    NextSign = case is_odd(Twos) andalso (N rem 8 =:= 3 orelse N rem 8 =:= 5) of
        true -> -Sign;
        false -> Sign
    end,
    jacobi_symbol(A bsr Twos, N, NextSign);
jacobi_symbol(A, N, Sign) ->
    NextSign = case {A rem 4, N rem 4} of
        {3, 3} -> -Sign;
        _ -> Sign
    end,
    jacobi_symbol(N rem A, A, NextSign).


%% https://en.wikipedia.org/wiki/Prime_number
%% Returns false for N < 2 or a detected composite. Trial division is exact
%% below 1000000. For larger N, true indicates a probable prime after trial
%% division, base-2 Miller-Rabin, a square check, and a strong Lucas test.
-spec is_prime(N :: integer()) ->
    P :: boolean().
is_prime(N) when is_integer(N) ->
    if
        N < 2 -> false;
        N =:= 2 -> true;
        N rem 2 =:= 0 -> false;
        N < 1000000 -> trial_division(N);
        true ->
            trial_division(N)
            andalso miller_rabin(N, 2)
            andalso not is_square(N)
            andalso strong_lucas(N)
    end.


trial_division(N) -> trial_division(N, ?PRIME_LIST_1000).
trial_division(_, [     ])                      -> true;
trial_division(N, [H | _]) when H * H > N       -> true;
trial_division(N, [H | _]) when N rem H =:= 0   -> false;
trial_division(N, [_ | T]) -> trial_division(N, T).

%% https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
miller_rabin(N, A) ->
    NMinusOne = N - 1,
    Twos = multiplicity(NMinusOne, 2),
    OddPart = NMinusOne bsr Twos,
    case power_mod(A, OddPart, N) of
        1 -> true;
        NMinusOne -> true;
        Residue -> miller_rabin(N, Residue, Twos - 1)
    end.
miller_rabin(_, _, 0) -> false;
miller_rabin(N, Residue, RemainingRounds) ->
    case remainder(Residue * Residue, N) of
        1 -> false;
        SquaredResidue when SquaredResidue =:= N - 1 -> true;
        SquaredResidue -> miller_rabin(N, SquaredResidue, RemainingRounds - 1)
    end.

%% https://en.wikipedia.org/wiki/Lucas_pseudoprime
strong_lucas(N) ->
    Discriminant = lucas_discriminant(5, N),
    Q = (1 - Discriminant) div 4,
    NPlusOne = N + 1,
    Twos = multiplicity(NPlusOne, 2),
    OddPart = NPlusOne bsr Twos,
    case lucas_sequence(OddPart, Discriminant, Q, N) of
        {0, _, _} -> true;
        {_, 0, _} -> true;
        {_, V, QPower} ->
            strong_lucas(N, V, QPower, Twos - 1)
    end.
strong_lucas(_, _, _, 0) -> false;
strong_lucas(N, V, QPower, RemainingRounds) ->
    case remainder(V * V - 2 * QPower, N) of
        0 -> true;
        NextV ->
            NextQPower = remainder(QPower * QPower, N),
            strong_lucas(N, NextV, NextQPower, RemainingRounds - 1)
    end.

lucas_discriminant(Candidate, N) ->
    case jacobi_symbol(Candidate, N) of
        -1 -> Candidate;
        _ -> lucas_discriminant(-Candidate - 2 * sgn(Candidate), N)
    end.

%% Uses Lucas sequences with P = 1 and Discriminant = 1 - 4*Q. Carry Q^Index
%% mod N alongside U_Index and V_Index to reuse it at the next doubling.
lucas_sequence(1, _, Q, N) -> {1, 1, remainder(Q, N)};
lucas_sequence(Index, Discriminant, Q, N) ->
    HalfSequence = lucas_sequence(Index div 2, Discriminant, Q, N),
    DoubledSequence = lucas_double(HalfSequence, N),
    case Index rem 2 of
        0 -> DoubledSequence;
        1 -> lucas_add_one(DoubledSequence, Discriminant, Q, N)
    end.

lucas_double({U, V, QPower}, N) ->
    NextU = remainder(U * V, N),
    NextV = remainder(V * V - 2 * QPower, N),
    NextQPower = remainder(QPower * QPower, N),
    {NextU, NextV, NextQPower}.

%% Requires odd N. Adding N to an odd numerator makes division by two exact
%% while preserving the numerator's residue modulo N.
lucas_add_one({U, V, QPower}, Discriminant, Q, N) ->
    USum = U + V,
    UAdjustment = case is_odd(USum) of
        true -> N;
        false -> 0
    end,
    VSum = Discriminant * U + V,
    VAdjustment = case is_odd(VSum) of
        true -> N;
        false -> 0
    end,
    NextU = remainder((USum + UAdjustment) div 2, N),
    NextV = remainder((VSum + VAdjustment) div 2, N),
    NextQPower = remainder(QPower * Q, N),
    {NextU, NextV, NextQPower}.

is_square(N) ->
    {_, R} = nthrootrem(N, 2),
    R =:= 0.


%% Returns the number of bits needed to represent abs(N). Zero needs 0 bits;
%% for nonzero N, the result S satisfies 2^(S-1) =< abs(N) < 2^S.
-spec bit_length(N :: integer()) ->
    S :: non_neg_integer().
bit_length(N) when is_integer(N) ->
    bit_length(erlang:abs(N), 0).
bit_length(N, Bits) when N >= 16#10000000000000000 ->
    Shift = erlang:external_size(N) * 8 - 64,
    bit_length(N bsr Shift, Bits + Shift);
bit_length(N, Bits) when N >= 16#100000000 -> bit_length(N bsr 32, Bits + 32);
bit_length(N, Bits) when N >= 16#10000     -> bit_length(N bsr 16, Bits + 16);
bit_length(N, Bits) when N >= 16#100       -> bit_length(N bsr  8, Bits +  8);
bit_length(N, Bits) when N >= 16#10        -> bit_length(N bsr  4, Bits +  4);
bit_length(N, Bits) when N >= 16#4         -> bit_length(N bsr  2, Bits +  2);
bit_length(N, Bits) when N >= 16#2         -> bit_length(N bsr  1, Bits +  1);
bit_length(N, Bits) when N =:= 1; N =:= 0  -> N + Bits.


%% Returns {Y, R}, where Y is the largest integer with Y^N =< X and R = X - Y^N.
%% Requires X >= 0 and N > 0.
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
        Shift = (bit_length(X) - 1) div N,
        nthrootrem(X, N, 1 bsl Shift, 2 bsl Shift)
    end.
nthrootrem(X, N, Low, High) when Low > High ->
    {High, X - power(High, N)};
nthrootrem(X, N, Low, High) ->
    Mid = (Low + High) div 2,
    case X < power(Mid, N) of
        true ->
            nthrootrem(X, N, Low, Mid - 1);
        false ->
            nthrootrem(X, N, Mid + 1, High)
    end.


%% Returns {G, {A, B}}, where G = gcd(N, M) = A*N + B*M is nonnegative.
%% The case extended_gcd(0, 0) returns {0, {0, 0}}.
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
