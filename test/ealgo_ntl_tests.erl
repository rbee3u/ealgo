-module(ealgo_ntl_tests).
-include_lib("eunit/include/eunit.hrl").


floor_division_test_() ->
    Big = 1 bsl 200,
    [{lists:flatten(io_lib:format("~p / ~p", [N, M])), fun() ->
        ?assertEqual(Q, ealgo_ntl:quotient(N, M)),
        ?assertEqual(R, ealgo_ntl:remainder(N, M)),
        ?assertEqual({Q, R}, ealgo_ntl:quotient_remainder(N, M)),
        ?assertEqual(R =:= 0, ealgo_ntl:divisible(N, M))
    end} || {N, M, Q, R} <- [
        {10, 3, 3, 1}, {-10, 3, -4, 2},
        {10, -3, -4, -2}, {-10, -3, 3, -1},
        {9, 3, 3, 0}, {-9, 3, -3, 0},
        {9, -3, -3, 0}, {-9, -3, 3, 0},
        {0, 3, 0, 0}, {0, -3, 0, 0},
        {1, 3, 0, 1}, {-1, 3, -1, 2},
        {1, -3, -1, -2}, {-1, -3, 0, -1},
        {7, 1, 7, 0}, {7, -1, -7, 0},
        {3 * Big + 1, 3, Big, 1},
        {-3 * Big - 1, 3, -Big - 1, 2}
    ]].


parity_test_() ->
    [{lists:flatten(io_lib:format("~p", [N])), fun() ->
        ?assertEqual(Odd, ealgo_ntl:is_odd(N)),
        ?assertEqual(not Odd, ealgo_ntl:is_even(N))
    end} || {N, Odd} <- [
        {0, false}, {-1, true}, {1, true}, {-2, false}, {2, false},
        {-223897189, true}, {223897189, true},
        {-328978959790, false}, {328978959790, false},
        {1 bsl 200, false}, {-(1 bsl 200) - 1, true}
    ]].


prime_collection_test_() ->
    [{lists:flatten(io_lib:format("primes through ~p", [N])), fun() ->
        ?assertEqual(Expected, ealgo_ntl:prime_list(N)),
        ?assertEqual(Expected, array:to_list(ealgo_ntl:prime_array(N)))
    end} || {N, Expected} <- [
        {-10, []}, {0, []}, {1, []}, {2, [2]}, {3, [2, 3]}, {4, [2, 3]},
        {25, [2, 3, 5, 7, 11, 13, 17, 19, 23]},
        {29, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]},
        {30, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]}
    ]].


common_divisors_test_() ->
    [{lists:flatten(io_lib:format("~p and ~p", [N, M])), fun() ->
        ?assertEqual(G, ealgo_ntl:gcd(N, M)),
        ?assertEqual(L, ealgo_ntl:lcm(N, M)),
        ?assertEqual(G =:= 1, ealgo_ntl:is_coprime(N, M)),
        {ActualG, {A, B}} = ealgo_ntl:extended_gcd(N, M),
        ?assertEqual(G, ActualG),
        %% Bezout coefficients need not be a particular valid pair.
        ?assertEqual(G, A * N + B * M)
    end} || {N, M, G, L} <- [
        {0, 0, 0, 0}, {0, 7, 7, 0}, {7, 0, 7, 0},
        {0, -7, 7, 0}, {-7, 0, 7, 0},
        {54, 24, 6, 216}, {-54, 24, 6, 216},
        {54, -24, 6, 216}, {-54, -24, 6, 216},
        {24, 54, 6, 216}, {7, 7, 7, 7},
        {35, 64, 1, 2240}, {-1, 0, 1, 0},
        {12 bsl 130, 18 bsl 130, 6 bsl 130, 36 bsl 130}
    ]].


extended_gcd_zero_test() ->
    ?assertEqual({0, {0, 0}}, ealgo_ntl:extended_gcd(0, 0)).


common_divisors_list_test_() ->
    [{lists:flatten(io_lib:format("~p", [Items])), fun() ->
        ?assertEqual(G, ealgo_ntl:gcd(Items)),
        ?assertEqual(L, ealgo_ntl:lcm(Items))
    end} || {Items, G, L} <- [
        {[], 0, 1}, {[0], 0, 0}, {[-7], 7, 7},
        {[0, 0], 0, 0}, {[0, -6, 9], 3, 0},
        {[6, 0, 9], 3, 0}, {[6, 9, 0], 3, 0},
        {[-6, 10, 15], 1, 30}
    ]].


multiplicity_test_() ->
    [{lists:flatten(io_lib:format("multiplicity(~p, ~p)", [N, Base])),
      ?_assertEqual(Expected, ealgo_ntl:multiplicity(N, Base))}
     || {N, Base, Expected} <- [
        {1, 2, 0}, {-1, 2, 0}, {7, 2, 0}, {2, 3, 0},
        {72, 2, 3}, {-72, 2, 3}, {72, 3, 2},
        {64, 4, 3}, {3 bsl 257, 2, 257}
    ]].


power_test_() ->
    [{lists:flatten(io_lib:format("~p ^ ~p", [Base, Exponent])),
      ?_assertEqual(Expected, ealgo_ntl:power(Base, Exponent))}
     || {Base, Exponent, Expected} <- [
        {0, 3, 0}, {7, 0, 1}, {-7, 0, 1}, {7, 1, 7},
        {-2, 3, -8}, {-2, 4, 16}, {3, 5, 243},
        {2, 256, 1 bsl 256}, {-2, 257, -(1 bsl 257)}
    ]].


power_mod_test_() ->
    [{lists:flatten(io_lib:format("~p ^ ~p modulo ~p", [Base, Exponent, Modulus])),
      ?_assertEqual(Expected, ealgo_ntl:power_mod(Base, Exponent, Modulus))}
     || {Base, Exponent, Modulus, Expected} <- [
        {2, 10, 7, 2}, {2, 10, -7, -5},
        {-2, 3, 5, 2}, {-2, 3, -5, -3}, {-2, 4, 5, 1},
        {2, 3, -4, 0}, {0, 5, 7, 0}, {0, 5, -7, 0},
        {3, 0, 7, 1}, {3, 0, -7, -6},
        {3, 0, 1, 0}, {3, 0, -1, 0}, {3, 5, 1, 0}, {3, 5, -1, 0},
        {(1 bsl 128) + 1, 2, 1 bsl 128, 1}, {2, 1000, 17, 1}
    ]].


jacobi_symbol_test_() ->
    [{lists:flatten(io_lib:format("(~p / ~p)", [A, N])),
      ?_assertEqual(Expected, ealgo_ntl:jacobi_symbol(A, N))}
     || {A, N, Expected} <- [
        {0, 1, 1}, {-3, 1, 1}, {0, 9, 0}, {6, 9, 0},
        {2, 3, -1}, {2, 5, -1}, {2, 7, 1}, {2, 9, 1},
        {4, 3, 1}, {3, 7, -1}, {5, 11, 1},
        {-1, 3, -1}, {-1, 5, 1}, {9, 7, 1},
        %% Composite denominators: Jacobi +1 does not imply a square residue.
        {2, 15, 1}, {5, 15, 0}
    ]].


is_prime_test_() ->
    [{lists:flatten(io_lib:format("~p", [N])), ?_assertEqual(Expected, ealgo_ntl:is_prime(N))}
     || {N, Expected} <- [
        {-7, false}, {0, false}, {1, false}, {2, true}, {3, true},
        {4, false}, {49, false}, {561, false}, {997, true},
        {997 * 997, false}, {999983, true}, {999999, false}, {1000000, false},
        {1000003, true}, {1000033, true},
        {(1 bsl 255) - 19, true},
        {(1 bsl 256) - (1 bsl 32) - 977, true},
        {(1 bsl 448) - (1 bsl 224) - 1, true},
        %% Base-2 strong pseudoprimes must still be rejected.
        {2251 * 11251, false}, {10670053 * 32010157, false},
        {1000003 * 1000033, false}, {1000003 * 1000003, false}
    ]].


bit_length_test_() ->
    [?_assertEqual(0, ealgo_ntl:bit_length(0)),
     [{lists:flatten(io_lib:format("near 2^~p, sign ~p", [Bits, Sign])), fun() ->
        ?assertEqual(Bits, ealgo_ntl:bit_length(Sign * ((1 bsl Bits) - 1))),
        ?assertEqual(Bits + 1, ealgo_ntl:bit_length(Sign * (1 bsl Bits))),
        ?assertEqual(Bits + 1, ealgo_ntl:bit_length(Sign * ((1 bsl Bits) + 1)))
      end} || Bits <- [1, 2, 4, 8, 16, 32, 64, 65, 128, 255, 256, 1024, 8192],
              Sign <- [-1, 1]]].


nthrootrem_test_() ->
    Big = 1 bsl 128,
    [{Name, ?_assertEqual(Expected, ealgo_ntl:nthrootrem(X, Degree))}
     || {Name, X, Degree, Expected} <- [
        {"zero", 0, 7, {0, 0}},
        {"one", 1, 7, {1, 0}},
        {"degree one", Big, 1, {Big, 0}},
        {"degree exceeds bit length", 2, 100, {1, 1}},
        {"below square", 15, 2, {3, 6}},
        {"exact square", 16, 2, {4, 0}},
        {"above square", 17, 2, {4, 1}},
        {"below cube", 26, 3, {2, 18}},
        {"exact cube", 27, 3, {3, 0}},
        {"above cube", 28, 3, {3, 1}},
        {"below large square", (1 bsl 256) - 1, 2, {Big - 1, 2 * Big - 2}},
        {"exact large square", 1 bsl 256, 2, {Big, 0}},
        {"above large square", (1 bsl 256) + 1, 2, {Big, 1}},
        {"large odd degree", (1 bsl 300) + 7, 3, {1 bsl 100, 7}}
    ]].


invalid_domain_test_() ->
    [{Name, ?_assertError(Reason, Call())} || {Name, Reason, Call} <- [
        {"quotient by zero", function_clause, fun() -> ealgo_ntl:quotient(1, 0) end},
        {"remainder by zero", function_clause, fun() -> ealgo_ntl:remainder(1, 0) end},
        {"quotient/remainder by zero", function_clause,
         fun() -> ealgo_ntl:quotient_remainder(1, 0) end},
        {"divisibility by zero", function_clause, fun() -> ealgo_ntl:divisible(1, 0) end},
        {"odd noninteger", function_clause, fun() -> ealgo_ntl:is_odd(1.0) end},
        {"even noninteger", function_clause, fun() -> ealgo_ntl:is_even(2.0) end},
        {"multiplicity of zero", function_clause, fun() -> ealgo_ntl:multiplicity(0, 2) end},
        {"multiplicity base one", function_clause, fun() -> ealgo_ntl:multiplicity(8, 1) end},
        {"multiplicity base zero", function_clause, fun() -> ealgo_ntl:multiplicity(8, 0) end},
        {"zero to zero power", if_clause, fun() -> ealgo_ntl:power(0, 0) end},
        {"negative exponent", function_clause, fun() -> ealgo_ntl:power(2, -1) end},
        {"zero to zero modular power", if_clause, fun() -> ealgo_ntl:power_mod(0, 0, 7) end},
        {"negative modular exponent", function_clause, fun() -> ealgo_ntl:power_mod(2, -1, 7) end},
        {"zero modulus", function_clause, fun() -> ealgo_ntl:power_mod(2, 3, 0) end},
        {"Jacobi even denominator", function_clause, fun() -> ealgo_ntl:jacobi_symbol(3, 4) end},
        {"Jacobi zero denominator", function_clause, fun() -> ealgo_ntl:jacobi_symbol(3, 0) end},
        {"Jacobi negative denominator", function_clause, fun() -> ealgo_ntl:jacobi_symbol(3, -1) end},
        {"root of negative integer", function_clause, fun() -> ealgo_ntl:nthrootrem(-1, 2) end},
        {"root degree zero", function_clause, fun() -> ealgo_ntl:nthrootrem(8, 0) end},
        {"root negative degree", function_clause, fun() -> ealgo_ntl:nthrootrem(8, -1) end}
    ]].
