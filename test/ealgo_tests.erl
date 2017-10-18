-module(ealgo_tests).
-include_lib("eunit/include/eunit.hrl").
-import(ealgo, [
      cartesian_product/1
    , combinations/1
    , combinations/2
    , permutations/1
    , permutations/2
    , next_permutation/1
    , rabin_karp/2
    , rabin_karp/3
    , sgn/1
    , boole/1
    , ustep/1
    , id/1
]).


all_test_() ->
    {foreach,
        fun() -> error_logger:tty(false) end,
        fun(_) -> error_logger:tty(true) end,
        [
            {<<"test cartesian_product">>, fun test_cartesian_product/0},
            {<<"test combinations">>,      fun test_combinations/0},
            {<<"test permutations">>,      fun test_permutations/0},
            {<<"test next_permutation">>,  fun test_next_permutation/0},
            {<<"test rabin_karp">>,        fun test_rabin_karp/0},
            {<<"test sgn">>,               fun test_sgn/0},
            {<<"test boole">>,             fun test_boole/0},
            {<<"test ustep">>,             fun test_ustep/0},
            {<<"test id">>,                fun test_id/0}
        ]
    }.


test_cartesian_product() ->
    ?assertEqual(cartesian_product([])
                , [[]]),
    ?assertEqual(cartesian_product([[]])
                , []),
    ?assertEqual(cartesian_product([[a]])
                , [[a]]),
    ?assertEqual(cartesian_product([[a, b, c]])
                , [[a], [b], [c]]),
    ?assertEqual(cartesian_product([[a, b, c], []])
                , []),
    ?assertEqual(cartesian_product([[], [a, b, c]])
                , []),
    ?assertEqual(cartesian_product([[a, b, c], [x, y, z]])
                , [[a,x], [a,y], [a,z], [b,x], [b,y], [b,z], [c,x], [c,y], [c,z]]),
    ?assertEqual(cartesian_product([[a, b], [p, q], [x, y]])
                , [[a,p,x], [a,p,y], [a,q,x], [a,q,y], [b,p,x], [b,p,y], [b,q,x], [b,q,y]]),
    ok.


test_combinations() ->
    ?assertEqual(combinations([])
                , [[]]),
    ?assertEqual(combinations([a])
                , [[], [a]]),
    ?assertEqual(combinations([a, b])
                , [[], [b], [a], [a, b]]),
    ?assertEqual(combinations([a, b, c])
                , [[], [c], [b], [b, c], [a], [a, c], [a, b], [a, b, c]]),
    ?assertEqual(combinations([], 0)
                , [[]]),
    ?assertEqual(combinations([], 1)
                , []),
    ?assertEqual(combinations([a, b, c], 0)
                , [[]]),
    ?assertEqual(combinations([a, b, c], 1)
                , [[a], [b], [c]]),
    ?assertEqual(combinations([a, b, c], 2)
                , [[a, b], [a, c], [b, c]]),
    ?assertEqual(combinations([a, b, c], 3)
                , [[a, b, c]]),
    ?assertEqual(combinations([a, b, c], 4)
                , []),
    ok.


test_permutations() ->
    ?assertEqual(permutations([])
                , [[]]),
    ?assertEqual(permutations([a])
                , [[a]]),
    ?assertEqual(permutations([a, b])
                , [[a, b], [b, a]]),
    ?assertEqual(permutations([a, b, c])
                , [[a,b,c], [a,c,b], [b,a,c], [b,c,a], [c,a,b], [c,b,a]]),
    ?assertEqual(permutations([], 0)
                , [[]]),
    ?assertEqual(permutations([], 1)
                , []),
    ?assertEqual(permutations([a, b, c], 0)
                , [[]]),
    ?assertEqual(permutations([a, b, c], 1)
                , [[a], [b], [c]]),
    ?assertEqual(permutations([a, b, c], 2)
                , [[a, b], [a, c], [b, a], [b, c], [c, a], [c, b]]),
    ?assertEqual(permutations([a, b, c], 3)
                , [[a,b,c], [a,c,b], [b,a,c], [b,c,a], [c,a,b], [c,b,a]]),
    ?assertEqual(permutations([a, b, c], 4)
                , []),
    ok.


test_next_permutation() ->
    ?assertEqual(next_permutation([])
                , false),
    ?assertEqual(next_permutation([a])
                , false),
    ?assertEqual(next_permutation([a, b])
                , {true, [b, a]}),
    ?assertEqual(next_permutation([b, a])
                , false),
    ?assertEqual(next_permutation([a, b, c])
                , {true, [a, c, b]}),
    ?assertEqual(next_permutation([a, c, b])
                , {true, [b, a, c]}),
    ?assertEqual(next_permutation([b, a, c])
                , {true, [b, c, a]}),
    ?assertEqual(next_permutation([b, c, a])
                , {true, [c, a, b]}),
    ?assertEqual(next_permutation([c, a, b])
                , {true, [c, b, a]}),
    ?assertEqual(next_permutation([c, b, a])
                , false),
    ok.


test_rabin_karp() ->
    ?assertEqual(rabin_karp("", "")
                , [1]),
    ?assertEqual(rabin_karp("", "", 0)
                , []),
    ?assertEqual(rabin_karp("", "", -1)
                , [1]),
    ?assertEqual(rabin_karp("", "", 1)
                , [1]),
    ?assertEqual(rabin_karp("", "a")
                , []),
    ?assertEqual(rabin_karp("", "a", 0)
                , []),
    ?assertEqual(rabin_karp("", "a", -1)
                , []),
    ?assertEqual(rabin_karp("", "a", 1)
                , []),
    ?assertEqual(rabin_karp("a", "a")
                , [1]),
    ?assertEqual(rabin_karp("a", "a", 0)
                , []),
    ?assertEqual(rabin_karp("a", "a", -1)
                , [1]),
    ?assertEqual(rabin_karp("a", "a", 1)
                , [1]),
    ?assertEqual(rabin_karp("aba", "a")
                , [1, 3]),
    ?assertEqual(rabin_karp("aba", "a", 0)
                , []),
    ?assertEqual(rabin_karp("aba", "a", -1)
                , [1, 3]),
    ?assertEqual(rabin_karp("aba", "a", 1)
                , [1]),
    Text = "It was the best of times, "
           "it was the worst of times; "
           "it was the age of wisdom, "
           "it was the age of foolishness; "
           "it was the epoch of belief, "
           "it was the epoch of incredulity; "
           "it was the season of light, "
           "it was the season of darkness; "
           "it was the spring of hope, "
           "it was the winter of despair; "
           "we had everything before us, "
           "we had nothing before us; "
           "we were all going direct to Heaven, "
           "we were all going direct the other way.",
    ?assertEqual(rabin_karp(Text, "it")
                , [27, 54, 80, 111, 139, 167, 172, 200, 231, 258]),
    ok.


test_sgn() ->
    ?assertEqual(sgn(0)
                , 0),
    ?assertEqual(sgn(1)
                , 1),
    ?assertEqual(sgn(2)
                , 1),
    ?assertEqual(sgn(-1)
                , -1),
    ?assertEqual(sgn(-2)
                , -1),
    ?assertEqual(sgn(0.000000000001)
                , 1),
    ?assertEqual(sgn(-0.000000000001)
                , -1),
    ?assertEqual(sgn(12345678901234567890)
                , 1),
    ?assertEqual(sgn(-12345678901234567890)
                , -1),
    ok.


test_boole() ->
    ?assertEqual(boole(true)
                , 1),
    ?assertEqual(boole(false)
                , 0),
    ok.


test_ustep() ->
    ?assertEqual(ustep(0)
                , 1),
    ?assertEqual(ustep(1)
                , 1),
    ?assertEqual(ustep(2)
                , 1),
    ?assertEqual(ustep(-1)
                , 0),
    ?assertEqual(ustep(-2)
                , 0),
    ?assertEqual(ustep(0.000000000001)
                , 1),
    ?assertEqual(ustep(-0.000000000001)
                , 0),
    ?assertEqual(ustep(12345678901234567890)
                , 1),
    ?assertEqual(ustep(-12345678901234567890)
                , 0),
    ok.


test_id() ->
    ?assertEqual(id(233)
                , 233),
    ok.

