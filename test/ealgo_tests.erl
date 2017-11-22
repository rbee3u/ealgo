-module(ealgo_tests).
-include_lib("eunit/include/eunit.hrl").
-import(ealgo, [
      cartesian_product/1
    , combinations/1, combinations/2
    , permutations/1, permutations/2
    , next_permutation/1
    , sign/1, boole/1, ustep/1
    , rabin_karp/2, rabin_karp/3
]).


all_test_() ->
    {foreach,
        fun() -> error_logger:tty(false) end,
        fun(_) -> error_logger:tty(true) end,
        [
            {<<"test cartesian product">>
                , fun test_cartesian_product/0},
            {<<"test combinations">>
                , fun test_combinations/0},
            {<<"test permutations">>
                , fun test_permutations/0},
            {<<"test next_permutation">>
                , fun test_next_permutation/0}
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

