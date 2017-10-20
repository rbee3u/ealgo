-module(ealgo_ntheory_tests).
-include_lib("eunit/include/eunit.hrl").


ealgo_ntheory_test_() ->
    {foreach,
        fun() -> error_logger:tty(false) end,
        fun(_) -> error_logger:tty(true) end,
        [
              {<<"test_quotient">>, fun test_quotient/0}
            , {<<"test_remainder">>, fun test_remainder/0}
            , {<<"test_quotient_remainder">>, fun test_quotient_remainder/0}
            , {<<"test_is_divisible">>, fun test_is_divisible/0}
            , {<<"test_is_odd">>, fun test_is_odd/0}
            , {<<"test_is_even">>, fun test_is_even/0}
        ]
    }.


test_quotient() ->
    ?assertEqual(ealgo_ntheory:quotient(10, 3), 3),
    ?assertEqual(ealgo_ntheory:quotient(-10, 3), -4),
    ?assertEqual(ealgo_ntheory:quotient(10, -3), -4),
    ?assertEqual(ealgo_ntheory:quotient(-10, -3), 3),
    ok.


test_remainder() ->
    ?assertEqual(ealgo_ntheory:remainder(10, 3), 1),
    ?assertEqual(ealgo_ntheory:remainder(-10, 3), 2),
    ?assertEqual(ealgo_ntheory:remainder(10, -3), -2),
    ?assertEqual(ealgo_ntheory:remainder(-10, -3), -1),
    ok.


test_quotient_remainder() ->
    ?assertEqual(ealgo_ntheory:quotient_remainder(10, 3), {3, 1}),
    ?assertEqual(ealgo_ntheory:quotient_remainder(-10, 3), {-4, 2}),
    ?assertEqual(ealgo_ntheory:quotient_remainder(10, -3), {-4, -2}),
    ?assertEqual(ealgo_ntheory:quotient_remainder(-10, -3), {3, -1}),
    ok.


test_is_divisible() ->
    ?assertEqual(ealgo_ntheory:is_divisible(10, 3), false),
    ?assertEqual(ealgo_ntheory:is_divisible(10, 2), true),
    ?assertEqual(ealgo_ntheory:is_divisible(10, 5), true),
    ?assertEqual(ealgo_ntheory:is_divisible(10, 7), false),
    ok.


test_is_odd() ->
    ?assertEqual(ealgo_ntheory:is_odd(0), false),
    ?assertEqual(ealgo_ntheory:is_odd(-1), true),
    ?assertEqual(ealgo_ntheory:is_odd(1), true),
    ?assertEqual(ealgo_ntheory:is_odd(-2), false),
    ?assertEqual(ealgo_ntheory:is_odd(2), false),
    ?assertEqual(ealgo_ntheory:is_odd(-223897189), true),
    ?assertEqual(ealgo_ntheory:is_odd(223897189), true),
    ?assertEqual(ealgo_ntheory:is_odd(-328978959790), false),
    ?assertEqual(ealgo_ntheory:is_odd(328978959790), false),
    ok.


test_is_even() ->
    ?assertEqual(ealgo_ntheory:is_even(0), true),
    ?assertEqual(ealgo_ntheory:is_even(-1), false),
    ?assertEqual(ealgo_ntheory:is_even(1), false),
    ?assertEqual(ealgo_ntheory:is_even(-2), true),
    ?assertEqual(ealgo_ntheory:is_even(2), true),
    ?assertEqual(ealgo_ntheory:is_even(-223897189), false),
    ?assertEqual(ealgo_ntheory:is_even(223897189), false),
    ?assertEqual(ealgo_ntheory:is_even(-328978959790), true),
    ?assertEqual(ealgo_ntheory:is_even(328978959790), true),
    ok.


