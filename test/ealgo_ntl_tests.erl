-module(ealgo_ntl_tests).
-include_lib("eunit/include/eunit.hrl").
-import(ealgo_ntl, [
      quotient/2
    , remainder/2
    , quotient_remainder/2
    , divisible/2
    , is_odd/1
    , is_even/1
]).


all_test_() ->
    {foreach,
        fun() -> error_logger:tty(false) end,
        fun(_) -> error_logger:tty(true) end,
        [
              {<<"test quotient">>,           fun test_quotient/0}
            , {<<"test remainder">>,          fun test_remainder/0}
            , {<<"test quotient_remainder">>, fun test_quotient_remainder/0}
            , {<<"test is_divisible">>,       fun test_is_divisible/0}
            , {<<"test is_odd">>,             fun test_is_odd/0}
            , {<<"test is_even">>,            fun test_is_even/0}
        ]
    }.


test_quotient() ->
    ?assertEqual(quotient(10, 3), 3),
    ?assertEqual(quotient(-10, 3), -4),
    ?assertEqual(quotient(10, -3), -4),
    ?assertEqual(quotient(-10, -3), 3),
    ok.


test_remainder() ->
    ?assertEqual(remainder(10, 3), 1),
    ?assertEqual(remainder(-10, 3), 2),
    ?assertEqual(remainder(10, -3), -2),
    ?assertEqual(remainder(-10, -3), -1),
    ok.


test_quotient_remainder() ->
    ?assertEqual(quotient_remainder(10, 3), {3, 1}),
    ?assertEqual(quotient_remainder(-10, 3), {-4, 2}),
    ?assertEqual(quotient_remainder(10, -3), {-4, -2}),
    ?assertEqual(quotient_remainder(-10, -3), {3, -1}),
    ok.


test_is_divisible() ->
    ?assertEqual(divisible(10, 3), false),
    ?assertEqual(divisible(10, 2), true),
    ?assertEqual(divisible(10, 5), true),
    ?assertEqual(divisible(10, 7), false),
    ok.


test_is_odd() ->
    ?assertEqual(is_odd(0), false),
    ?assertEqual(is_odd(-1), true),
    ?assertEqual(is_odd(1), true),
    ?assertEqual(is_odd(-2), false),
    ?assertEqual(is_odd(2), false),
    ?assertEqual(is_odd(-223897189), true),
    ?assertEqual(is_odd(223897189), true),
    ?assertEqual(is_odd(-328978959790), false),
    ?assertEqual(is_odd(328978959790), false),
    ok.


test_is_even() ->
    ?assertEqual(is_even(0), true),
    ?assertEqual(is_even(-1), false),
    ?assertEqual(is_even(1), false),
    ?assertEqual(is_even(-2), true),
    ?assertEqual(is_even(2), true),
    ?assertEqual(is_even(-223897189), false),
    ?assertEqual(is_even(223897189), false),
    ?assertEqual(is_even(-328978959790), true),
    ?assertEqual(is_even(328978959790), true),
    ok.


