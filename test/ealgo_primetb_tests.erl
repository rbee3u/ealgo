-module(ealgo_primetb_tests).
-include_lib("eunit/include/eunit.hrl").


ealgo_primetb_test_() ->
    {foreach,
        fun() -> error_logger:tty(false) end,
        fun(_) -> error_logger:tty(true) end,
        [
              {<<"basic_operation">>, fun basic_operation/0}
        ]
    }.


basic_operation() ->
    ?assertEqual(
        ealgo_primetb:list(20), [2,3,5,7,11,13,17,19]
    ),
    PT1 = ealgo_primetb:new(10),
    ?assertEqual(
        ealgo_primetb:get(1, PT1), 2
    ),
    ?assertEqual(
        ealgo_primetb:get(4, PT1), 7
    ),
    PT2 = ealgo_primetb:new(2000000),
    ?assertEqual(
        ealgo_primetb:get(2, PT2), 3
    ),
    ?assertEqual(
        ealgo_primetb:get(100000, PT2), 1299709
    ),
    ok.

