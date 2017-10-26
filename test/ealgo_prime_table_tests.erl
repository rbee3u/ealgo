-module(ealgo_prime_table_tests).
-include_lib("eunit/include/eunit.hrl").
-import(ealgo_prime_table, [
      new/1
    , list/1
    , get/2
]).


all_test_() ->
    {foreach,
        fun() -> error_logger:tty(false) end,
        fun(_) -> error_logger:tty(true) end,
        [
              {<<"basic_operation">>, fun basic_operation/0}
        ]
    }.


basic_operation() ->
    ?assertEqual(
        list(20), [2,3,5,7,11,13,17,19]
    ),
    PT1 = new(10),
    ?assertEqual(
        get(1, PT1), 2
    ),
    ?assertEqual(
        get(4, PT1), 7
    ),
    PT2 = new(2000000),
    ?assertEqual(
        get(2, PT2), 3
    ),
    ?assertEqual(
        get(100000, PT2), 1299709
    ),
    ok.

