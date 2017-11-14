-module(ealgo_tests).
-include_lib("eunit/include/eunit.hrl").
-import(ealgo, [
      cartesian_product/2
    , cartesian_product/1
    , cartesian_power/2
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
    ok.

test_cartesian() ->
    ?assertEqual(cartesian_product([1, 2, 3, 4, 5, 6], []), []),
    ?assertEqual(cartesian_product([a, b, c], [x, y, z]),
        [[a,x], [a,y], [a,z], [b,x], [b,y], [b,z], [c,x], [c,y], [c,z]]),
    ?assertEqual(cartesian_product([]), []),
    ?assertEqual(cartesian_product([[a, b], [p, q], [x, y]]),
        [[a,p,x], [a,p,y], [a,q,x], [a,q,y], [b,p,x], [b,p,y], [b,q,x], [b,q,y]]),
    ?assertEqual(cartesian_power([0, 1], 2), [[0,0], [0,1], [1,0], [1,1]]),
    ok.
