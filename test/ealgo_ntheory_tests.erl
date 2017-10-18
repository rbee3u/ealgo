-module(ealgo_ntheory_tests).
-include_lib("eunit/include/eunit.hrl").


ealgo_ntheory_test_() ->
    {foreach,
        fun() -> error_logger:tty(false) end,
        fun(_) -> error_logger:tty(true) end,
        [
              {<<"basic_operation">>, fun basic_operation/0}
        ]
    }.


basic_operation() ->
    ok.

