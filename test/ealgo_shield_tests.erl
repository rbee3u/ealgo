-module(ealgo_shield_tests).
-include_lib("eunit/include/eunit.hrl").


all_test_() ->
    {foreach,
        fun() -> error_logger:tty(false) end,
        fun(_) -> error_logger:tty(true) end,
        [
              {<<"test detect">>, fun test_detect/0}
        ]
    }.


test_detect() ->
    ShieldingWordList = ["abc", "xyz", "甲乙", "哈哈哈哈"],
    ShieldingTrie = ealgo_shield:create(ShieldingWordList),
    ?assertEqual(ealgo_shield:detect("", ShieldingTrie), false),
    ?assertEqual(ealgo_shield:detect("pqr", ShieldingTrie), false),
    ?assertEqual(ealgo_shield:detect("abcde", ShieldingTrie), true),
    ?assertEqual(ealgo_shield:detect("12abc", ShieldingTrie), true),
    ?assertEqual(ealgo_shield:detect("甲乙丙丁", ShieldingTrie), true),
    ?assertEqual(ealgo_shield:detect("哈哈哈", ShieldingTrie), false),
    ?assertEqual(ealgo_shield:detect("哈哈哈哈", ShieldingTrie), true),
    ?assertEqual(ealgo_shield:detect("哈哈哈哈哈", ShieldingTrie), true),
    ok.

