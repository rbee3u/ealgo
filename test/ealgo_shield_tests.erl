-module(ealgo_shield_tests).
-include_lib("eunit/include/eunit.hrl").


detect_test_() ->
    [{Name, fun() ->
        Trie = ealgo_shield:create(Words),
        ?assertEqual(Expected, ealgo_shield:detect(Sentence, Trie))
    end} || {Name, Words, Sentence, Expected} <- [
        {"empty sentence", ["abc"], "", false},
        {"no match", ["abc", "xyz"], "pqr", false},
        {"match at start", ["abc", "xyz"], "abcde", true},
        {"match at end", ["abc", "xyz"], "12abc", true},
        {"match in middle", ["abc", "xyz"], "12abc34", true},
        {"second dictionary word", ["abc", "xyz"], "12xyz34", true},
        {"Unicode", ["甲乙"], "甲乙丙丁", true},
        {"incomplete repeated word", ["哈哈哈哈"], "哈哈哈", false},
        {"exact repeated word", ["哈哈哈哈"], "哈哈哈哈", true},
        {"overlapping repeated word", ["哈哈哈哈"], "哈哈哈哈哈", true},
        {"restart after partial match", ["aab"], "aaab", true},
        {"partial prefix only", ["abc", "abd"], "ab", false},
        {"shared prefix first branch", ["abc", "abd"], "abc", true},
        {"shared prefix second branch", ["abc", "abd"], "abd", true},
        {"shared prefix mismatch", ["abc", "abd"], "abe", false},
        {"duplicate dictionary words", ["abc", "abc"], "abc", true},
        {"case sensitive", ["bad"], "BAD", false},
        {"substring without word boundaries", ["bad"], "badminton", true},
        {"no Unicode normalization", [[$é]], [$e, 16#301], false},
        {"empty dictionary empty sentence", [], "", false},
        {"empty dictionary nonempty sentence", [], "abc", false}
    ]].


prefix_insertion_order_test_() ->
    [{lists:flatten(io_lib:format("~p", [Words])), fun() ->
        Trie = ealgo_shield:create(Words),
        ?assert(ealgo_shield:detect("ab", Trie)),
        ?assert(ealgo_shield:detect("abc", Trie)),
        ?assertNot(ealgo_shield:detect("a", Trie))
    end} || Words <- [["ab", "abc"], ["abc", "ab"]]].


empty_word_test_() ->
    [{lists:flatten(io_lib:format("~p", [Words])), fun() ->
        Trie = ealgo_shield:create(Words),
        ?assert(ealgo_shield:detect("", Trie)),
        ?assert(ealgo_shield:detect("unrelated", Trie))
    end} || Words <- [[""], ["", "abc"], ["abc", ""]]].
