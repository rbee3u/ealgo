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


cartesian_product_test_() ->
    %% Compare multiplicities, not the unspecified order of the outer list.
    %% Map keys also keep distinct terms such as 1 and 1.0 distinct.
    [{Name, ?_assertEqual(counts(Expected), counts(cartesian_product(Lists)))}
     || {Name, Lists, Expected} <- [
        {"no factors", [], [[]]},
        {"empty factor", [[]], []},
        {"singleton", [[a]], [[a]]},
        {"one factor", [[a, b, c]], [[a], [b], [c]]},
        {"empty last factor", [[a, b, c], []], []},
        {"empty first factor", [[], [a, b, c]], []},
        {"empty middle factor", [[a], [], [b]], []},
        {"two factors", [[a, b, c], [x, y, z]],
         [[a,x], [a,y], [a,z], [b,x], [b,y], [b,z], [c,x], [c,y], [c,z]]},
        {"three factors", [[a, b], [p, q], [x, y]],
         [[a,p,x], [a,p,y], [a,q,x], [a,q,y], [b,p,x], [b,p,y], [b,q,x], [b,q,y]]},
        {"duplicate positions", [[a, a], [b]], [[a,b], [a,b]]}
    ]].


combinations_test_() ->
    [{Name, ?_assertEqual(counts(Expected), counts(combinations(Items)))}
     || {Name, Items, Expected} <- [
        {"empty", [], [[]]},
        {"singleton", [a], [[], [a]]},
        {"two items", [a, b], [[], [b], [a], [a,b]]},
        {"three items", [a, b, c],
         [[], [c], [b], [b,c], [a], [a,c], [a,b], [a,b,c]]},
        {"duplicate positions", [a, a], [[], [a], [a], [a,a]]}
    ]].


combinations_amount_test_() ->
    [{Name, ?_assertEqual(counts(Expected), counts(combinations(Items, Amount)))}
     || {Name, Items, Amount, Expected} <- [
        {"empty zero", [], 0, [[]]},
        {"empty positive", [], 1, []},
        {"zero", [a, b, c], 0, [[]]},
        {"one", [a, b, c], 1, [[a], [b], [c]]},
        {"partial", [a, b, c], 2, [[a,b], [a,c], [b,c]]},
        {"full", [a, b, c], 3, [[a,b,c]]},
        {"too many", [a, b, c], 4, []},
        {"duplicate positions", [a, b, a], 2, [[a,b], [a,a], [b,a]]}
    ]].


combinations_near_full_test() ->
    L = lists:seq(1, 50),
    Expected = [lists:delete(X, L) || X <- L],
    ?assertEqual(counts(Expected), counts(combinations(L, 49))),
    ?assertEqual([L], combinations(L, 50)).


permutations_test_() ->
    [{Name, ?_assertEqual(counts(Expected), counts(permutations(Items)))}
     || {Name, Items, Expected} <- [
        {"empty", [], [[]]},
        {"singleton", [a], [[a]]},
        {"two items", [a, b], [[a,b], [b,a]]},
        {"three items", [a, b, c],
         [[a,b,c], [a,c,b], [b,a,c], [b,c,a], [c,a,b], [c,b,a]]},
        {"duplicate positions", [a, b, a],
         [[a,b,a], [a,a,b], [b,a,a], [b,a,a], [a,a,b], [a,b,a]]},
        {"distinct numeric terms", [1, 1.0], [[1,1.0], [1.0,1]]}
    ]].


permutations_amount_test_() ->
    [{Name, ?_assertEqual(counts(Expected), counts(permutations(Items, Amount)))}
     || {Name, Items, Amount, Expected} <- [
        {"empty zero", [], 0, [[]]},
        {"empty positive", [], 1, []},
        {"zero", [a, b, c], 0, [[]]},
        {"one", [a, b, c], 1, [[a], [b], [c]]},
        {"partial", [a, b, c], 2, [[a,b], [a,c], [b,a], [b,c], [c,a], [c,b]]},
        {"full", [a, b, c], 3,
         [[a,b,c], [a,c,b], [b,a,c], [b,c,a], [c,a,b], [c,b,a]]},
        {"too many", [a, b, c], 4, []},
        {"duplicates single draw", [a, b, a], 1, [[a], [b], [a]]},
        {"duplicates partial", [a, b, a], 2,
         [[a,b], [a,a], [b,a], [b,a], [a,a], [a,b]]},
        {"distinct numeric terms", [1, 1.0], 2, [[1,1.0], [1.0,1]]}
    ]].


combinatorial_invalid_input_test_() ->
    [{lists:flatten(io_lib:format("~p(~p, ~p)", [Name, Items, Amount])),
      ?_assertError(badarg, Generate(Items, Amount))}
     || {Name, Generate} <- [{combinations, fun ealgo:combinations/2},
                            {permutations, fun ealgo:permutations/2}],
        {Items, Amount} <- [{[a], -1}, {[a], 1.0}, {not_a_list, 0}]].


next_permutation_test_() ->
    [{lists:flatten(io_lib:format("~p", [Items])), ?_assertEqual(Expected, next_permutation(Items))}
     || {Items, Expected} <- [
        {[], false},
        {[a], false},
        {[a, b], {true, [b, a]}},
        {[b, a], false},
        {[a, b, c], {true, [a, c, b]}},
        {[a, c, b], {true, [b, a, c]}},
        {[b, a, c], {true, [b, c, a]}},
        {[b, c, a], {true, [c, a, b]}},
        {[c, a, b], {true, [c, b, a]}},
        {[c, b, a], false},
        {[a, a, b], {true, [a, b, a]}},
        {[a, b, a], {true, [b, a, a]}},
        {[b, a, a], false},
        {[a, a, a], false},
        {[1, 3, 5, 4, 2], {true, [1, 4, 2, 3, 5]}},
        {[1, a, {b}], {true, [1, {b}, a]}}
    ]].


rabin_karp_test_() ->
    Cases = [
        {"empty text and pattern", "", "", [1]},
        {"empty text", "", "a", []},
        {"exact match", "a", "a", [1]},
        {"separated matches", "aba", "a", [1, 3]},
        {"overlapping matches", "aaaaa", "aaa", [1, 2, 3]},
        {"overlapping Unicode", "甲乙甲乙甲", "甲乙甲", [1, 3]},
        {"match at end", "abc", "bc", [2]},
        {"pattern longer than text", "ab", "abc", []},
        {"no match", "abcabc", "abd", []},
        {"empty pattern", "abc", "", [1, 2, 3, 4]},
        %% These distinct Unicode windows collide in the rolling hash.
        {"hash collision before exact match", [98, 743, 97, 1000], [97, 1000], [3]},
        {"case sensitive", "ABCabc", "abc", [4]}
    ],
    [{Name, [
        ?_assertEqual(Expected, rabin_karp(Text, Pattern)),
        [{lists:flatten(io_lib:format("limit ~p", [Limit])),
          ?_assertEqual(case Limit < 0 of
                            true -> Expected;
                            false -> lists:sublist(Expected, Limit)
                        end, rabin_karp(Text, Pattern, Limit))}
         || Limit <- lists:usort([-5, -1, 0, 1, 2, length(Expected) + 1])]
    ]} || {Name, Text, Pattern, Expected} <- Cases].


numeric_helpers_test_() ->
    [{lists:flatten(io_lib:format("~p", [X])), [
        ?_assertEqual(Sign, sgn(X)),
        ?_assertEqual(Step, ustep(X))
    ]} || {X, Sign, Step} <- [
        {0, 0, 1}, {0.0, 0, 1}, {-0.0, 0, 1},
        {1, 1, 1}, {-1, -1, 0},
        {0.000000000001, 1, 1}, {-0.000000000001, -1, 0},
        {12345678901234567890, 1, 1}, {-12345678901234567890, -1, 0}
    ]].


numeric_helpers_invalid_input_test_() ->
    [?_assertError(badarg, F(not_a_number)) || F <- [fun ealgo:sgn/1, fun ealgo:ustep/1]].


boole_test_() ->
    [?_assertEqual(1, boole(true)), ?_assertEqual(0, boole(false))].


id_test_() ->
    [?_assertEqual(X, id(X)) || X <- [233, 1.0, atom, [], {a, 1}, #{a => [b]}]].


shuffle_contents_test_() ->
    [{lists:flatten(io_lib:format("~p", [Items])), fun() ->
        rand:seed(exsplus, {11, 22, 33}),
        ?assertEqual(counts(Items), counts(ealgo:shuffle(Items)))
    end} || Items <- [[], [a], [a, a], [a, {b, 1}, 1, 1.0, a, [c]]]].


random_permutation_distribution_test_() ->
    [{Name, fun() ->
        rand:seed(exsplus, {41, 52, 63}),
        Counts = counts([Draw() || _ <- lists:seq(1, 6000)]),
        ?assertEqual(lists:sort(Expected), lists:sort(maps:keys(Counts))),
        %% Fixed seeds and wide bounds detect gross bias without exact draws.
        lists:foreach(fun(Outcome) ->
            Count = maps:get(Outcome, Counts),
            ?assert(Count > 700 andalso Count < 1300)
        end, Expected)
    end} || {Name, Draw, Expected} <- [
        {"shuffle", fun() -> ealgo:shuffle([a, b, c]) end,
         [[a,b,c], [a,c,b], [b,a,c], [b,c,a], [c,a,b], [c,b,a]]},
        {"sample without replacement", fun() ->
            {ok, Pair} = ealgo:get_rand_elems([a, b, c], 2), Pair
         end, [[a,b], [a,c], [b,a], [b,c], [c,a], [c,b]]}
    ]].


weighted_selection_boundaries_test_() ->
    [{Name, ?_assertEqual(Expected, ealgo:select_by_weight(Items))}
     || {Name, Items, Expected} <- [
        {"empty", [], {error, <<"AllWeightMustGreaterThanZero">>}},
        {"all zero", [{a, 0}, {b, 0}], {error, <<"AllWeightMustGreaterThanZero">>}},
        {"single positive", [{a, 7}], {ok, a}},
        {"positive first", [{a, 7}, {b, 0}], {ok, a}},
        {"positive middle", [{a, 0}, {b, 7}, {c, 0}], {ok, b}},
        {"positive last", [{a, 0}, {b, 7}], {ok, b}}
    ]].


random_weight_distribution_test_() ->
    [{Name, fun() ->
        rand:seed(exsplus, {21, 32, 43}),
        Counts = counts([Draw() || _ <- lists:seq(1, 4000)]),
        ?assertEqual([a, b], lists:sort(maps:keys(Counts))),
        %% a has three times b's weight, whether explicit or from positions.
        CountA = maps:get(a, Counts),
        ?assert(CountA > 2800 andalso CountA < 3200)
    end} || {Name, Draw} <- [
        {"weighted choice", fun() ->
            {ok, Key} = ealgo:select_by_weight([{zero, 0}, {a, 3}, {b, 1}]), Key
         end},
        {"weighted partial sample", fun() ->
            {ok, [Key]} = ealgo:select_amount_by_weight([{zero, 0}, {a, 3}, {b, 1}], 1), Key
         end},
        {"single element keeps duplicate positions", fun() ->
            ealgo:get_rand_elem([a, a, a, b])
         end},
        {"single sample keeps duplicate positions", fun() ->
            {ok, [Key]} = ealgo:get_rand_elems([a, a, a, b], 1), Key
         end}
    ]].


weighted_sample_distribution_test() ->
    rand:seed(exsplus, {21, 32, 43}),
    Items = [{zero, 0}, {a, 1}, {b, 2}, {c, 3}],
    Counts = counts([begin
        {ok, Keys} = ealgo:select_amount_by_weight(Items, 2),
        Keys
    end || _ <- lists:seq(1, 6000)]),
    %% P(first, second) = w_first / 6 * w_second / (6 - w_first).
    %% Results are in reverse draw order: [b,a] means a was drawn first.
    ExpectedCounts = #{[b,a] => 400, [c,a] => 600,
                       [a,b] => 500, [c,b] => 1500,
                       [a,c] => 1000, [b,c] => 2000},
    %% Exact outcomes also rule out replacement, zero weights and short samples.
    ?assertEqual(lists:sort(maps:keys(ExpectedCounts)), lists:sort(maps:keys(Counts))),
    maps:foreach(fun(Outcome, Expected) ->
        Actual = maps:get(Outcome, Counts),
        %% Fixed seed and a wide tolerance detect bias without pinning draws.
        ?assertMatch({_, _, _, true},
                     {Outcome, Expected, Actual, abs(Actual - Expected) < Expected div 5})
    end, ExpectedCounts).


weighted_sample_boundaries_test_() ->
    [{Name, ?_assertEqual(Expected, ealgo:select_amount_by_weight(Items, Amount))}
     || {Name, Items, Amount, Expected} <- [
        {"zero amount", [{a, 1}], 0, {error, <<"AmountMustGreaterThanZero">>}},
        {"negative amount", [{a, 1}], -1, {error, <<"AmountMustGreaterThanZero">>}},
        {"empty zero", [], 0, {error, <<"AmountMustGreaterThanZero">>}},
        {"empty positive", [], 1, {error, <<"ListLengthMustGreaterThanAmount">>}},
        {"too many", [{a, 1}], 2, {error, <<"ListLengthMustGreaterThanAmount">>}},
        {"full sample keeps order and zero weights", [{c, 0}, {a, 2}, {b, 0}], 3,
         {ok, [c, a, b]}},
        {"full sample with all zero weights", [{a, 0}, {b, 0}], 2, {ok, [a, b]}}
    ]].


weighted_sample_insufficient_positive_weights_test_() ->
    [{Name, ?_assertError({badmatch, _}, ealgo:select_amount_by_weight(Items, Amount))}
     || {Name, Items, Amount} <- [
        {"no positive weights", [{a, 0}, {b, 0}], 1},
        {"weights exhausted after a draw", [{a, 0}, {b, 1}, {c, 0}], 2}
    ]].


weighted_sample_zero_weights_test() ->
    rand:seed(exsplus, {21, 32, 43}),
    {ok, Keys} = ealgo:select_amount_by_weight([{zero, 0}, {a, 1}, {b, 2}], 2),
    ?assertEqual([a, b], lists:sort(Keys)).


random_sample_contents_test_() ->
    [{Name, fun() ->
        rand:seed(exsplus, {31, 42, 53}),
        Available = counts(Items),
        lists:foreach(fun(_) ->
            {ok, Selected} = ealgo:get_rand_elems(Items, Amount),
            ?assertEqual(Amount, length(Selected)),
            maps:foreach(fun(Item, Count) ->
                %% No value may be selected more often than it occurs in the input.
                Limit = maps:get(Item, Available, 0),
                ?assertMatch({_, _, _, true}, {Item, Count, Limit, Count =< Limit})
            end, counts(Selected))
        end, lists:seq(1, Repeats))
    end} || {Name, Items, Amount, Repeats} <- [
        {"large partial sample", lists:seq(1, 16000), 8000, 1},
        {"mixed duplicate positions", [a, a, b, c], 3, 100},
        {"distinct numeric and compound terms", [1, 1.0, {a}, [b], #{c => d}], 4, 100}
    ]].


random_sample_boundaries_test_() ->
    [{Name, ?_assertEqual(Expected, ealgo:get_rand_elems(Items, Amount))}
     || {Name, Items, Amount, Expected} <- [
        {"empty positive", [], 1, {error, <<"ListEmpty">>}},
        {"empty zero", [], 0, {error, <<"ListEmpty">>}},
        {"zero amount", [a], 0, {error, <<"ArgAmountIllegal">>}},
        {"negative amount", [a], -1, {error, <<"ArgAmountIllegal">>}},
        {"too many", [a], 2, {error, <<"NoEnoughListElems">>}},
        {"singleton", [a], 1, {ok, [a]}},
        {"full sample preserves order and duplicates", [c, a, a, b], 4,
         {ok, [c, a, a, b]}},
        {"partial sample preserves duplicates", [a, a, a], 2, {ok, [a, a]}}
    ]].


random_single_sample_test() ->
    ?assertEqual(undefined, ealgo:get_rand_elem([])),
    ?assertEqual(a, ealgo:get_rand_elem([a])),
    Seed = {51, 62, 73},
    Items = [a, a, b, c, 1, 1.0],
    rand:seed(exsplus, Seed),
    Draws = [ealgo:get_rand_elem(Items) || _ <- lists:seq(1, 100)],
    ?assert(lists:all(fun(X) -> lists:member(X, Items) end, Draws)),
    %% Re-seeding reproduces results without fixing an algorithm's sequence.
    rand:seed(exsplus, Seed),
    ?assertEqual(Draws, [ealgo:get_rand_elem(Items) || _ <- Draws]).


counts(Items) ->
    lists:foldl(fun(Item, Acc) ->
        maps:update_with(Item, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, Items).
