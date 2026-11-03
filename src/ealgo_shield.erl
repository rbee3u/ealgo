-module(ealgo_shield).
-export([
      create/1
    , detect/2
]).


%% Builds a trie from character lists for use with detect/2. An empty word list
%% matches nothing; including an empty word makes every sentence match.
-spec create(ShieldingWordList) -> ShieldingTrie when
    ShieldingWordList :: [string()],
    ShieldingTrie :: map().
create(ShieldingWordList) ->
    lists:foldl(fun insert/2, #{}, ShieldingWordList).


-spec insert(ShieldingWord, ShieldingTrie1) -> ShieldingTrie2 when
    ShieldingWord :: string(),
    ShieldingTrie1 :: map(),
    ShieldingTrie2 :: map().
insert([     ], ShieldingNode) ->
    ShieldingNode#{ending => true};
insert([H | T], ShieldingNode) ->
    Child = maps:get(H, ShieldingNode, #{}),
    ShieldingNode#{H => insert(T, Child)}.


%% Returns whether Sentence contains any word in a trie returned by create/1.
%% Matching uses exact, case-sensitive character-list substrings, without word
%% boundaries or Unicode normalization. An empty word matches an empty sentence.
-spec detect(Sentence, ShieldingTrie) -> boolean() when
    Sentence :: string(),
    ShieldingTrie :: map().
detect([], ShieldingTrie) ->
    lookup([], ShieldingTrie);
detect([_ | Rest] = Sentence, ShieldingTrie) ->
    lookup(Sentence, ShieldingTrie) orelse detect(Rest, ShieldingTrie).


lookup( _S,  #{ending := true}) -> true;
lookup([     ], _ShieldingNode) -> false;
lookup([H | T],  ShieldingNode) ->
    case ShieldingNode of
        #{H := Child} -> lookup(T, Child);
        _             -> false
    end.
