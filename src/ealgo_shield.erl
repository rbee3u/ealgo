-module(ealgo_shield).
-export([
      create/1
    , detect/2
]).


%% Create a trie tree from a given list of shielding words.
-spec create(ShieldingWordList) -> ShieldingTrie when
    ShieldingWordList :: [string()],
    ShieldingTrie :: map().
create(ShieldingWordList) ->
    lists:foldl(fun insert/2, #{}, ShieldingWordList).


%% Insert a shielding word into the shielding trie.
-spec insert(ShieldingWord, ShieldingTrie1) -> ShieldingTrie2 when
    ShieldingWord :: string(),
    ShieldingTrie1 :: map(),
    ShieldingTrie2 :: map().
insert([     ], ShieldingNode) ->
    ShieldingNode#{ending => true};
insert([H | T], ShieldingNode) ->
    Child = maps:get(H, ShieldingNode, #{}),
    ShieldingNode#{H => insert(T, Child)}.


%% Detect whether the sentence contains a shielding word.
-spec detect(Sentence, ShieldingTrie) -> boolean() when
    Sentence :: string(),
    ShieldingTrie :: map().
detect([      ] = S, ShieldingTrie) ->
    lookup(S, ShieldingTrie);
detect([_H | T] = S, ShieldingTrie) ->
    case lookup(S, ShieldingTrie) of
        true  -> true;
        false -> detect(T, ShieldingTrie)
    end.


%% Judge whether the sentence has a prefix is a shielding word.
lookup( _S,  #{ending := true}) -> true;
lookup([     ], _ShieldingNode) -> false;
lookup([H | T],  ShieldingNode) ->
    case ShieldingNode of
        #{H := Child} -> lookup(T, Child);
        _             -> false
    end.
