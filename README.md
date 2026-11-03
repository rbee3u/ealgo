# An Erlang Algorithm Library

Although the standard library of Erlang has become more and more complete, I have found that the support for various common algorithms is far from enough. This is an Erlang algorithm library similar to the `<algorithm>` in C++/STL, which is intended to supplement the shortcomings of the standard library.


<h3>Getting Started</h3>

With Erlang/OTP and `rebar3` installed, run these commands from the repository root:

```sh
rebar3 compile
rebar3 eunit
rebar3 shell
```

In the Erlang shell:

```erlang
1> ealgo_ntl:prime_list(10).
[2,3,5,7]
2> ealgo:combinations([a, b, c], 2).
[[a,b],[a,c],[b,c]]
3> ealgo:rabin_karp("banana", "ana").
[2,4]
4> ealgo_shield:detect("a bad word", ealgo_shield:create(["bad"])).
true
```

String arguments are Erlang character lists, such as `"text"`. Convert UTF-8
binaries with `unicode:characters_to_list/1` before passing them to string APIs.


<h3>Index</h3>

* [Number Theory](#number-theory)
* [Combinatorial](#combinatorial)
* [Random](#random)
* [Shielding Words](#shielding-words)
* [Miscs](#miscs)


<h3 id='number-theory'>Number Theory</h3>

* `ealgo_ntl:prime_list(N)`: Returns primes up to and including `N` in ascending
  order; returns `[]` when `N < 2`.
* `ealgo_ntl:prime_array(N)`: Returns the same primes as an Erlang `array`, indexed
  from zero. The entries are prime values, not primality flags.
* `ealgo_ntl:divisible(N, M)`: Returns true if `N` is divisible by `M`. `M` must be
  nonzero.
* `ealgo_ntl:is_odd(N)`: Returns whether integer `N` is odd. Non-integer input
  raises a `function_clause` error.
* `ealgo_ntl:is_even(N)`: Returns whether integer `N` is even. Non-integer input
  raises a `function_clause` error.
* `ealgo_ntl:multiplicity(N, A)`: Returns the largest exponent `K` such that
  `A^K` divides `N`. Requires `N != 0` and `A > 1`; for example,
  `multiplicity(72, 2)` returns `3`.
* `ealgo_ntl:quotient(N, M)`: Returns `floor(N / M)` for nonzero `M`.
* `ealgo_ntl:remainder(N, M)`: Returns `N - M * quotient(N, M)`. A nonzero result
  has the same sign as `M`, which must be nonzero.
* `ealgo_ntl:quotient_remainder(N, M)`: Returns `{Q, R}` using the definitions
  above. These differ from Erlang's `div` and `rem` when the operands have
  opposite signs and division is not exact: `quotient_remainder(-10, 3)` returns
  `{-4, 2}`.
* `ealgo_ntl:gcd(N, M)`: Returns the nonnegative
  [greatest common divisor](https://en.wikipedia.org/wiki/Greatest_common_divisor)
  of integers `N` and `M`. `gcd(0, 0)` returns `0`.
* `ealgo_ntl:gcd(L)`: Returns the nonnegative greatest common divisor of a list
  of integers. `gcd([X])` returns `abs(X)`; `gcd([])` returns `0`.
* `ealgo_ntl:lcm(N, M)`: Returns the nonnegative
  [least common multiple](https://en.wikipedia.org/wiki/Least_common_multiple)
  of integers `N` and `M`. Returns `0` if either argument is zero.
* `ealgo_ntl:lcm(L)`: Returns the nonnegative least common multiple of a list
  of integers. `lcm([X])` returns `abs(X)`; `lcm([])` returns `1`.
* `ealgo_ntl:power(A, X)`: Returns `A^X` for integer `A` and nonnegative integer
  `X`. `power(0, 0)` raises an `if_clause` error.
* `ealgo_ntl:power_mod(A, X, M)`: Returns `remainder(A^X, M)` for integer `A`,
  nonnegative integer `X`, and nonzero integer `M`. `power_mod(0, 0, M)` raises
  an `if_clause` error for valid `M`.
* `fun ealgo_ntl:is_coprime/2`: Gives true if `N` and `M` are [relatively prime](https://en.wikipedia.org/wiki/Coprime_integers), and false otherwise. relation: `is_coprime(N, M) = true  <=>  gcd(N, M) = 1`
* `ealgo_ntl:jacobi_symbol(A, N)`: Returns the
  [Jacobi symbol](https://en.wikipedia.org/wiki/Jacobi_symbol) `(A / N)` for
  integer `A` and positive odd integer `N`.
* `ealgo_ntl:is_prime(N)`: Returns false for `N < 2` or a detected composite.
  Below `1,000,000`, trial division gives an exact primality result. For larger
  integers, true means a probable prime: the implementation combines trial
  division, a base-2 [Miller–Rabin test](https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test),
  a square check, and a [strong Lucas test](https://en.wikipedia.org/wiki/Lucas_pseudoprime).
* `ealgo_ntl:bit_length(N)`: Returns the number of bits needed to represent
  `abs(N)` for integer `N`. `bit_length(0)` returns `0`; `bit_length(-7)` returns `3`.
* `ealgo_ntl:nthrootrem(X, N)`: Returns `{Y, R}`, where `Y` is the largest integer
  with `Y^N <= X` and `R = X - Y^N`. Requires nonnegative integer `X` and positive
  integer `N`; for example, `nthrootrem(20, 2)` returns `{4, 4}`.
* `ealgo_ntl:extended_gcd(N, M)`: Returns `{G, {A, B}}`, where
  `G = gcd(N, M) = A*N + B*M`. Returns `{0, {0, 0}}` for `(0, 0)`.



<h3 id='combinatorial'>Combinatorial</h3>

* `fun ealgo:cartesian_product/1`: Gives the [cartesian product](https://en.wikipedia.org/wiki/Cartesian_product) of a list of lists.
* `fun ealgo:combinations/1`: Gives all combinations of `L`.
* `fun ealgo:combinations/2`: Gives all combinations of `L` containing exactly `N` elements.
* `fun ealgo:permutations/1`: Gives all permutations of `L`.
* `fun ealgo:permutations/2`: Gives all permutations of `L` containing exactly `N` elements.
* `ealgo:next_permutation(L)`: Returns `{true, Next}` for the next lexicographic
  permutation under Erlang term ordering, or `false` if there is no successor.

Combinations and permutations treat list positions as distinct, so repeated
values can produce repeated results. The two-argument forms require a
nonnegative integer `N`; `N = 0` returns `[[]]`, and `N > length(L)` returns `[]`.
All results are built eagerly, so large inputs can use substantial memory.


<h3 id='random'>Random</h3>

Random functions use the calling process's `rand` state. Use `rand:seed/2` in
that process when reproducible draws are needed.

* `ealgo:shuffle(L)`: Returns a uniformly shuffled list, preserving duplicates.
* `ealgo:select_by_weight(L)`: Accepts `{Key, Weight}` pairs with nonnegative
  integer weights. Returns `{ok, Key}` with probability proportional to its
  weight; zero-weight entries are not drawn. Returns
  `{error, <<"AllWeightMustGreaterThanZero">>}` if the total weight is zero.
* `ealgo:select_amount_by_weight(L, N)`: Returns `{ok, Keys}` by weighted sampling
  without replacement. Use nonnegative integer weights and keys distinct under
  Erlang's `==` comparison (`1` and `1.0` count as the same key). For
  `0 < N < length(L)`, at least `N` entries must have positive weight; otherwise
  sampling can raise a `badmatch` error. Partial samples are returned in reverse
  draw order. If `N = length(L)`, all keys are returned in input order regardless
  of their weights. Nonpositive `N` returns
  `{error, <<"AmountMustGreaterThanZero">>}`; `N > length(L)` returns
  `{error, <<"ListLengthMustGreaterThanAmount">>}`. `N` must be an integer.
* `ealgo:get_rand_elem(L)`: Returns a uniformly chosen element, or `undefined`
  for an empty list.
* `ealgo:get_rand_elems(L, N)`: Returns `{ok, Elements}` by uniformly sampling
  list positions without replacement. Duplicate values can appear in the
  result. `N` must be a positive integer; if `N = length(L)`, a nonempty `L` is
  returned in input order. An empty list returns `{error, <<"ListEmpty">>}`;
  otherwise, nonpositive `N` returns `{error, <<"ArgAmountIllegal">>}`, and
  `N > length(L)` returns `{error, <<"NoEnoughListElems">>}`.


<h3 id='shielding-words'>Shielding Words</h3>

* `ealgo_shield:create(Words)`: Builds a trie from a list of character lists.
* `ealgo_shield:detect(Sentence, Trie)`: Returns whether `Sentence` contains any
  word in `Trie` as an exact, case-sensitive substring. No word boundaries or
  Unicode normalization are applied. An empty dictionary matches nothing; a
  dictionary containing `""` matches every sentence, including `""`.


<h3 id='miscs'>Miscs</h3>

* `ealgo:rabin_karp(String, Pattern)`: Uses
  [Rabin–Karp](https://en.wikipedia.org/wiki/Rabin%E2%80%93Karp_algorithm) to return
  matching start positions in ascending order, counting character-list elements
  from one and including overlapping matches. An empty pattern matches positions
  `1` through `length(String) + 1`.
* `ealgo:rabin_karp(String, Pattern, N)`: Returns at most the first `N` matches.
  Integer `N < 0` returns all matches; `N = 0` returns `[]`.
* `fun ealgo:sgn/1`: [Sign Function](https://en.wikipedia.org/wiki/Sign_function) gives `-1`, `0`, or `1` depending on whether `X` is negative, zero, or positive.
* `fun ealgo:boole/1`: [Indicator Function](https://en.wikipedia.org/wiki/Indicator_function) yields `1` if `X` is true and `0` if it is false.
* `fun ealgo:ustep/1`: [Heaviside Step Function](https://en.wikipedia.org/wiki/Heaviside_step_function) Represents the unit step function, equal to `0` for `X < 0` and `1` for `X >= 0`.
* `fun ealgo:id/1`: [Identity Function](https://en.wikipedia.org/wiki/Identity_function) Gives `X` (the identity operation).

