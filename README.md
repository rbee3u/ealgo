# An Erlang Algorithm Library

Although the standard library of Erlang has become more and more complete, I have found that the support for various common algorithms is far from enough. This is an Erlang algorithm library similar to the `<algorithm>` in C++/STL, which is intended to supplement the shortcomings of the standard library.


<h3>Index</h3>

* [Number Theory](#number-theory)
* [Combinatorial](#combinatorial)
* [Random](#random)
* [Shielding Words](#shielding-words)
* [Miscs](#miscs)


<h3 id='number-theory'>Number Theory</h3>

* `fun ealgo_ntl:prime_list/1`: Generates a prime list where any element is not greater than `N`.
* `fun ealgo_ntl:prime_array/1`: Generates a prime array where any element is not greater than `N`.
* `fun ealgo_ntl:divisible/2`: Gives true if `N` is divisible by `M`, and false otherwise.
* `fun ealgo_ntl:is_odd/1`: Gives true if `N` is an odd integer, and false otherwise.
* `fun ealgo_ntl:is_even/1`: Gives true if `N` is an even integer, and false otherwise.
* `fun ealgo_ntl:multiplicity/2`: Gives the highest power of `A` that divides `N`.
* `fun ealgo_ntl:quotient/2`: Gives the integer quotient of `N` and `M`.
* `fun ealgo_ntl:remainder/2`: Gives the remainder on division of `N` by `M`.
* `fun ealgo_ntl:quotient_remainder/2`: Gives the quotient and remainder from division of `N` by `M`.
* `fun ealgo_ntl:gcd/2`: Gives the [greatest common divisor](https://en.wikipedia.org/wiki/Greatest_common_divisor) of `N` and `M`. note: we define `gcd(0, 0) = 0` for convenience.
* `fun ealgo_ntl:gcd/1`: Gives the [greatest common divisor](https://en.wikipedia.org/wiki/Greatest_common_divisor) of a list of integers. If the list has only one integer `X`, it returns `gcd(X, 0)`; If the list is empty, for this corner case, it returns `0`.
* `fun ealgo_ntl:lcm/2`: Gives the [least common multiple](https://en.wikipedia.org/wiki/Least_common_multiple) of `N` and `M`. note: we define `lcm(X, Y) = 0` when `X = 0` or `y = 0`.
* `fun ealgo_ntl:lcm/1`: Gives the [least common multiple](https://en.wikipedia.org/wiki/Least_common_multiple) of a list of integers. If the list has only one integer `X`, it returns `lcm(X, 1)`; If the list is empty, for this corner case, it returns `1`.
* `fun ealgo_ntl:power/2`: Gives `A` to the power `X` where `A` is an integer and `X` is a nonnegative integer. note: `power(0, 0)` is undefined.
* `fun ealgo_ntl:power_mod/3`: Gives `A ^ X mod M` where `A` is an integer, `X` is a nonnegative integer and `M` is a nonzero integer. note: `power_mod(0, 0, M)` is undefined.
* `fun ealgo_ntl:is_coprime/2`: Gives true if `N` and `M` are [relatively prime](https://en.wikipedia.org/wiki/Coprime_integers), and false otherwise. relation: `is_coprime(N, M) = true  <=>  gcd(N, M) = 1`
* `fun ealgo_ntl:jacobi_symbol/2`: Gives the [Jacobi symbol](https://en.wikipedia.org/wiki/Jacobi_symbol) `(A / N)`..
* `fun ealgo_ntl:is_prime/1`: Gives true if `N` is a [prime number](https://en.wikipedia.org/wiki/Prime_number), and false otherwise, mainly implemented with [miller rabin test](https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test) and [strong lucas test](https://en.wikipedia.org/wiki/Lucas_pseudoprime).
* `fun ealgo_ntl:bit_length/1`: Gives the number of binary bits necessary to represent the integer `N`. In other words, the integer `S` satisfied that `2^(S-1) ≤ |N| < 2^S`, and we defined `S = 0` when `N = 0` for the corner case.
* `fun ealgo_ntl:nthrootrem/2`: Gives the `N'th` root and remainder of integer `X`.
* `fun ealgo_ntl:extended_gcd/2`: Gives the extended greatest common divisor of N and M. note: `extended_gcd(0, 0) = {0, {0, 0}}` for convenience.



<h3 id='combinatorial'>Combinatorial</h3>

* `fun ealgo:cartesian_product/1`: Gives the [cartesian product](https://en.wikipedia.org/wiki/Cartesian_product) of a list of list.
* `fun ealgo:combinations/1`: Gives all combinations of `L`.
* `fun ealgo:combinations/2`: Gives all combinations of `L` containing exactly `N` elements.
* `fun ealgo:permutations/1`: Gives all permutations of `L`.
* `fun ealgo:permutations/2`: Gives all permutations of `L` containing exactly `N` elements.
* `fun ealgo:next_permutation/1`: Gives the next permutation of `L`.


<h3 id='random'>Random</h3>

* `fun ealgo:shuffle/1`: Shuffle the list `L` randomly.
* `fun ealgo:select_by_weight/1`: Select one element from list `L` by weight.
* `fun ealgo:select_amount_by_weight/2`: Select `N` element from list `L` by weight.
* `fun ealgo:get_rand_elem/1`: Select one element from list `L` randomly.
* `fun ealgo:get_rand_elems/2`: Select `N` element from list `L` randomly.


<h3 id='shielding-words'>Shielding Words</h3>

* `fun ealgo_shield:create/1`: Create a trie tree from a given list of shielding words.
* `fun ealgo_shield:insert/2`: Insert a shielding word into the shielding trie.
* `fun ealgo_shield:detect/2`: Detect whether the sentence contains a shielding word.
* `fun ealgo_shield:lookup/2`: Judge whether the sentence has a prefix is a shielding word.


<h3 id='miscs'>Miscs</h3>

* `fun ealgo:rabin_karp/2`: [Rabin Karp](https://en.wikipedia.org/wiki/Rabin%E2%80%93Karp_algorithm) gives a list of the starting character positions at which `Pattern` appears as a substring of `String`.
* `fun ealgo:sgn/1`: [Sign Function](https://en.wikipedia.org/wiki/Sign_function) gives `-1`, `0`, or `1` depending on whether `X` is negative, zero, or positive.
* `fun ealgo:boole/1`: [Indicator Function](https://en.wikipedia.org/wiki/Indicator_function) yields `1` if `X` is true and `0` if it is false.
* `fun ealgo:ustep/1`: [Heaviside Step Function](https://en.wikipedia.org/wiki/Heaviside_step_function) Represents the unit step function, equal to `0` for `X < 0` and `1` for `X >= 0`.
* `fun ealgo:id/1`: [Identity Function](https://en.wikipedia.org/wiki/Identity_function) Gives `X` (the identity operation).

