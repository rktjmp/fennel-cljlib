# Cljlib (v0.5.4)
Fennel-cljlib - functions from Clojure's core.clj implemented on top
of Fennel.

This library contains a set of functions providing functions that
behave similarly to Clojure's equivalents.  Library itself has nothing
Fennel specific so it should work on Lua, e.g:

``` lua
Lua 5.3.5  Copyright (C) 1994-2018 Lua.org, PUC-Rio
> clj = require"cljlib"
> table.concat(clj.mapv(function (x) return x * x end, {1, 2, 3}), " ")
-- 1 4 9
```

This example is mapping an anonymous `function` over a table,
producing new table and concatenating it with `" "`.

However this library also provides Fennel-specific set of
[macros](./macros.md), that provides additional facilities like
`defn` or `defmulti` which extend the language allowing writing code
that looks and works mostly like Clojure.

Each function in this library is created with `defn`, which is a
special macros for creating multi-arity functions.  So when you see
function signature like `(foo [x])`, this means that this is function
`foo`, that accepts exactly one argument `x`.  In contrary, functions
created with `fn` will produce `(foo x)` signature (`x` is not inside
brackets).

Functions, which signatures look like `(foo ([x]) ([x y]) ([x y &
zs]))`, it is a multi-arity function, which accepts either one, two,
or three-or-more arguments.  Each `([...])` represents different body
of a function which is chosen by checking amount of arguments passed
to the function.  See [Clojure's doc section on multi-arity
functions](https://clojure.org/guides/learn/functions#_multi_arity_functions).

## Compatibility
This library is mainly developed with Lua 5.4, and tested against
Lua 5.2, 5.3, 5.4, and LuaJIT 2.1.0-beta3.  Note, that in lua 5.2 and
LuaJIT equality semantics are a bit different from Lua 5.3 and Lua 5.4.
Main difference is that when comparing two tables, they must have
exactly the same `__eq` metamethods, so comparing hash sets with hash
sets will work, but comparing sets with other tables works only in
Lua5.3+.  Another difference is that Lua 5.2 and LuaJIT don't have
inbuilt UTF-8 library, therefore [`seq`](#seq) function will not work for
non-ASCII strings.

**Table of contents**

- [`apply`](#apply)
- [`add`](#add)
- [`sub`](#sub)
- [`mul`](#mul)
- [`div`](#div)
- [`le`](#le)
- [`lt`](#lt)
- [`ge`](#ge)
- [`gt`](#gt)
- [`inc`](#inc)
- [`dec`](#dec)
- [`eq`](#eq)
- [`map?`](#map)
- [`vector?`](#vector)
- [`multifn?`](#multifn)
- [`set?`](#set)
- [`nil?`](#nil)
- [`zero?`](#zero)
- [`pos?`](#pos)
- [`neg?`](#neg)
- [`even?`](#even)
- [`odd?`](#odd)
- [`string?`](#string)
- [`boolean?`](#boolean)
- [`true?`](#true)
- [`false?`](#false)
- [`int?`](#int)
- [`pos-int?`](#pos-int)
- [`neg-int?`](#neg-int)
- [`double?`](#double)
- [`empty?`](#empty)
- [`not-empty`](#not-empty)
- [`vector`](#vector-1)
- [`seq`](#seq)
- [`kvseq`](#kvseq)
- [`first`](#first)
- [`rest`](#rest)
- [`last`](#last)
- [`butlast`](#butlast)
- [`conj`](#conj)
- [`disj`](#disj)
- [`cons`](#cons)
- [`concat`](#concat)
- [`reduce`](#reduce)
- [`reduced`](#reduced)
- [`reduce-kv`](#reduce-kv)
- [`mapv`](#mapv)
- [`filter`](#filter)
- [`every?`](#every)
- [`some`](#some)
- [`not-any?`](#not-any)
- [`range`](#range)
- [`reverse`](#reverse)
- [`take`](#take)
- [`nthrest`](#nthrest)
- [`partition`](#partition)
- [`identity`](#identity)
- [`comp`](#comp)
- [`complement`](#complement)
- [`constantly`](#constantly)
- [`memoize`](#memoize)
- [`assoc`](#assoc)
- [`hash-map`](#hash-map)
- [`get`](#get)
- [`get-in`](#get-in)
- [`keys`](#keys)
- [`vals`](#vals)
- [`find`](#find)
- [`dissoc`](#dissoc)
- [`remove-method`](#remove-method)
- [`remove-all-methods`](#remove-all-methods)
- [`methods`](#methods)
- [`get-method`](#get-method)
- [`ordered-set`](#ordered-set)
- [`hash-set`](#hash-set)

## `apply`
Function signature:

```
(apply 
  ([f args]) 
  ([f a args]) 
  ([f a b args]) 
  ([f a b c args]) 
  ([f a b c d & args]))
```

Apply `f` to the argument list formed by prepending intervening
arguments to `args`, and `f` must support variadic amount of
arguments.

### Examples
Applying [`add`](#add) to different amount of arguments:

``` fennel
(assert-eq (apply add [1 2 3 4]) 10)
(assert-eq (apply add 1 [2 3 4]) 10)
(assert-eq (apply add 1 2 3 4 5 6 [7 8 9]) 45)
```

## `add`
Function signature:

```
(add ([]) ([a]) ([a b]) ([a b c]) ([a b c d]) ([a b c d & rest]))
```

Sum arbitrary amount of numbers.

## `sub`
Function signature:

```
(sub ([]) ([a]) ([a b]) ([a b c]) ([a b c d]) ([a b c d & rest]))
```

Subtract arbitrary amount of numbers.

## `mul`
Function signature:

```
(mul ([]) ([a]) ([a b]) ([a b c]) ([a b c d]) ([a b c d & rest]))
```

Multiply arbitrary amount of numbers.

## `div`
Function signature:

```
(div ([a]) ([a b]) ([a b c]) ([a b c d]) ([a b c d & rest]))
```

Divide arbitrary amount of numbers.

## `le`
Function signature:

```
(le ([a]) ([a b]) ([a b & [c d & more]]))
```

Returns true if nums are in monotonically non-decreasing order

## `lt`
Function signature:

```
(lt ([a]) ([a b]) ([a b & [c d & more]]))
```

Returns true if nums are in monotonically decreasing order

## `ge`
Function signature:

```
(ge ([a]) ([a b]) ([a b & [c d & more]]))
```

Returns true if nums are in monotonically non-increasing order

## `gt`
Function signature:

```
(gt ([a]) ([a b]) ([a b & [c d & more]]))
```

Returns true if nums are in monotonically increasing order

## `inc`
Function signature:

```
(inc [x])
```

Increase number `x` by one

## `dec`
Function signature:

```
(dec [x])
```

Decrease number `x` by one

## `eq`
Function signature:

```
(eq ([x]) ([x y]) ([x y & xs]))
```

Deep compare values.

### Examples

[`eq`](#eq) can compare both primitive types, tables, and user defined types
that have `__eq` metamethod.

``` fennel
(assert-is (eq 42 42))
(assert-is (eq [1 2 3] [1 2 3]))
(assert-is (eq (hash-set :a :b :c) (hash-set :a :b :c)))
(assert-is (eq (hash-set :a :b :c) (ordered-set :c :b :a)))
```

Deep comparison is used for tables which use tables as keys:

``` fennel
(assert-is (eq {[1 2 3] {:a [1 2 3]} {:a 1} {:b 2}}
               {{:a 1} {:b 2} [1 2 3] {:a [1 2 3]}}))
(assert-is (eq {{{:a 1} {:b 1}} {{:c 3} {:d 4}} [[1] [2 [3]]] {:a 2}}
               {[[1] [2 [3]]] {:a 2} {{:a 1} {:b 1}} {{:c 3} {:d 4}}}))
```

## `map?`
Function signature:

```
(map? [tbl])
```

Check whether `tbl` is an associative table.

Non empty associative tables are tested for two things:
- `next` returns the key-value pair,
- key, that is returned by the `next` is not equal to `1`.

Empty tables can't be analyzed with this method, and [`map?`](#map) will
return `false`.  If you need this test pass for empty table, see
[`hash-map`](#hash-map) for creating tables that have additional
metadata attached for this test to work.

### Examples
Non empty tables:

``` fennel
(assert-is (map? {:a 1 :b 2}))

(local some-table {:key :value})
(assert-is (map? some-table))
```

Empty tables:

``` fennel
(local some-table {})
(assert-not (map? some-table))
```

Empty tables created with [`hash-map`](#hash-map) will pass the test:

``` fennel
(local some-table (hash-map))
(assert-is (map? some-table))
```

## `vector?`
Function signature:

```
(vector? [tbl])
```

Check whether `tbl` is an sequential table.

Non empty sequential tables are tested for two things:
- `next` returns the key-value pair,
- key, that is returned by the `next` is equal to `1`.

Empty tables can't be analyzed with this method, and [`vector?`](#vector) will
always return `false`.  If you need this test pass for empty table,
see [`vector`](#vector-1) for creating tables that have additional
metadata attached for this test to work.

### Examples
Non empty vector:

``` fennel
(assert-is (vector? [1 2 3 4]))

(local some-table [1 2 3])
(assert-is (vector? some-table))
```

Empty tables:

``` fennel
(local some-table [])
(assert-not (vector? some-table))
```

Empty tables created with [`vector`](#vector-1) will pass the test:

``` fennel
(local some-table (vector))
(assert-is (vector? some-table))
```

## `multifn?`
Function signature:

```
(multifn? [mf])
```

Test if `mf` is an instance of `multifn`.

`multifn` is a special kind of table, created with `defmulti` macros
from `macros.fnl`.

## `set?`
Function signature:

```
(set? [s])
```

Test if `s` is either instance of a [`hash-set`](#hash-set) or [`ordered-set`](#ordered-set).

## `nil?`
Function signature:

```
(nil? ([]) ([x]))
```

Test if `x` is nil.

## `zero?`
Function signature:

```
(zero? [x])
```

Test if `x` is equal to zero.

## `pos?`
Function signature:

```
(pos? [x])
```

Test if `x` is greater than zero.

## `neg?`
Function signature:

```
(neg? [x])
```

Test if `x` is less than zero.

## `even?`
Function signature:

```
(even? [x])
```

Test if `x` is even.

## `odd?`
Function signature:

```
(odd? [x])
```

Test if `x` is odd.

## `string?`
Function signature:

```
(string? [x])
```

Test if `x` is a string.

## `boolean?`
Function signature:

```
(boolean? [x])
```

Test if `x` is a Boolean

## `true?`
Function signature:

```
(true? [x])
```

Test if `x` is `true`

## `false?`
Function signature:

```
(false? [x])
```

Test if `x` is `false`

## `int?`
Function signature:

```
(int? [x])
```

Test if `x` is a number without floating point data.

Number is rounded with `math.floor` and compared with original number.

## `pos-int?`
Function signature:

```
(pos-int? [x])
```

Test if `x` is a positive integer.

## `neg-int?`
Function signature:

```
(neg-int? [x])
```

Test if `x` is a negative integer.

## `double?`
Function signature:

```
(double? [x])
```

Test if `x` is a number with floating point data.

## `empty?`
Function signature:

```
(empty? [x])
```

Check if collection is empty.

## `not-empty`
Function signature:

```
(not-empty [x])
```

If `x` is empty, returns `nil`, otherwise `x`.

## `vector`
Function signature:

```
(vector [& args])
```

Constructs sequential table out of it's arguments.

Sets additional metadata for function [`vector?`](#vector) to work.

### Examples

``` fennel
(local v (vector 1 2 3 4))
(assert-eq v [1 2 3 4])
```

## `seq`
Function signature:

```
(seq [col])
```

Create sequential table.

Transforms original table to sequential table of key value pairs
stored as sequential tables in linear time.  If `col` is an
associative table, returns sequential table of vectors with key and
value.  If `col` is sequential table, returns its shallow copy.  If
`col` is string, return sequential table of its codepoints.

### Examples
Sequential tables remain as is:

``` fennel
(seq [1 2 3 4])
;; [1 2 3 4]
```

Associative tables are transformed to format like this `[[key1 value1]
... [keyN valueN]]` and order is non deterministic:

``` fennel
(seq {:a 1 :b 2 :c 3})
;; [[:b 2] [:a 1] [:c 3]]
```

See `into` macros for transforming this back to associative table.
Additionally you can use [`conj`](#conj) and [`apply`](#apply) with
[`hash-map`](#hash-map):

``` fennel
(apply conj (hash-map) [:c 3] [[:a 1] [:b 2]])
;; => {:a 1 :b 2 :c 3}
```

## `kvseq`
Function signature:

```
(kvseq [col])
```

Transforms any table `col` to key-value sequence.

## `first`
Function signature:

```
(first [col])
```

Return first element of a table. Calls [`seq`](#seq) on its argument.

## `rest`
Function signature:

```
(rest [col])
```

Returns table of all elements of a table but the first one. Calls
  [`seq`](#seq) on its argument.

## `last`
Function signature:

```
(last [col])
```

Returns the last element of a table. Calls [`seq`](#seq) on its argument.

## `butlast`
Function signature:

```
(butlast [col])
```

Returns everything but the last element of a table as a new
  table. Calls [`seq`](#seq) on its argument.

## `conj`
Function signature:

```
(conj ([]) ([tbl]) ([tbl x]) ([tbl x & xs]))
```

Insert `x` as a last element of a table `tbl`.

If `tbl` is a sequential table or empty table, inserts `x` and
optional `xs` as final element in the table.

If `tbl` is an associative table, that satisfies [`map?`](#map) test,
insert `[key value]` pair into the table.

Mutates `tbl`.

### Examples
Adding to sequential tables:

``` fennel
(conj [] 1 2 3 4)
;; => [1 2 3 4]
(conj [1 2 3] 4 5)
;; => [1 2 3 4 5]
```

Adding to associative tables:

``` fennel
(conj {:a 1} [:b 2] [:c 3])
;; => {:a 1 :b 2 :c 3}
```

Note, that passing literal empty associative table `{}` will not work:

``` fennel
(conj {} [:a 1] [:b 2])
;; => [[:a 1] [:b 2]]
(conj (hash-map) [:a 1] [:b 2])
;; => {:a 1 :b 2}
```

See [`hash-map`](#hash-map) for creating empty associative tables.

## `disj`
Function signature:

```
(disj ([s]) ([s k]) ([s k & ks]))
```

Remove key `k` from set `s`.

## `cons`
Function signature:

```
(cons [x tbl])
```

Insert `x` to `tbl` at the front.  Calls [`seq`](#seq) on `tbl`.

## `concat`
Function signature:

```
(concat ([]) ([x]) ([x y]) ([x y & xs]))
```

Concatenate tables.

## `reduce`
Function signature:

```
(reduce ([f col]) ([f val col]))
```

Reduce collection `col` using function `f` and optional initial value `val`.

`f` should be a function of 2 arguments.  If val is not supplied,
returns the result of applying f to the first 2 items in coll, then
applying f to that result and the 3rd item, etc.  If coll contains no
items, f must accept no arguments as well, and reduce returns the
result of calling f with no arguments.  If coll has only 1 item, it is
returned and f is not called.  If val is supplied, returns the result
of applying f to val and the first item in coll, then applying f to
that result and the 2nd item, etc.  If coll contains no items, returns
val and f is not called.  Calls [`seq`](#seq) on `col`.

Early termination is possible with the use of [`reduced`](#reduced)
function.

### Examples
Reduce sequence of numbers with [`add`](#add)

``` fennel
(reduce add [1 2 3 4])
;; => 10
(reduce add 10 [1 2 3 4])
;; => 20
```

## `reduced`
Function signature:

```
(reduced [x])
```

Wraps `x` in such a way so [`reduce`](#reduce) will terminate early
with this value.

### Examples
Stop reduction is result is higher than `10`:

``` fennel
(reduce (fn [res x]
          (if (>= res 10)
              (reduced res)
              (+ res x)))
        [1 2 3])
;; => 6

(reduce (fn [res x]
          (if (>= res 10)
              (reduced res)
              (+ res x)))
        [1 2 3 4 :nil])
;; => 10
```

Note that in second example we had `:nil` in the array, which is not a
valid number, but we've terminated right before we've reached it.

## `reduce-kv`
Function signature:

```
(reduce-kv [f val tbl])
```

Reduces an associative table using function `f` and initial value `val`.

`f` should be a function of 3 arguments.  Returns the result of
applying `f` to `val`, the first key and the first value in `tbl`,
then applying `f` to that result and the 2nd key and value, etc.  If
`tbl` contains no entries, returns `val` and `f` is not called.  Note
that reduce-kv is supported on sequential tables and strings, where
the keys will be the ordinals.

Early termination is possible with the use of [`reduced`](#reduced)
function.

### Examples
Reduce associative table by adding values from all keys:

``` fennel
(local t {:a1 1
          :b1 2
          :a2 2
          :b2 3})

(reduce-kv #(+ $1 $3) 0 t)
;; => 8
```

Reduce table by adding values from keys that start with letter `a`:

``` fennel
(local t {:a1 1
          :b1 2
          :a2 2
          :b2 3})

(reduce-kv (fn [res k v] (if (= (string.sub k 1 1) :a) (+ res v) res))
           0 t)
;; => 3
```

## `mapv`
Function signature:

```
(mapv 
  ([f col]) 
  ([f col1 col2]) 
  ([f col1 col2 col3]) 
  ([f col1 col2 col3 & cols]))
```

Maps function `f` over one or more collections.

Accepts arbitrary amount of collections, calls [`seq`](#seq) on each of it.
Function `f` must take the same amount of arguments as the amount of
tables, passed to [`mapv`](#mapv). Applies `f` over first value of each
table. Then applies `f` to second value of each table. Continues until
any of the tables is exhausted. All remaining values are
ignored. Returns a sequential table of results.

### Examples
Map `string.upcase` over the string:

``` fennel
(mapv string.upper "string")
;; => ["S" "T" "R" "I" "N" "G"]
```

Map [`mul`](#mul) over two tables:

``` fennel
(mapv mul [1 2 3 4] [1 0 -1])
;; => [1 0 -3]
```

Basic `zipmap` implementation:

``` fennel
(import-macros {: into} :init-macros)
(fn zipmap [keys vals]
  (into {} (mapv vector keys vals)))

(zipmap [:a :b :c] [1 2 3 4])
;; => {:a 1 :b 2 :c 3}
```

## `filter`
Function signature:

```
(filter [pred col])
```

Returns a sequential table of the items in `col` for which `pred`
  returns logical true.

## `every?`
Function signature:

```
(every? [pred tbl])
```

Test if every item in `tbl` satisfies the `pred`.

## `some`
Function signature:

```
(some [pred tbl])
```

Test if any item in `tbl` satisfies the `pred`.

## `not-any?`
Function signature:

```
(not-any? [pred tbl])
```

Test if no item in `tbl` satisfy the `pred`.

## `range`
Function signature:

```
(range ([upper]) ([lower upper]) ([lower upper step]))
```

return range of of numbers from `lower` to `upper` with optional `step`.

## `reverse`
Function signature:

```
(reverse [tbl])
```

Returns table with same items as in `tbl` but in reverse order.

## `take`
Function signature:

```
(take [n col])
```

Returns a sequence of the first `n` items in `col`, or all items if
there are fewer than `n`.

## `nthrest`
Function signature:

```
(nthrest [col n])
```

Returns the nth rest of `col`, `col` when `n` is 0.

### Examples

``` fennel
(assert-eq (nthrest [1 2 3 4] 3) [4])
(assert-eq (nthrest [1 2 3 4] 2) [3 4])
(assert-eq (nthrest [1 2 3 4] 1) [2 3 4])
(assert-eq (nthrest [1 2 3 4] 0) [1 2 3 4])
```


## `partition`
Function signature:

```
(partition ([n col]) ([n step col]) ([n step pad col]))
```

Returns a sequence of sequences of `n` items each, at offsets step
apart. If `step` is not supplied, defaults to `n`, i.e. the partitions
do not overlap. If a `pad` collection is supplied, use its elements as
necessary to complete last partition up to `n` items. In case there
are not enough padding elements, return a partition with less than `n`
items.

### Examples
Partition sequence into sub-sequences of size 3:

``` fennel
(assert-eq (partition 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
```

When collection doesn't have enough elements, partition will not include those:

``` fennel
(assert-eq (partition 3 [1 2 3 4]) [[1 2 3]])
```

Partitions can overlap if step is supplied:

``` fennel
(assert-eq (partition 2 1 [1 2 3 4]) [[1 2] [2 3] [3 4]])
```

Additional padding can be used to supply insufficient elements:

``` fennel
(assert-eq (partition 3 3 [3 2 1] [1 2 3 4]) [[1 2 3] [4 3 2]])
```

## `identity`
Function signature:

```
(identity [x])
```

Returns its argument.

## `comp`
Function signature:

```
(comp ([]) ([f]) ([f g]) ([f g & fs]))
```

Compose functions.

## `complement`
Function signature:

```
(complement [f])
```

Takes a function `f` and returns the function that takes the same
amount of arguments as `f`, has the same effect, and returns the
oppisite truth value.

## `constantly`
Function signature:

```
(constantly [x])
```

Returns a function that takes any number of arguments and returns `x`.

## `memoize`
Function signature:

```
(memoize [f])
```

Returns a memoized version of a referentially transparent function.
The memoized version of the function keeps a cache of the mapping from
arguments to results and, when calls with the same arguments are
repeated often, has higher performance at the expense of higher memory
use.

## `assoc`
Function signature:

```
(assoc ([tbl k v]) ([tbl k v & kvs]))
```

Associate key `k` with value `v` in `tbl`.

## `hash-map`
Function signature:

```
(hash-map ([]) ([& kvs]))
```

Create associative table from `kvs` represented as sequence of keys
and values

## `get`
Function signature:

```
(get ([tbl key]) ([tbl key not-found]))
```

Get value from the table by accessing it with a `key`.
Accepts additional `not-found` as a marker to return if value wasn't
found in the table.

## `get-in`
Function signature:

```
(get-in ([tbl keys]) ([tbl keys not-found]))
```

Get value from nested set of tables by providing key sequence.
Accepts additional `not-found` as a marker to return if value wasn't
found in the table.

## `keys`
Function signature:

```
(keys [tbl])
```

Returns a sequence of the table's keys, in the same order as [`seq`](#seq).

## `vals`
Function signature:

```
(vals [tbl])
```

Returns a sequence of the table's values, in the same order as [`seq`](#seq).

## `find`
Function signature:

```
(find [tbl key])
```

Returns the map entry for `key`, or `nil` if key not present in `tbl`.

## `dissoc`
Function signature:

```
(dissoc ([tbl]) ([tbl key]) ([tbl key & keys]))
```

Remove `key` from table `tbl`.  Optionally takes more `keys`.

## `remove-method`
Function signature:

```
(remove-method [multimethod dispatch-value])
```

Remove method from `multimethod` for given `dispatch-value`.

## `remove-all-methods`
Function signature:

```
(remove-all-methods [multimethod])
```

Removes all of the methods of `multimethod`

## `methods`
Function signature:

```
(methods [multimethod])
```

Given a `multimethod`, returns a map of dispatch values -> dispatch fns

## `get-method`
Function signature:

```
(get-method [multimethod dispatch-value])
```

Given a `multimethod` and a `dispatch-value`, returns the dispatch
`fn` that would apply to that value, or `nil` if none apply and no
default.

## `ordered-set`
Function signature:

```
(ordered-set [& xs])
```

Create ordered set.

Set is a collection of unique elements, which sore purpose is only to
tell you if something is in the set or not.

[`ordered-set`](#ordered-set) is follows the argument insertion order, unlike sorted
sets, which apply some sorting algorithm internally. New items added
at the end of the set. Ordered set supports removal of items via
`tset` and [`disj`](#disj). To add element to the ordered set use
`tset` or [`conj`](#conj). Both operations modify the set.

**Note**: Hash set prints as `@set{a b c}`, but this construct is not
supported by the Fennel reader, so you can't create sets with this
syntax. Use [`ordered-set`](#ordered-set) function instead.

Below are some examples of how to create and manipulate sets.

#### Create ordered set:
Ordered sets are created by passing any amount of elements desired to
be in the set:

``` fennel
(ordered-set)
;; => @set{}
(ordered-set :a :c :b)
;; => @set{:a :c :b}
```

Duplicate items are not added:

``` fennel
(ordered-set :a :c :a :a :a :a :c :b)
;; => @set{:a :c :b}
```

#### Check if set contains desired value:
Sets are functions of their keys, so simply calling a set with a
desired key will either return the key, or `nil`:

``` fennel
(local oset (ordered-set [:a :b :c] [:c :d :e] :e :f))
(oset [:a :b :c])
;; => ["a" "b" "c"]
(. oset :e)
;; "e"
(oset [:a :b :f])
;; => nil
```

#### Add items to existing set:
To add element to the set use [`conj`](#conj) or `tset`

``` fennel
(local oset (ordered-set :a :b :c))
(conj oset :d :e)
;; => @set{:a :b :c :d :e}
```

##### Remove items from the set:
To add element to the set use [`disj`](#disj) or `tset`

``` fennel
(local oset (ordered-set :a :b :c))
(disj oset :b)
;; => @set{:a :c}
(tset oset :a nil)
oset
;; => @set{:c}
```

#### Equality semantics
Both [`ordered-set`](#ordered-set) and [`hash-set`](#hash-set) implement `__eq` metamethod,
and are compared for having the same keys without particular order and
same size:

``` fennel
(assert-eq (ordered-set :a :b) (ordered-set :b :a))
(assert-ne (ordered-set :a :b) (ordered-set :b :a :c))
(assert-eq (ordered-set :a :b) (hash-set :a :b))
```

## `hash-set`
Function signature:

```
(hash-set [& xs])
```

Create hash set.

Set is a collection of unique elements, which sore purpose is only to
tell you if something is in the set or not.

Hash set differs from ordered set in that the keys are do not have any
particular order. New items are added at the arbitrary position by
using [`conj`](#conj) or `tset` functions, and items can be removed
with [`disj`](#disj) or `tset` functions. Rest semantics are the same
as for [`ordered-set`](#ordered-set)

**Note**: Hash set prints as `@set{a b c}`, but this construct is not
supported by the Fennel reader, so you can't create sets with this
syntax. Use [`hash-set`](#hash-set) function instead.


---

Copyright (C) 2020-2021 Andrey Listopadov

License: [MIT](https://gitlab.com/andreyorst/fennel-cljlib/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc v0.1.6
     https://gitlab.com/andreyorst/fenneldoc -->
