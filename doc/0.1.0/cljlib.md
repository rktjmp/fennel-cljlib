# Cljlib.fnl
Fennel-cljlib - functions from Clojure's core.clj implemented on top of Fennel.

Documentation for version: 0.1.0

## `add`
Function signature:

```
(add 
  ([a]) 
  ([a b]) 
  ([a b c]) 
  ([a b c d]) 
  ([a b c d & rest]))
```

Sum arbitrary amount of numbers.

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
arguments to `args`.

## `assoc`
Function signature:

```
(assoc 
  ([tbl k v]) 
  ([tbl k v & kvs]))
```

Associate key `k` with value `v` in `tbl`.

## `boolean?`
Function signature:

```
(boolean? [x])
```

Test if `x` is a Boolean

## `butlast`
Function signature:

```
(butlast [col])
```

Returns everything but the last element of a table as a new
  table. Calls `seq` on its argument.

## `comp`
Function signature:

```
(comp 
  ([f]) 
  ([f g]) 
  ([f g & fs]))
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

## `concat`
Function signature:

```
(concat 
  ([x]) 
  ([x y]) 
  ([x y & xs]))
```

Concatenate tables.

## `conj`
Function signature:

```
(conj 
  ([tbl]) 
  ([tbl x]) 
  ([tbl x & xs]))
```

Insert `x` as a last element of indexed table `tbl`. Modifies `tbl`

## `cons`
Function signature:

```
(cons [x tbl])
```

Insert `x` to `tbl` at the front. Modifies `tbl`.

## `constantly`
Function signature:

```
(constantly [x])
```

Returns a function that takes any number of arguments and returns `x`.

## `dec`
Function signature:

```
(dec [x])
```

Decrease number by one

## `div`
Function signature:

```
(div 
  ([a]) 
  ([a b]) 
  ([a b c]) 
  ([a b c d]) 
  ([a b c d & rest]))
```

Divide arbitrary amount of numbers.

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

## `eq`
Function signature:

```
(eq 
  ([x]) 
  ([x y]) 
  ([x y & xs]))
```

Deep compare values.

## `even?`
Function signature:

```
(even? [x])
```

Test if value is even.

## `every?`
Function signature:

```
(every? [pred tbl])
```

Test if every item in `tbl` satisfies the `pred`.

## `false?`
Function signature:

```
(false? [x])
```

Test if `x` is `false`

## `filter`
Function signature:

```
(filter [pred col])
```

Returns a sequential table of the items in `col` for which `pred`
  returns logical true.

## `first`
Function signature:

```
(first [col])
```

Return first element of a table. Calls `seq` on its argument.

## `ge`
Function signature:

```
(ge 
  ([x]) 
  ([x y]) 
  ([x y & more]))
```

Returns true if nums are in monotonically non-increasing order

## `get`
Function signature:

```
(get 
  ([tbl key]) 
  ([tbl key not-found]))
```

Get value from the table by accessing it with a `key`.
Accepts additional `not-found` as a marker to return if value wasn't
found in the table.

## `get-in`
Function signature:

```
(get-in 
  ([tbl keys]) 
  ([tbl keys not-found]))
```

Get value from nested set of tables by providing key sequence.
Accepts additional `not-found` as a marker to return if value wasn't
found in the table.

## `get-method`
Function signature:

```
(get-method [multifn dispatch-val])
```

Given a multimethod and a dispatch value, returns the dispatch `fn`
that would apply to that value, or `nil` if none apply and no default.

## `gt`
Function signature:

```
(gt 
  ([x]) 
  ([x y]) 
  ([x y & more]))
```

Returns true if nums are in monotonically increasing order

## `hash-map`
Function signature:

```
(hash-map 
  ([& kvs]))
```

Create associative table from keys and values

## `identity`
Function signature:

```
(identity [x])
```

Returns its argument.

## `inc`
Function signature:

```
(inc [x])
```

Increase number by one

## `int?`
Function signature:

```
(int? [x])
```

Test if `x` is a number without floating point data.

## `kvseq`
Function signature:

```
(kvseq [tbl])
```

Transforms any table kind to key-value sequence.

## `last`
Function signature:

```
(last [col])
```

Returns the last element of a table. Calls `seq` on its argument.

## `le`
Function signature:

```
(le 
  ([x]) 
  ([x y]) 
  ([x y & more]))
```

Returns true if nums are in monotonically non-decreasing order

## `lt`
Function signature:

```
(lt 
  ([x]) 
  ([x y]) 
  ([x y & more]))
```

Returns true if nums are in monotonically decreasing order

## `map?`
Function signature:

```
(map? [tbl])
```

Check whether `tbl` is an associative table.

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

Accepts arbitrary amount of tables, calls `seq` on each of it.
Function `f` must take the same amount of parameters as the amount of
tables passed to `mapv`. Applies `f` over first value of each
table. Then applies `f` to second value of each table. Continues until
any of the tables is exhausted. All remaining values are
ignored. Returns a table of results.

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

## `methods`
Function signature:

```
(methods [multifn])
```

Given a multimethod, returns a map of dispatch values -> dispatch fns

## `mul`
Function signature:

```
(mul 
  ([a]) 
  ([a b]) 
  ([a b c]) 
  ([a b c d]) 
  ([a b c d & rest]))
```

Multiply arbitrary amount of numbers.

## `neg-int?`
Function signature:

```
(neg-int? [x])
```

Test if `x` is a negetive integer.

## `neg?`
Function signature:

```
(neg? [x])
```

Test if `x` is less than zero.

## `nil?`
Function signature:

```
(nil? 
  ([x]))
```

Test if value is nil.

## `not-any?`
Function signature:

```
(not-any? pred tbl)
```

Test if no item in `tbl` satisfy the `pred`.

## `not-empty`
Function signature:

```
(not-empty [x])
```

If `x` is empty, returns `nil`, otherwise `x`.

## `odd?`
Function signature:

```
(odd? [x])
```

Test if value is odd.

## `pos-int?`
Function signature:

```
(pos-int? [x])
```

Test if `x` is a positive integer.

## `pos?`
Function signature:

```
(pos? [x])
```

Test if `x` is greater than zero.

## `range`
Function signature:

```
(range 
  ([upper]) 
  ([lower upper]) 
  ([lower upper step]))
```

return range of of numbers from `lower` to `upper` with optional `step`.

## `reduce`
Function signature:

```
(reduce 
  ([f col]) 
  ([f val col]))
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
val and f is not called.  Calls `seq` on `col`.

## `reduce-kv`
Function signature:

```
(reduce-kv [f val col])
```

Reduces an associative table using function `f` and initial value `val`.

`f` should be a function of 3 arguments.  Returns the result of
applying `f` to `val`, the first key and the first value in `coll`,
then applying `f` to that result and the 2nd key and value, etc.  If
`coll` contains no entries, returns `val` and `f` is not called.  Note
that reduce-kv is supported on sequential tables and strings, where
the keys will be the ordinals.

## `remove-all-methods`
Function signature:

```
(remove-all-methods [multifn])
```

Removes all of the methods of multimethod

## `remove-method`
Function signature:

```
(remove-method [multifn dispatch-val])
```

Remove method from `multifn` for given `dispatch-val`.

## `rest`
Function signature:

```
(rest [seq])
```

Returns table of all elements of a table but the first one. Calls
  `seq` on its argument.

## `reverse`
Function signature:

```
(reverse [tbl])
```

Returns table with same items as in `tbl` but in reverse order.

## `seq`
Function signature:

```
(seq [col])
```

Create sequential table.
Transforms original table to sequential table of key value pairs
stored as sequential tables in linear time.  If `col` is an
associative table, returns `[[key1 value1] ... [keyN valueN]]` table.
If `col` is sequential table, returns its shallow copy.

## `seq?`
Function signature:

```
(seq? [tbl])
```

Check whether `tbl` is an sequential table.

## `some`
Function signature:

```
(some [pred tbl])
```

Test if any item in `tbl` satisfies the `pred`.

## `string?`
Function signature:

```
(string? [x])
```

Test if `x` is a string.

## `sub`
Function signature:

```
(sub 
  ([a]) 
  ([a b]) 
  ([a b c]) 
  ([a b c d]) 
  ([a b c d & rest]))
```

Subtract arbitrary amount of numbers.

## `true?`
Function signature:

```
(true? [x])
```

Test if `x` is `true`

## `vector`
Function signature:

```
(vector [& args])
```

Constructs sequential table out of it's arguments.

## `zero?`
Function signature:

```
(zero? [x])
```

Test if value is zero.


<!-- Generated with Fenneldoc 0.0.1-->