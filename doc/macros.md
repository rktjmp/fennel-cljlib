# Macros (v0.5.4)
Macros for Cljlib that implement various facilities from Clojure.

**Table of contents**

- [`fn*`, `defn`](#fn-defn)
- [`try`](#try)
- [`def`](#def)
- [`defonce`](#defonce)
- [`defmulti`](#defmulti)
- [`defmethod`](#defmethod)
- [`into`](#into)
- [`empty`](#empty)
- [`with-meta`](#with-meta)
- [`meta`](#meta)
- [`if-let`](#if-let)
- [`when-let`](#when-let)
- [`if-some`](#if-some)
- [`when-some`](#when-some)
- [`loop`](#loop)

## `fn*`, `defn`
Function signature:

```
(fn* name docstring? ([arglist*] body)*)
```

Create (anonymous) function of fixed arity.
Accepts optional `name` and `docstring?` as first two arguments,
followed by single or multiple arity bodies defined as lists. Each
list starts with `arglist*` vector, which supports destructuring, and
is followed by `body*` wrapped in implicit `do`.

### Examples
Named function of fixed arity 2:

``` fennel
(fn* f [a b] (+ a b))
```

Function of fixed arities 1 and 2:

``` fennel
(fn* ([x] x)
     ([x y] (+ x y)))
```

Named function of 2 arities, one of which accepts 0 arguments, and the
other one or more arguments:

``` fennel
(fn* f
  ([] nil)
  ([x & xs]
   (print x)
   (f ((or table.unpack _G.unpack) xs))))
```

Note, that this function is recursive, and calls itself with less and
less amount of arguments until there's no arguments, and terminates
when the zero-arity body is called.

Named functions accept additional documentation string before the
argument list:

``` fennel
(fn* cube
     "raise `x` to power of 3"
     [x]
     (^ x 3))

(fn* greet
     "greet a `person`, optionally specifying default `greeting`."
     ([person] (print (.. "Hello, " person "!")))
     ([greeting person] (print (.. greeting ", " person "!"))))
```

Argument lists follow the same destruction rules as per `let`.
Variadic arguments with `...` are not supported use `& rest` instead.
Note that only one arity with `&` is supported.

##### Namespaces
If function name contains namespace part, defines local variable
without namespace part, then creates function with this name, sets
this function to the namespace, and returns it.

This roughly means, that instead of writing this:

``` fennel
(local ns {})

(fn f [x]                   ;; we have to define `f` without `ns`
  (if (> x 0) (f (- x 1)))) ;; because we're going to use it in `g`

(set ns.f f)

(fn ns.g [x] (f (* x 100))) ;; `g` can be defined as `ns.g` as it is only exported

ns
```

It is possible to write:

``` fennel
(local ns {})

(fn* ns.f [x]
  (if (> x 0) (f (- x 1))))

(fn* ns.g [x] (f (* x 100))) ;; we can use `f` here no problem

ns
```

It is still possible to call `f` and `g` in current scope without `ns`
part, so functions can be reused inside the module, and `ns` will hold
both functions, so it can be exported from the module.

Note that `fn` will not create the `ns` for you, hence this is just a
syntax sugar. Functions deeply nested in namespaces require exising
namespace tables:

``` fennel
(local ns {:strings {}
           :tables {}})

(fn* ns.strings.join
  ([s1 s2] (.. s1 s2))
  ([s1 s2 & strings]
   (join (join s1 s2) ((or table.unpack _G.unpack) strings)))) ;; call `join` resolves to ns.strings.join

(fn* ns.tables.join
  ([t1 t2]
   (let [res []]
     (each [_ v (ipairs t1)] (table.insert res v))
     (each [_ v (ipairs t2)] (table.insert res v))
     res))
  ([t1 t2 & tables]
   (join (join t1 t2) ((or table.unpack _G.unpack) tables)))) ;; call to `join` resolves to ns.tables.join

(assert-eq (ns.strings.join "a" "b" "c") "abc")

(assert-eq (join ["a"] ["b"] ["c"] ["d" "e"])
           ["a" "b" "c" "d" "e"])
(assert-eq (join "a" "b" "c")
           [])
```

Note that this creates a collision and local `join` overrides `join`
from `ns.strings`, so the latter must be fully qualified
`ns.strings.join` when called outside of the function.

## `try`
Function signature:

```
(try body* catch-clause* finally-clause?)
```

General purpose try/catch/finally macro.
Wraps its body in `pcall` and checks the return value with `match`
macro.

Catch clause is written either as `(catch symbol body*)`, thus acting
as catch-all, or `(catch value body*)` for catching specific errors.
It is possible to have several `catch` clauses.  If no `catch` clauses
specified, an implicit catch-all clause is created.  `body*`, and
inner expressions of `catch-clause*`, and `finally-clause?` are
wrapped in implicit `do`.

Finally clause is optional, and written as (finally body*).  If
present, it must be the last clause in the [`try`](#try) form, and the only
`finally` clause.  Note that `finally` clause is for side effects
only, and runs either after succesful run of [`try`](#try) body, or after any
`catch` clause body, before returning the result.  If no `catch`
clause is provided `finally` runs in implicit catch-all clause, and
trows error to upper scope using `error` function.

To throw error from [`try`](#try) to catch it with `catch` clause use `error`
or `assert` functions.

### Examples
Catch all errors, ignore those and return fallback value:

``` fennel
(fn add [x y]
  (try
    (+ x y)
    (catch _ 0)))

(assert-eq (add nil 1) 0)
```

Catch error and do cleanup:

``` fennel
(local tbl [])

(try
  (table.insert tbl "a")
  (table.insert tbl "b" "c")
  (catch _
    (each [k _ (pairs tbl)]
      (tset tbl k nil))))

(assert-eq (length tbl) 0)

```

Always run some side effect action:

``` fennel
(local t [])
(local res (try 10 (finally (table.insert t :finally))))
(assert-eq (. t 1) :finally)
(assert-eq res 10)

(local res (try (error 10) (catch 10 nil) (finally (table.insert t :again))))
(assert-eq (. t 2) :again)
(assert-eq res nil)
```


## `def`
Function signature:

```
(def attr-map? name expr)
```

Wrapper around `local` which can declare variables inside namespace,
and as local `name` at the same time similarly to
[`fn*`](#fn). Accepts optional `attr-map?` which can contain a
docstring, and whether variable should be mutable or not.  Sets
variable to the result of `expr`.

``` fennel
(def ns {})
(def a 10) ;; binds `a` to `10`

(assert-eq a 10)

(def ns.b 20) ;; binds `ns.b` and `b` to `20`

(assert-eq b 20)
(assert-eq ns.b 20)
```

`a` is a `local`, and both `ns.b` and `b` refer to the same value.

Additionally metadata can be attached to values, by providing
attribute map or keyword as first parameter.  Only one keyword is
supported, which is `:mutable`, which allows mutating variable with
`set` later on:

``` fennel
;; Bad, will override existing documentation for 299792458 (if any)
(def {:doc "speed of light in m/s"} c 299792458)

(def :mutable address "Lua St.") ;; same as (def {:mutable true} address "Lua St.")
(set address "Lisp St.") ;; can mutate `address`
```

However, attaching documentation metadata to anything other than
tables and functions considered bad practice, due to how Lua
works. More info can be found in [`with-meta`](#with-meta)
description.

## `defonce`
Function signature:

```
(defonce attr-map? name expr)
```

Works the same as [`def`](#def), but ensures that later [`defonce`](#defonce)
calls will not override existing bindings. Accepts same `attr-map?` as
[`def`](#def), and sets `name` to the result of `expr`:

``` fennel
(defonce a 10)
(defonce a 20)
(assert-eq a 10)
```

## `defmulti`
Function signature:

```
(defmulti name docstring? dispatch-fn options*)
```

Create multifunction `name` with runtime dispatching based on results
from `dispatch-fn`.  Returns a proxy table with `__call` metamethod,
that calls `dispatch-fn` on its arguments.  Amount of arguments
passed, should be the same as accepted by `dispatch-fn`.  Looks for
multimethod based on result from `dispatch-fn`.

Accepts optional `docstring?`, and `options*` arguments, where
`options*` is a sequence of key value pairs representing additional
attributes.  Supported options:

`:default` - the default dispatch value, defaults to `:default`.

By default, multifunction has no multimethods, see
[`defmethod`](#defmethod) on how to add one.

## `defmethod`
Function signature:

```
(defmethod multi-fn dispatch-value fnspec)
```

Attach new method to multi-function dispatch value. accepts the
`multi-fn` as its first argument, the `dispatch-value` as second, and
`fnspec` - a function tail starting from argument list, followed by
function body as in [`fn*`](#fn).

### Examples
Here are some examples how multimethods can be used.

#### Factorial example
Key idea here is that multimethods can call itself with different
values, and will dispatch correctly.  Here, `fac` recursively calls
itself with less and less number until it reaches `0` and dispatches
to another multimethod:

``` fennel
(defmulti fac (fn [x] x))

(defmethod fac 0 [_] 1)
(defmethod fac :default [x] (* x (fac (- x 1))))

(assert-eq (fac 4) 24)
```

`:default` is a special method which gets called when no other methods
were found for given dispatch value.

#### Multi-arity dispatching
Multi-arity function tails are also supported:

``` fennel
(defmulti foo (fn* ([x] [x]) ([x y] [x y])))

(defmethod foo [10] [_] (print "I've knew I'll get 10"))
(defmethod foo [10 20] [_ _] (print "I've knew I'll get both 10 and 20"))
(defmethod foo :default ([x] (print (.. "Umm, got" x)))
                        ([x y] (print (.. "Umm, got both " x " and " y))))
```

Calling `(foo 10)` will print `"I've knew I'll get 10"`, and calling
`(foo 10 20)` will print `"I've knew I'll get both 10 and 20"`.
However, calling `foo` with any other numbers will default either to
`"Umm, got x"` message, when called with single value, and `"Umm, got
both x and y"` when calling with two values.

#### Dispatching on object's type
We can dispatch based on types the same way we dispatch on values.
For example, here's a naive conversion from Fennel's notation for
tables to Lua's one:

``` fennel
(defmulti to-lua-str (fn [x] (type x)))

(defmethod to-lua-str :number [x] (tostring x))
(defmethod to-lua-str :table [x]
  (let [res []]
    (each [k v (pairs x)]
      (table.insert res (.. "[" (to-lua-str k) "] = " (to-lua-str v))))
    (.. "{" (table.concat res ", ") "}")))
(defmethod to-lua-str :string [x] (.. "\"" x "\""))
(defmethod to-lua-str :default [x] (tostring x))

(assert-eq (to-lua-str {:a {:b 10}}) "{[\"a\"] = {[\"b\"] = 10}}")

(assert-eq (to-lua-str [:a :b :c [:d {:e :f}]])
           "{[1] = \"a\", [2] = \"b\", [3] = \"c\", [4] = {[1] = \"d\", [2] = {[\"e\"] = \"f\"}}}")
```

And if we call it on some table, we'll get a valid Lua table, which we
can then reformat as we want and use in Lua.

All of this can be done with functions, and single entry point
function, that uses if statement and branches on the type, however one
of the additional features of multimethods, is that separate libraries
can extend such multimethod by adding additional claues to it without
needing to patch the source of the function.  For example later on
support for userdata or coroutines can be added to `to-lua-str`
function as a separate multimethods for respective types.

## `into`
Function signature:

```
(into to from)
```

Transform table `from` into another table `to`.  Mutates first table.

Transformation happens in runtime, but type deduction happens in
compile time if possible.  This means, that if literal values passed
to [`into`](#into) this will have different effects for associative tables and
vectors:

``` fennel
(assert-eq (into [1 2 3] [4 5 6]) [1 2 3 4 5 6])
(assert-eq (into {:a 1 :c 2} {:a 0 :b 1}) {:a 0 :b 1 :c 2})
```

Conversion between different table types is also supported:

``` fennel
(assert-eq (into [] {:a 1}) [[:a 1]])
(assert-eq (into {} [[:a 1] [:b 2]]) {:a 1 :b 2})
```

Same rules apply to runtime detection of table type, except that this
will not work for empty tables:

``` fennel
(local empty-table {})
(assert-eq (into empty-table {:a 1}) [[:a 1]])
``` fennel

If table is empty, [`into`](#into) defaults to sequential table, because it
allows safe conversion from both sequential and associative tables.

Type for non empty tables hidden in variables can be deduced at
runtime, and this works as expected:

``` fennel
(local t1 [1 2 3])
(local t2 {:a 10 :c 3})
(assert-eq (into t1 {:a 1}) [1 2 3 [:a 1]])
(assert-eq (into t2 {:a 1}) {:a 1 :c 3})
```

`cljlib.fnl` module provides two additional functions `vector` and
`hash-map`, that can create empty tables, which can be distinguished
at runtime:

``` fennel
(assert-eq (into (vector) {:a 1}) [[:a 1]])
(assert-eq (into (hash-map) [[:a 1] [:b 2]]) {:a 1 :b 2})
```

## `empty`
Function signature:

```
(empty x)
```

Return empty table of the same kind as input table `x`, with
additional metadata indicating its type.

### Example
Creating a generic `map` function, that will work on any table type,
and return result of the same type:

``` fennel
(fn map [f tbl]
  (let [res []]
    (each [_ v (ipairs (into [] tbl))]
      (table.insert res (f v)))
    (into (empty tbl) res)))

(assert-eq (map (fn [[k v]] [(string.upper k) v]) {:a 1 :b 2 :c 3})
           {:A 1 :B 2 :C 3})
(assert-eq (map #(* $ $) [1 2 3 4])
           [1 4 9 16])
```
See [`into`](#into) for more info on how conversion is done.

## `with-meta`
Function signature:

```
(with-meta value meta)
```

Attach [`meta`](#meta) to a `value`.

``` fennel
(local foo (with-meta (fn [...] (let [[x y z] [...]] (+ x y z)))
                      {:fnl/arglist ["x" "y" "z" "..."]
                       :fnl/docstring "sum first three values"}))
;; (doc foo)
;; => (foo x y z ...)
;; =>   sum first three values
```

## `meta`
Function signature:

```
(meta value)
```

Get `value` metadata.  If value has no metadata returns `nil`.

### Example

``` fennel
(meta (with-meta {} {:meta "data"}))
;; => {:meta "data"}
```

### Note
There are several important gotchas about using metadata.

First, note that this works only when used with Fennel, and only when
`(require fennel)` works.  For compiled Lua library this feature is
turned off.

Second, try to avoid using metadata with anything else than tables and
functions.  When storing function or table as a key into metatable,
its address is used, while when storing string of number, the value is
used.  This, for example, may cause documentation collision, when
you've set some variable holding a number value to have certain
docstring, and later you've defined another variable with the same
value, but different docstring.  While this isn't a major breakage, it
may confuse if someone will explore your code in the REPL with `doc`.

Lastly, note that prior to Fennel 0.7.1 `import-macros` wasn't
respecting `--metadata` switch.  So if you're using Fennel < 0.7.1
this stuff will only work if you use `require-macros` instead of
`import-macros`.

## `if-let`
Function signature:

```
(if-let [binding test] then-branch else-branch)
```

If `binding` is set by `test` to logical true, evaluates `then-branch`
with binding-form bound to the value of test, if not, yields
`else-branch`.

## `when-let`
Function signature:

```
(when-let [binding test] & body)
```

If `binding` was bound by `test` to logical true, evaluates `body` in
implicit `do`.

## `if-some`
Function signature:

```
(if-some [binding test] then-branch else-branch)
```

If `test` is non-`nil`, evaluates `then-branch` with `binding`-form bound
to the value of test, if not, yields `else-branch`.

## `when-some`
Function signature:

```
(when-some [binding test] & body)
```

If `test` sets `binding` to non-`nil`, evaluates `body` in implicit
`do`.


## `loop`
Function signature:

```
(loop binding-vec body*)
```

Recursive loop macro.

Similar to `let`, but binds a special `recur` call that will reassign the values
of the `binding-vec` and restart the loop `body*`.

The first argument is a binding table with alternating symbols (or destructure
forms), and the values to bind to them.

For example:

```fennel
(loop [[first & rest] [1 2 3 4 5]
       i 0]
  (if (= nil first)
      i
      (recur rest (+ 1 i))))
```

This would destructure the first table argument, with the first value inside it
being assigned to `first` and the remainder of the table being assigned to
`rest`. `i` simply gets bound to 0.

The body of the form executes for every item in the table, calling `recur` each
time with the table lacking its head element (thus consuming one element per
iteration), and with `i` being called with one value greater than the previous.

When the loop terminates (When the user doesn't call `recur`) it will return the
number of elements in the passed in table. (In this case, 5)


---

Copyright (C) 2020-2021 Andrey Listopadov

License: [MIT](https://gitlab.com/andreyorst/fennel-cljlib/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc v0.1.6
     https://gitlab.com/andreyorst/fenneldoc -->
