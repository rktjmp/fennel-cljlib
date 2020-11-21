# Cljlib-macros.fnl (0.3.0)
Macros for Cljlib that implement various facilities from Clojure.

**Table of contents**

- [`def`](#def)
- [`defmethod`](#defmethod)
- [`defmulti`](#defmulti)
- [`defonce`](#defonce)
- [`empty`](#empty)
- [`fn*`](#fn*)
- [`if-let`](#if-let)
- [`if-some`](#if-some)
- [`into`](#into)
- [`meta`](#meta)
- [`when-let`](#when-let)
- [`when-meta`](#when-meta)
- [`when-some`](#when-some)
- [`with-meta`](#with-meta)

## `def`
Function signature:

```
(def attr-map? name expr)
```

Wrapper around `local` which can
declare variables inside namespace, and as local at the same time
similarly to [`fn*`](#fn*):

``` fennel
(def ns {})
(def a 10) ;; binds `a` to `10`

(def ns.b 20) ;; binds `ns.b` and `b` to `20`
```

`a` is a `local`, and both `ns.b` and `b` refer to the same value.

Additionally metadata can be attached to values, by providing
attribute map or keyword as first parameter.  Only one keyword is
supported, which is `:mutable`, which allows mutating variable with
`set` later on:

``` fennel
;; Bad, will override existing documentation for 299792458 (if any)
(def {:doc "speed of light in m/s"} c 299792458)
(set c 0) ;; => error, can't mutate `c`

(def :mutable address "Lua St.") ;; same as (def {:mutable true} address "Lua St.")
(set address "Lisp St.") ;; can mutate `address`
```

However, attaching documentation metadata to anything other than
tables and functions considered bad practice, due to how Lua
works. More info can be found in [`with-meta`](#with-meta)
description.

## `defmethod`
Function signature:

```
(defmethod multifn dispatch-val fnspec)
```

Attach new method to multi-function dispatch value. accepts the `multi-fn`
as its first argument, the dispatch value as second, and function tail
starting from argument list, followed by function body as in
[`fn*`](#fn).

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

(fac 4) ;; => 24
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
(defmethod to-lua-str :table [x] (let [res []]
                                   (each [k v (pairs x)]
                                     (table.insert res (.. "[" (to-lua-str k) "] = " (to-lua-str v))))
                                   (.. "{" (table.concat res ", ") "}")))
(defmethod to-lua-str :string [x] (.. "\"" x "\""))
(defmethod to-lua-str :default [x] (tostring x))
```

And if we call it on some table, we'll get a valid Lua table:

``` fennel
(print (to-lua-str {:a {:b 10}}))
;; prints {["a"] = {["b"] = 10}}

(print (to-lua-str [:a :b :c [:d {:e :f}]]))
;; prints {[1] = "a", [2] = "b", [3] = "c", [4] = {[1] = "d", [2] = {["e"] = "f"}}}
```

Which we can then reformat as we want and use in Lua if we want.

## `defmulti`
Function signature:

```
(defmulti name docstring? dispatch-fn attr-map?)
```

Create multifunction with
runtime dispatching based on results from `dispatch-fn`.  Returns an
empty table with `__call` metamethod, that calls `dispatch-fn` on its
arguments.  Amount of arguments passed, should be the same as accepted
by `dispatch-fn`.  Looks for multimethod based on result from
`dispatch-fn`.

By default, multifunction has no multimethods, see
[`multimethod`](#multimethod) on how to add one.

## `defonce`
Function signature:

```
(defonce attr-map? name expr)
```

Works the same as [`def`](#def), but ensures that later `defonce`
calls will not override existing bindings:

``` fennel
(defonce a 10)
(defonce a 20)
(print a) ;; => prints 10
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

(map (fn [[k v]] [(string.upper k) v]) {:a 1 :b 2 :c 3})
;; => {:A 1 :B 2 :C 3}
(map #(* $ $) [1 2 3 4])
;; [1 4 9 16]
```
See [`into`](#into) for more info on how conversion is done.

## `fn*`
Function signature:

```
(fn* name docstring? [arglist*] body* name docstring ([arglist*] body)*)
```

Create (anonymous) function of fixed arity.
Supports multiple arities by defining bodies as lists:

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
   (f (unpack xs))))
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
   (join (join s1 s2) (unpack strings)))) ;; call `join` resolves to ns.strings.join

(fn* ns.tables.join
  ([t1 t2]
   (let [res []]
     (each [_ v (ipairs t1)] (table.insert res v))
     (each [_ v (ipairs t2)] (table.insert res v))
     res))
  ([t1 t2 & tables]
   (join (join t1 t2) (unpack tables)))) ;; call to `join` resolves to ns.tables.join
```

Note that this creates a collision and local `join` overrides `join`
from `ns.strings`, so the latter must be fully qualified
`ns.strings.join` when called outside of the function:

``` fennel
(ns.strings.join "a" "b" "c")
;; => abc
(join ["a"] ["b"] ["c"] ["d" "e"])
;; => ["a" "b" "c" "d" "e"]
(join "a" "b" "c")
;; {}
```

## `if-let`
Function signature:

```
(if-let [binding test] then-branch else-branch)
```

If test is logical true,
evaluates `then-branch` with binding-form bound to the value of test,
if not, yields `else-branch`.

## `if-some`
Function signature:

```
(if-some [binding test] then-branch else-branch)
```

If test is non-`nil`, evaluates
`then-branch` with binding-form bound to the value of test, if not,
yields `else-branch`.

## `into`
Function signature:

```
(into to from)
```

Transform one table into another.  Mutates first table.

Transformation happens in runtime, but type deduction happens in
compile time if possible.  This means, that if literal values passed
to `into` this will have different effects for associative tables and
vectors:

``` fennel
(into [1 2 3] [4 5 6]) ;; => [1 2 3 4 5 6]
(into {:a 1 :c 2} {:a 0 :b 1}) ;; => {:a 0 :b 1 :c 2}
```

Conversion between different table types is also supported:

``` fennel
(into [] {:a 1 :b 2 :c 3}) ;; => [[:a 1] [:b 2] [:c 3]]
(into {} [[:a 1] [:b 2]]) ;; => {:a 1 :b 2}
```

Same rules apply to runtime detection of table type, except that this
will not work for empty tables:

``` fennel
(local empty-table {})
(into empty-table {:a 1 :b 2}) ;; => [[:a 1] [:b 2]]
``` fennel

If table is empty, `into` defaults to sequential table, because it
allows safe conversion from both sequential and associative tables.

Type for non empty tables hidden in variables can be deduced at
runtime, and this works as expected:

``` fennel
(local t1 [1 2 3])
(local t2 {:a 10 :c 3})
(into t1 {:a 1 :b 2}) ;; => [1 2 3 [:a 1] [:b 2]]
(into t2 {:a 1 :b 2}) ;; => {:a 1 :b 2 :c 3}
```

`cljlib.fnl` module provides two additional functions `vector` and
`hash-map`, that can create empty tables, which can be distinguished
at runtime:

``` fennel
(into (vector) {:a 1 :b 2}) ;; => [[:a 1] [:b 2]]
(into (hash-map) [[:a 1 :b 2]]) ;; => {:a 1 :b 2}
```

## `meta`
Function signature:

```
(meta value)
```

Get `value` metadata.  If value has no metadata, or metadata
feature is not enabled returns `nil`.

### Example

``` fennel
>> (meta (with-meta {} {:meta "data"}))
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

## `when-let`
Function signature:

```
(when-let [binding test] & body)
```

If test is logical true,
evaluates `body` in implicit `do`.

## `when-meta`
Function signature:

```
(when-meta [& body])
```

Wrapper that compiles away if metadata support was not enabled.  What
this effectively means, is that everything that is wrapped with this
macro will disappear from the resulting Lua code if metadata is not
enabled when compiling with `fennel --compile` without `--metadata`
switch.

## `when-some`
Function signature:

```
(when-some [binding test] & body)
```

If test is non-`nil`,
evaluates `body` in implicit `do`.

## `with-meta`
Function signature:

```
(with-meta value meta)
```

Attach metadata to a value.  When metadata feature is not enabled,
returns the value without additional metadata.

``` fennel
>> (local foo (with-meta (fn [...] (let [[x y z] [...]] (+ x y z)))
                         {:fnl/arglist ["x" "y" "z" "..."]
                          :fnl/docstring "sum first three values"}))
>> (doc foo)
(foo x y z ...)
  sum first three values
```


---

Copyright (C) 2020 Andrey Orst

License: [MIT](https://gitlab.com/andreyorst/fennel-cljlib/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc 0.0.4
     https://gitlab.com/andreyorst/fenneldoc -->
