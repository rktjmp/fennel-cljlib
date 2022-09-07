# Macros (v1.0.0)
Macros for fennel-cljlib.

**Table of contents**

- [`cond`](#cond)
- [`def`](#def)
- [`defmethod`](#defmethod)
- [`defmulti`](#defmulti)
- [`defn`](#defn)
- [`defn-`](#defn-)
- [`fn*`](#fn)
- [`if-let`](#if-let)
- [`if-some`](#if-some)
- [`in-ns`](#in-ns)
- [`lazy-cat`](#lazy-cat)
- [`lazy-seq`](#lazy-seq)
- [`loop`](#loop)
- [`ns`](#ns)
- [`time`](#time)
- [`try`](#try)
- [`when-let`](#when-let)
- [`when-some`](#when-some)

## `cond`
Function signature:

```
(cond ...)
```

**Undocumented**

## `def`
Function signature:

```
(def ([name initializer]) ([meta name initializer]))
```

Name binding macro similar to `local` but acts in terms of current
namespace set with the `ns` macro, unless `:private` was passed before
the binding name.

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
(ns test)

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
(ns test)

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
(ns test)

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

## `defn`
Function signature:

```
(defn ([name doc-string? [params*] pre-post? body]) ([name doc-string? ([params*] pre-post? body) +]))
```

Same as (def name (fn* name docstring? [params*] pre-post? exprs*))
or (def name (fn* name docstring? ([params*] pre-post? exprs*)+)) with
any doc-string or attrs added to the function metadata.

## `defn-`
Function signature:

```
(defn- ([name doc-string? [params*] pre-post? body]) ([name doc-string? ([params*] pre-post? body) +]))
```

Same as (def :private name (fn* name docstring? [params*] pre-post?
exprs*)) or (def :private name (fn* name docstring? ([params*]
pre-post?  exprs*)+)) with any doc-string or attrs added to the
function metadata.

## `fn*`
Function signature:

```
(fn* ([name doc-string? [params*] pre-post? body]) ([name doc-string? ([params*] pre-post? body) +]))
```

Clojure-inspired `fn` macro for defining functions.
Supports multi-arity dispatching via the following syntax:

(fn* optional-name
  optional-docstring
  ([arity1] body1)
  ([other arity2] body2))

Accepts pre and post conditions in a form of a table after argument
list:

(fn* optional-name
  optional-docstring
  [arg1 arg2]
  {:pre  [(check1 arg1 arg2) (check2 arg1)]
   :post [(check1 $) ... (checkN $)]}
  body)

The same syntax applies to multi-arity version.

(pre and post checks are not yet implemented)

## `if-let`
Function signature:

```
(if-let [name test] if-branch else-branch ...)
```

**Undocumented**

## `if-some`
Function signature:

```
(if-some [name test] if-branch else-branch ...)
```

**Undocumented**

## `in-ns`
Function signature:

```
(in-ns name)
```

**Undocumented**

## `lazy-cat`
Function signature:

```
(lazy-cat ...)
```

**Undocumented**

## `lazy-seq`
Function signature:

```
(lazy-seq ...)
```

**Undocumented**

## `loop`
Function signature:

```
(loop binding-vec body*)
```

Recursive loop macro.

Similar to `let`, but binds a special `recur` call that will reassign
the values of the `binding-vec` and restart the loop `body*`.  Unlike
`let`, doesn't support multiple-value destructuring.

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

## `ns`
Function signature:

```
(ns name commentary requirements)
```

Namespace declaration macro.

## `time`
Function signature:

```
(time expr)
```

Measure expression execution time in ms.

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

## `when-let`
Function signature:

```
(when-let [name test] ...)
```

**Undocumented**

## `when-some`
Function signature:

```
(when-some [name test] ...)
```

**Undocumented**


---

Copyright (C) 2020-2021 Andrey Listopadov

License: [MIT](https://gitlab.com/andreyorst/fennel-cljlib/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc v0.1.9
     https://gitlab.com/andreyorst/fenneldoc -->
