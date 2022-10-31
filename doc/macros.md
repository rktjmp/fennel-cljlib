# Macros (v1.1.1)
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

Takes a set of test expression pairs. It evaluates each test one at a
time.  If a test returns logical true, `cond` evaluates and returns
the value of the corresponding expression and doesn't evaluate any of
the other tests or exprs. `(cond)` returns nil.

## `def`
Function signature:

```
(def ([name initializer]) ([meta name initializer]))
```

Name binding macro similar to `local` but acts in terms of current
namespace set with the `ns` macro, unless `:private` was passed before
the binding name. Accepts the `name` to be bound and the `initializer`
expression. `meta` can be either an associative table where keys are
strings, or a string representing a key from the table. If a sole
string is given, it's value is set to `true` in the meta table.

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
any doc-string or attrs added to the function metadata.  Accepts
`name` wich will be used to refer to a function in the current
namespace, and optional `doc-string?`, a vector of function's
`params*`, `pre-post?` conditions, and the `body` of the function.
The body is wrapped in an implicit do.  See `fn*` for more info.

## `defn-`
Function signature:

```
(defn- ([name doc-string? [params*] pre-post? body]) ([name doc-string? ([params*] pre-post? body) +]))
```

Same as (def :private name (fn* name docstring? [params*] pre-post?
exprs*)) or (def :private name (fn* name docstring? ([params*]
pre-post?  exprs*)+)) with any doc-string or attrs added to the
function metadata. Accepts `name` wich will be used to refer to a
function, and optional `doc-string?`, a vector of function's `params*`,
`pre-post?` conditions, and the `body` of the function.  The body is
wrapped in an implicit do. See `fn*` for more info.

## `fn*`
Function signature:

```
(fn* ([name doc-string? [params*] pre-post? body]) ([name doc-string? ([params*] pre-post? body) +]))
```

Clojure-inspired `fn` macro for defining functions.
Accepts an optional `name` and `docstring?`, followed by the binding
list containing function's `params*`. The `body` is wrapped in an
implicit `do`.  The `doc-string?` argument specifies an optional
documentation for the function.  Supports multi-arity dispatching via
the following syntax:

(fn* optional-name
  optional-docstring
  ([arity1] body1)
  ([other arity2] body2))

Accepts `pre-post?` conditions in a form of a table after argument
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
(if-let [name test] if-branch else-branch)
```

When `test` is logical `true`, evaluates the `if-branch` with `name`
bound to the value of `test`. Otherwise evaluates the `else-branch`

## `if-some`
Function signature:

```
(if-some [name test] if-branch else-branch)
```

When `test` is not `nil`, evaluates the `if-branch` with `name`
bound to the value of `test`. Otherwise evaluates the `else-branch`

## `in-ns`
Function signature:

```
(in-ns name)
```

Sets the compile time variable `current-ns` to the given `name`.
Affects such macros as `def`, `defn`, which will bind names to the
specified namespace.

### Examples

```fennel
(ns a)
(defn f [] "f from a")
(ns b)
(defn f [] "f from b")
(in-ns a)
(defn g [] "g from a")
(in-ns b)
(defn g [] "g from b")

(assert-eq (a.f) "f from a")
(assert-eq (b.f) "f from b")
(assert-eq (a.g) "g from a")
(assert-eq (b.g) "g from b")
```

## `lazy-cat`
Function signature:

```
(lazy-cat & colls)
```

Expands to code which yields a lazy sequence of the concatenation of
`colls` - expressions returning collections.  Each expression is not
evaluated until it is needed.

## `lazy-seq`
Function signature:

```
(lazy-seq & body)
```

Takes a `body` of expressions that returns an sequence, table or nil,
and yields a lazy sequence that will invoke the body only the first
time `seq` is called, and will cache the result and return it on all
subsequent `seq` calls. See also - `realized?`

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
Accepts the `name` of the generated namespace, and creates a local
variable with this name holding a table. Optionally accepts
`commentary` describing what namespace is about and a `requirements`
spec, specifying what libraries should be required.

The `requirements` spec is a list that consists of vectors, specifying
library name and a possible alias or a vector of names to refer to
without a prefix:

```
(ns some-namespace
  "Description of the some-namespace."
  (:require [some.lib]
            [some.other.lib :as lib2]
            [another.lib :refer [foo bar baz]]))

(defn inc [x] (+ x 1))
```

Which is equivalent to:

```
(local some-namespace {})
(local lib (require :some.lib))
(local lib2 (require :some.other.lib))
(local {:bar bar :baz baz :foo foo} (require :another.lib))
(comment "Description of the some-namespace.")
```

Note that when no `:as` alias is given, the library will be named
after the innermost part of the require path, i.e. `some.lib` is
transformed to `lib`.

## `time`
Function signature:

```
(time expr)
```

Measure the CPU time spent executing `expr`.

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
(when-let [name test] & body)
```

When `test` is logical `true`, evaluates the `body` with `name` bound
to the value of `test`.

## `when-some`
Function signature:

```
(when-some [name test] & body)
```

When `test` is not `nil`, evaluates the `body` with `name` bound to
the value of `test`.


---

Copyright (C) 2020-2021 Andrey Listopadov

License: [MIT](https://gitlab.com/andreyorst/fennel-cljlib/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc v0.1.9
     https://gitlab.com/andreyorst/fenneldoc -->
