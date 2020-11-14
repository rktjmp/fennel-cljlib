# Cljlib-macros.fnl
Macro module for Fennel Cljlib.

## Metadata macros
Metadata in Fennel is a pretty tough subject, as there's no such thing as metadata in Lua.
Therefore, the metadata usage in Fennel is more limited compared to Clojure.
This library provides some facilities for metadata management, which are experimental and should be used with care.

There are several important gotchas about using metadata.

First, note that this works only when used with Fennel, and only when `(require fennel)` works.
For compiled Lua library this feature is turned off.

Second, try to avoid using metadata with anything else than tables and functions.
When storing function or table as a key into metatable, its address is used, while when storing string of number, the value is used.
This, for example, may cause documentation collision, when you've set some variable holding a number value to have certain docstring, and later you've defined another variable with the same value, but different docstring.
While this isn't a major breakage, it may confuse if someone will explore your code in the REPL with `doc`.

Lastly, note that prior to Fennel 0.7.1 `import-macros` wasn't respecting `--metadata` switch.
So if you're using Fennel < 0.7.1 this stuff will only work if you use `require-macros` instead of `import-macros`.


### `when-meta`
This macros is a wrapper that compiles away if metadata support was not enabled.
What this effectively means, is that everything that is wrapped with this macro will disappear from the resulting Lua code if metadata is not enabled when compiling with `fennel --compile`.


### `with-meta`
Attach metadata to a value.

    >> (local foo (with-meta (fn [...] (let [[x y z] [...]] (+ x y z)))
                             {:fnl/arglist [:x :y :z :...]
                              :fnl/docstring "sum first three values"}))
    >> (doc foo)
    (foo x y z ...)
      sum first three values

When metadata feature is not enabled, returns the value without additional metadata.


### `meta`
Get metadata table from object:

    >> (meta (with-meta {} {:meta "data"}))
    {
        :meta "data"
    }


## `def` and `defonce`
`def` is wrappers around `local` which can declare variables inside namespace, and as local at the same time:

    >> (def ns {})
    >> (def a 10)
    >> a
    10
    >> (def ns.a 20)
    >> a
    20
    >> ns.a
    20

Both `ns.a` and `a` refer to the same value.

`defonce` ensures that the binding isn't overridden by another `defonce`:

    >> (defonce ns {})
    >> (defonce ns.a 42)
    >> (defonce ns 10)
    >> ns
    {:a 42}
    >> a
    42

Both `def` and `defonce` support literal metadata table as first argument, or a :dynamic keyword, that uses Fennel `var` instead of `local`:

    >> (def {:dynamic true} a 10)
    >> (set a 20)
    >> a
    20
    >> (defonce :dynamic b 40)
    >> (set b 42)
    >> b
    42

Documentation string can be attached to value via `:doc` keyword.
However it is not recommended to attach metadata to everything except tables and functions:

    ;; Bad, may overlap with existing documentation for 299792458, if any
    >> (def {:doc "The speed of light in m/s"} c 299792458)
    >> (doc c)
    c
      The speed of light in m/s

    ;; OK
    >> (def {:doc "default connection options"}
            defaults {:port 1234
                      :host localhost})


## `fn*`
Clojure's `fn` equivalent.
Returns a function of fixed amount of arguments by doing runtime dispatch based on argument count.
Capable of producing multi-arity functions:

    (fn* square "square number" [x] (^ x 2))

    (square 9) ;; => 81.0
    (square 1 2) ;; => error

    (fn* range
      "Returns increasing sequence  of numbers from `lower' to `upper'.
    If `lower' is not provided, sequence starts from zero.
    Accepts optional `step'"
      ([upper] (range 0 upper 1))
      ([lower upper] (range lower upper 1))
      ([lower upper step]
       (let [res []]
         (for [i lower (- upper step) step]
           (table.insert res i))
         res)))

    (range 10) ;; => [0 1 2 3 4 5 6 7 8 9]
    (range -10 0) ;; => [-10 -9 -8 -7 -6 -5 -4 -3 -2 -1]
    (range 0 1 0.2) ;; => [0.0 0.2 0.4 0.6 0.8]

Both variants support up to one arity with `& more`:

    (fn* vec [& xs] xs)

    (vec 1 2 3) ;; => [1 2 3]

    (fn* add
      "sum two or more values"
      ([] 0)
      ([a] a)
      ([a b] (+ a b))
      ([a b & more] (add (+ a b) (unpack more))))

    (add) ;; => 0
    (add 1) ;; => 1
    (add 1 2) ;; => 3
    (add 1 2 3 4) ;; => 10

One extra capability of `fn*` supports the same semantic as `def` regarding namespaces:

    (local ns {})

    (fn* ns.plus
      ([] 0)
      ([x] x)
      ([x y] (+ x y))
      ([x y & zs] (apply plus (+ x y) zs)))

    ns

Note, that `plus` is used without `ns` part, e.g. not `ns.plus`.
If we `require` this code from file in the REPL, we will see that our `ns` has single function `plus`:

    >> (local ns (require :module))
    >> ns
    {add #<function 0xbada55code>}

This is possible because `fn*` separates the namespace part from the function name, and creates a `local` variable with the same name as function, then defines the function within lexical scope of `do`, sets `namespace.foo` to it and returns the function object to the outer scope.

    (local plus
           (do (fn plus [...]
                 ;; plus body
                 )
               (set ns.plus plus)
               plus))

See `core.fnl` for more examples.


## `fn+`
Works similarly to Fennel's `fn`, by creating ordinary function without arity semantics, except does the namespace automation like `fn*`, and has the same order of arguments as the latter:

    (local ns {})

    ;; module & file-local functions
    (fn+ ns.double
      "double the number"
      [x]
      (* x 2))

    (fn+ ns.triple
      [x]
      (* x 3))

    ;; no namespace, file-local function
    (fn+ quadruple
      [x]
      (* x 4))

    ;; anonymous file-local function
    (fn+ [x] (* x 5))

    ns

See `core.fnl` for more examples.


## `if-let` and `when-let`
When test expression is not `nil` or `false`, evaluates the first body form with the `name` bound to the result of the expressions.

    (if-let [val (test)]
      (print val)
      :fail)

Expanded form:

    (let [tmp (test)]
      (if tmp
          (let [val tmp]
            (print val))
          :fail))

`when-let` is mostly the same, except doesn't have false branch and accepts any amount of forms:

    (when-let [val (test)]
      (print val)
      val)

Expanded form:

    (let [tmp (test)]
      (if tmp
          (let [val tmp]
            (print val)
            val)))


## `if-some` and `when-some`
Much like `if-let` and `when-let`, except tests expression for not being `nil`.

    (when-some [val (foo)]
      (print (.. "val is not nil: " val))
      val)


## `into`
Clojure's `into` function is implemented as macro, because Fennel has no runtime distinction between `[]` and `{}` tables, since Lua also doesn't feature this feature.
However we can do this at compile time.

    (into [1 2 3] [4 5 6]) ;; => [1 2 3 4 5 6]
    (into [] {:a 1 :b 2 :c 3 :d 4}) ;; => [["d" 4] ["a" 1] ["b" 2] ["c" 3]]
    (into {} [[:d 4] [:a 1] [:b 2] [:c 3]]) ;; => {:a 1 :b 2 :c 3 :d 4}
    (into {:a 0 :e 5} {:a 1 :b 2 :c 3 :d 4}) ;; => {:a 1 :b 2 :c 3 :d 4 :e 5}

Because the type check at compile time it will only respect the type when literal representation is used.
If a variable holding the table, its type is checked at runtime.
Empty tables default to sequential ones:

    (local a [])
    (into a {:a 1 :b 2}) ;; => [["b" 2] ["a" 1]]

    (local b {})
    (into b {:a 1 :b 2}) ;; => [["b" 2] ["a" 1]]

However, if target table is not empty, its type can be deduced:

    (local a {:c 3})
    (into a {:a 1 :b 2}) ;; => {:a 1 :b 2 :c 3}

    (local b [1])
    (into b {:a 1 :b 2}) ;; => [1 ["b" 2] ["a" 1]]

Note that when converting associative table into sequential table order is determined by the `pairs` function.
Also note that if variable stores the table has both integer key 1, and other associative keys, the type will be the same as of sequential table.


## `defmulti` and `defmethod`
A bit more simple implementations of Clojure's `defmulti` and `defmethod`.
`defmulti` macros returns an empty table with `__call` metamethod, that calls dispatching function on its arguments.
Methods are defined inside `multimethods` table, which is also stored in the metatable.

`defmethod` adds a new method to the metatable of given `multifn`.
It accepts the multi-fn table as its first argument, the dispatch value as second, and Fennel's arglist followed by the body:

    (defmulti fac (fn [x] x))

    (defmethod fac 0 [_] 1)
    (defmethod fac :default [x] (* x (fac (- x 1))))

    (fac 4) ;; => 24

`:default` is a special method which gets called when no other methods were found for given dispatch value.
