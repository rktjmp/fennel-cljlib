(local fennel (require :fennel))


;;;;;;;;;; compile time check that `--metadata` feature was enabled ;;;;;;;;;;;;

(local meta-enabled (pcall _SCOPE.specials.doc
                           (list (sym :doc) (sym :doc))
                           _SCOPE _CHUNK))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn first [tbl]
  (. tbl 1))

(fn rest [tbl]
  [((or table.unpack _G.unpack) tbl 2)])

(fn string? [x]
  (= (type x) :string))

(fn multisym->sym [s]
  ;; Strip multisym part from symbol and return new symbol and
  ;; indication that sym was transformed.  Non-multisym symbols returned as
  ;; is.
  ;;
  ;; ``` fennel
  ;; (multisym->sym a.b)   ;; => (a true)
  ;; (multisym->sym a.b.c) ;; => (c true)
  ;; (multisym->sym a)     ;; => (a false)
  ;; ```
  (values (sym (string.match (tostring s) "[^.]+$"))
          (multi-sym? s)))

(fn contains? [tbl x]
  ;; Checks if `x` is stored in `tbl` in linear time.
  (var res false)
  (each [i v (ipairs tbl)]
    (if (= v x)
        (do (set res i)
            (lua :break))))
  res)

(fn check-two-binding-vec [bindings]
  ;; Test if `bindings` is a `sequence` that holds two forms, first of
  ;; which is a `sym`, `table` or `sequence`.
  (and (assert-compile (sequence? bindings)
                       "expected binding table" [])
       (assert-compile (= (length bindings) 2)
                       "expected exactly two forms in binding vector." bindings)
       (assert-compile (or (sym? (first bindings))
                           (sequence? (first bindings))
                           (table? (first bindings)))
                       "expected symbol, sequence or table as binding." bindings)))

(fn attach-meta [value meta]
  (each [k v (pairs meta)]
    (fennel.metadata:set value k v)))


;;;;;;;;;;;;;;;;;;;;;;;;;; Runtime function builers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This code should be shared with `cljlib.fnl` however it seems
;; impossible to actually do that right now, mainly because there's no
;; way of doing relative loading of macro modules.

(fn eq-fn []
  ;; Returns recursive equality function.
  ;;
  ;; This function is able to compare tables of any depth, even if one of
  ;; the tables uses tables as keys.
  `(fn eq# [left# right#]
     (if (and (= (type left#) :table) (= (type right#) :table))
         (let [oldmeta# (getmetatable right#)]
           ;; In case if we'll get something like
           ;; `(eq {[1 2 3] {:a [1 2 3]}} {[1 2 3] {:a [1 2 3]}})`
           ;; we have to do even deeper search
           (setmetatable right# {:__index (fn [tbl# key#]
                                        (var res# nil)
                                        (each [k# v# (pairs tbl#)]
                                          (when (eq# k# key#)
                                            (set res# v#)
                                            (lua :break)))
                                        res#)})
           (var [res# count-a# count-b#] [true 0 0])
           (each [k# v# (pairs left#)]
             (set res# (eq# v# (. right# k#)))
             (set count-a# (+ count-a# 1))
             (when (not res#) (lua :break)))
           (when res#
             (each [_# _# (pairs right#)]
               (set count-b# (+ count-b# 1)))
             (set res# (= count-a# count-b#)))
           (setmetatable right# oldmeta#)
           res#)
         (= left# right#))))

(fn seq-fn []
  ;; Returns function that transforms tables and strings into sequences.
  ;;
  ;; Sequential tables `[1 2 3 4]` are shallowly copied.
  ;;
  ;; Associative tables `{:a 1 :b 2}` are transformed into `[[:a 1] [:b 2]]`
  ;; with non deterministic order.
  ;;
  ;; Strings are transformed into a sequence of letters.
  `(fn [col#]
     (let [type# (type col#)
           res# (setmetatable {} {:cljlib/type :seq})
           insert# table.insert]
       (if (= type# :table)
           (do (var assoc?# false)
               (let [assoc-res# (setmetatable {} {:cljlib/type :seq})]
                 (each [k# v# (pairs col#)]
                   (if (and (not assoc?#)
                            (not (= (type k#) :number)))
                       (set assoc?# true))
                   (insert# res# v#)
                   (insert# assoc-res# [k# v#]))
                 (if assoc?# assoc-res# res#)))
           (= type# :string)
           (let [char# utf8.char]
             (each [_# b# (utf8.codes col#)]
               (insert# res# (char# b#)))
             res#)
           (= type# :nil) nil
           (error "expected table, string or nil" 2)))))

(fn table-type-fn []
  `(fn [tbl#]
     (let [t# (type tbl#)]
       (if (= t# :table)
           (let [meta# (getmetatable tbl#)
                 table-type# (and meta# (. meta# :cljlib/type))]
             (if table-type# table-type#
                 (let [(k# _#) (next tbl#)]
                   (if (and (= (type k#) :number) (= k# 1)) :seq
                       (= k# nil) :empty
                       :table))))
           (= t# :nil) :nil
           (= t# :string) :string
           :else))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Metadata ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn when-meta [...]
  "Wrapper that compiles away if metadata support was not enabled.  What
this effectively means, is that everything that is wrapped with this
macro will disappear from the resulting Lua code if metadata is not
enabled when compiling with `fennel --compile` without `--metadata`
switch."
  (when meta-enabled
    `(do ,...)))

(attach-meta when-meta {:fnl/arglist ["[& body]"]})

(fn meta [value]
  "Get `value` metadata.  If value has no metadata, or metadata
feature is not enabled returns `nil`.

# Example

``` fennel
>> (meta (with-meta {} {:meta \"data\"}))
;; => {:meta \"data\"}
```

# Note
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
`import-macros`."
  (when-meta
    `(let [(res# fennel#) (pcall require :fennel)]
       (if res# (. fennel#.metadata ,value)))))

(fn with-meta [value meta]
  "Attach metadata to a value.  When metadata feature is not enabled,
returns the value without additional metadata.

``` fennel
>> (local foo (with-meta (fn [...] (let [[x y z] [...]] (+ x y z)))
                         {:fnl/arglist [\"x\" \"y\" \"z\" \"...\"]
                          :fnl/docstring \"sum first three values\"}))
>> (doc foo)
(foo x y z ...)
  sum first three values
```"
  (if (not meta-enabled) value
      `(let [value# ,value
             (res# fennel#) (pcall require :fennel)]
         (if res#
             (each [k# v# (pairs ,meta)]
               (fennel#.metadata:set value# k# v#)))
         value#)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fn* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn gen-arglist-doc [args]
  ;; Construct vector of arguments represented as strings from AST.
  (if (list? (. args 1))
      (let [arglist []
            opener (if (> (length args) 1) "\n  (" "(")]
        (each [i v (ipairs args)]
          (let [arglist-doc (gen-arglist-doc v)]
            (when (next arglist-doc)
              (table.insert
               arglist
               (.. opener (table.concat arglist-doc " ") ")")))))
        arglist)

      (sequence? (. args 1))
      (let [arglist []
            args (. args 1)
            len (length args)]
        (each [i v (ipairs args)]
          (table.insert arglist
                        (match i
                          (1 ? (= len 1)) (.. "[" (tostring v) "]")
                          1   (.. "[" (tostring v))
                          len (.. (tostring v) "]")
                          _   (tostring v))))
        arglist)))

(fn multisym->sym [s]
  (if (multi-sym? s)
      (values (sym (string.gsub (tostring s) ".*[.]" "")) true)
      (values s false)))

(fn has-amp? [args]
  ;; Check if arglist has `&` and return its position of `false`.  Performs
  ;; additional checks for `&` and `...` usage in arglist.
  (var res false)
  (each [i s (ipairs args)]
    (if (= (tostring s) "&")
        (if res (assert-compile false "only one `&' can be specified in arglist." args)
            (set res i))
        (= (tostring s) "...")
        (assert-compile false "use of `...' in `fn*' is not permitted. Use `&' if you want a vararg." args)
        (and res (> i (+ res 1)))
        (assert-compile false "only one `more' argument can be supplied after `&' in arglist." args)))
  res)

(fn gen-arity [[args & body]]
  ;; Forms three values, representing data needed to create dispatcher:
  ;;
  ;; - the length of arglist;
  ;; - the body of the function we generate;
  ;; - position of `&` in the arglist if any.
  (assert-compile (sequence? args) "fn*: expected parameters table.

* Try adding function parameters as a list of identifiers in brackets." args)
  (values (length args)
          (list 'let [args ['...]] (list 'do ((or table.unpack _G.unpack) body)))
          (has-amp? args)))

(fn grows-by-one-or-equal? [tbl]
  ;; Checks if table consists of integers that grow by one or equal to
  ;; eachother when sorted.  Used for checking if we supplied all arities
  ;; for dispatching, and there's no need in the error handling.
  ;;
  ;; ``` fennel
  ;; (grows-by-one-or-equal? [1 3 2]) => true, because [1 2 3]
  ;; (grows-by-one-or-equal? [1 4 2]) => true, because 3 is missing
  ;; (grows-by-one-or-equal? [1 3 2 3]) => true, because equal values are allowed.
  ;; ```
  (let [t []]
    (each [_ v (ipairs tbl)] (table.insert t v))
    (table.sort t)
    (var prev nil)
    (each [_ cur (ipairs t)]
      (if prev
          (when (and (not= (+ prev 1) cur)
                     (not= prev cur))
            (lua "return false")))
      (set prev cur))
    prev))

(fn arity-dispatcher [len fixed body& name]
  ;; Forms an `if` expression with all fixed arities first, then `&` arity,
  ;; if present, and default error message as last arity.
  ;;
  ;; `len` is a symbol, that represents the length of the current argument
  ;; list, and is computed at runtime.
  ;;
  ;; `fixed` is a table of arities with fixed amount of arguments.  These
  ;; are put in this `if` as: `(= len fixed-len)`, where `fixed-len` is the
  ;; length of current arity arglist, computed with `gen-arity`.
  ;;
  ;; `body&` stores size of fixed part of arglist, that is, everything up
  ;; until `&`, and the body itself.  When `body&` provided, the `(>= len
  ;; more-len)` is added to the resulting `if` expression.
  ;;
  ;; Lastly the catchall branch is added to `if` expression, which ensures
  ;; that only valid amount of arguments were passed to function, which are
  ;; defined by previous branches.
  (let [bodies '(if)
        lengths []]
    (var max nil)
    (each [fixed-len body (pairs (doto fixed))]
      (when (or (not max) (> fixed-len max))
        (set max fixed-len))
      (table.insert lengths fixed-len)
      (table.insert bodies (list '= len fixed-len))
      (table.insert bodies body))
    (when body&
      (let [[more-len body arity] body&]
        (assert-compile (not (and max (<= more-len max))) "fn*: arity with `&' must have more arguments than maximum arity without `&'.

* Try adding more arguments before `&'" arity)
        (table.insert lengths (- more-len 1))
        (table.insert bodies (list '>= len (- more-len 1)))
        (table.insert bodies body)))
    (if (not (and (grows-by-one-or-equal? lengths)
                  (contains? lengths 0)
                  body&))
        (table.insert bodies (list 'error
                             (.. "wrong argument amount"
                                 (if name (.. " for "  name) "")) 2)))
    bodies))

(fn single-arity-body [args fname]
  ;; Produces arglist and body for single-arity function.
  ;; For more info check `gen-arity' documentation.
  (let [[args & body] args
        (arity body amp) (gen-arity [args ((or table.unpack _G.unpack) body)])]
    `(let [len# (select :# ...)]
       ,(arity-dispatcher
         'len#
         (if amp {} {arity body})
         (if amp [amp body])
         fname))))

(fn multi-arity-body [args fname]
  ;; Produces arglist and all body forms for multi-arity function.
  ;; For more info check `gen-arity' documentation.
  (let [bodies {}   ;; bodies of fixed arity
        bodies& []] ;; bodies where arglist contains `&'
    (each [_ arity (ipairs args)]
      (let [(n body amp) (gen-arity arity)]
        (if amp
            (table.insert bodies& [amp body arity])
            (tset bodies n body))))
    (assert-compile (<= (length bodies&) 1)
                    "fn* must have only one arity with `&':"
                    (. bodies& (length bodies&)))
    `(let [len# (select :# ...)]
       ,(arity-dispatcher
         'len#
         bodies
         (if (not= (next bodies&) nil)
             (. bodies& 1))
         fname))))

(fn fn* [name doc? ...]
  "Create (anonymous) function of fixed arity.
Supports multiple arities by defining bodies as lists:

# Examples
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
     \"raise `x` to power of 3\"
     [x]
     (^ x 3))

(fn* greet
     \"greet a `person`, optionally specifying default `greeting`.\"
     ([person] (print (.. \"Hello, \" person \"!\")))
     ([greeting person] (print (.. greeting \", \" person \"!\"))))
```

Argument lists follow the same destruction rules as per `let`.
Variadic arguments with `...` are not supported use `& rest` instead.
Note that only one arity with `&` is supported.

### Namespaces
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
(ns.strings.join \"a\" \"b\" \"c\")
;; => abc
(join [\"a\"] [\"b\"] [\"c\"] [\"d\" \"e\"])
;; => [\"a\" \"b\" \"c\" \"d\" \"e\"]
(join \"a\" \"b\" \"c\")
;; {}
```"
  (assert-compile (not (string? name)) "fn* expects symbol, vector, or list as first argument" name)
  (let [docstring (if (string? doc?) doc? nil)
        (name-wo-namespace namespaced?) (multisym->sym name)
        fname (if (sym? name-wo-namespace) (tostring name-wo-namespace))
        args (if (sym? name-wo-namespace)
                 (if (string? doc?) [...] [doc? ...])
                 [name-wo-namespace doc? ...])
        arglist-doc (gen-arglist-doc args)
        [x] args

        body (if (sequence? x) (single-arity-body args fname)
                 (list? x) (multi-arity-body args fname)
                 (assert-compile false "fn*: expected parameters table.

* Try adding function parameters as a list of identifiers in brackets." x))]
    (if (sym? name-wo-namespace)
        (if namespaced?
            `(local ,name-wo-namespace
                    (do
                      (fn ,name-wo-namespace [...] ,docstring ,body)
                      (set ,name ,name-wo-namespace)
                      ,(with-meta name-wo-namespace `{:fnl/arglist ,arglist-doc})))
            `(local ,name ,(with-meta `(fn ,name [...] ,docstring ,body) `{:fnl/arglist ,arglist-doc})))
        (with-meta `(fn [...] ,docstring ,body) `{:fnl/arglist ,arglist-doc}))))

(attach-meta fn* {:fnl/arglist ["name docstring? ([arglist*] body)*"]})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; let variants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fennel indeed has more advanced macro `match` which can be used in
;; place of any of the following macros, however it is sometimes more
;; convenient to convey intentions by explicitly saying `when-some`
;; implying that we're interested in non-nil value and only single branch
;; of execution.  The `match` macro on the other hand does not convey
;; such intention

(fn if-let [...]
  (let [[bindings then else] (match (select :# ...)
                               2 [...]
                               3 [...]
                               _ (error "wrong argument amount for if-some" 2))]
    (check-two-binding-vec bindings)
    (let [[form test] bindings]
      `(let [tmp# ,test]
         (if tmp#
             (let [,form tmp#]
               ,then)
             ,else)))))

(attach-meta if-let {:fnl/arglist ["[binding test]" "then-branch" "else-branch"]
                     :fnl/docstring "If test is logical true,
evaluates `then-branch` with binding-form bound to the value of test,
if not, yields `else-branch`."})


(fn when-let [...]
  (let [[bindings & body] (if (> (select :# ...) 0) [...]
                              (error "wrong argument amount for when-let" 2))]
    (check-two-binding-vec bindings)
    (let [[form test] bindings]
      `(let [tmp# ,test]
         (if tmp#
             (let [,form tmp#]
               ,((or table.unpack _G.unpack) body)))))))

(attach-meta when-let {:fnl/arglist ["[binding test]" "& body"]
                       :fnl/docstring "If test is logical true,
evaluates `body` in implicit `do`."})


(fn if-some [...]
  (let [[bindings then else] (match (select :# ...)
                               2 [...]
                               3 [...]
                               _ (error "wrong argument amount for if-some" 2))]
    (check-two-binding-vec bindings)
    (let [[form test] bindings]
      `(let [tmp# ,test]
         (if (= tmp# nil)
             ,else
             (let [,form tmp#]
               ,then))))))

(attach-meta if-some {:fnl/arglist ["[binding test]" "then-branch" "else-branch"]
                      :fnl/docstring "If test is non-`nil`, evaluates
`then-branch` with binding-form bound to the value of test, if not,
yields `else-branch`."})


(fn when-some [...]
  (let [[bindings & body] (if (> (select :# ...) 0) [...]
                              (error "wrong argument amount for when-some" 2))]
    (check-two-binding-vec bindings)
    (let [[form test] bindings]
      `(let [tmp# ,test]
         (if (= tmp# nil)
             nil
             (let [,form tmp#]
               ,((or table.unpack _G.unpack) body)))))))

(attach-meta when-some {:fnl/arglist ["[binding test]" "& body"]
                        :fnl/docstring "If test is non-`nil`,
evaluates `body` in implicit `do`."})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; into ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn table-type [tbl]
  (if (sequence? tbl) :seq
      (table? tbl) :table
      :else))

(fn into [to from]
  "Transform one table into another.  Mutates first table.

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
```"
  (assert-compile (and to from) "into: expected two arguments")
  (let [to-type (table-type to)
        from-type (table-type from)]
    (if (and (= to-type :seq) (= from-type :seq))
        `(let [to# (or ,to [])
               insert# table.insert]
           (each [_# v# (ipairs (or ,from []))]
             (insert# to# v#))
           (setmetatable to# {:cljlib/type :seq}))
        (= to-type :seq)
        `(let [to# (or ,to [])
               seq# ,(seq-fn)
               insert# table.insert]
           (each [_# v# (ipairs (seq# (or ,from [])))]
             (insert# to# v#))
           (setmetatable to# {:cljlib/type :seq}))
        (and (= to-type :table) (= from-type :seq))
        `(let [to# (or ,to [])]
           (each [_# [k# v#] (ipairs (or ,from []))]
             (tset to# k# v#))
           (setmetatable to# {:cljlib/type :table}))
        (and (= to-type :table) (= from-type :table))
        `(let [to# (or ,to [])
               from# (or ,from [])]
           (each [k# v# (pairs from#)]
             (tset to# k# v#))
           (setmetatable to# {:cljlib/type :table}))
        (= to-type :table)
        `(let [to# (or ,to [])
               from# (or ,from [])]
           (match (,(table-type-fn) from#)
             :seq (each [_# [k# v#] (ipairs from#)]
                    (tset to# k# v#))
             :table (each [k# v# (pairs from#)]
                      (tset to# k# v#))
             :else (error "expected table as second argument" 2))
           (setmetatable to# {:cljlib/type :table}))
        ;; runtime branch
        `(let [to# ,to
               from# ,from
               insert# table.insert
               table-type# ,(table-type-fn)
               seq# ,(seq-fn)
               to-type# (table-type# to#)
               to# (or to# []) ;; secure nil
               res# (match to-type#
                      ;; Sequence or empty table
                      (seq1# ? (or (= seq1# :seq) (= seq1# :empty)))
                      (do (each [_# v# (ipairs (seq# (or from# [])))]
                            (insert# to# v#))
                          to#)
                      ;; associative table
                      :table (match (table-type# from#)
                               (seq2# ? (or (= seq2# :seq) (= seq2# :string)))
                               (do (each [_# [k# v#] (ipairs (or from# []))]
                                     (tset to# k# v#))
                                   to#)
                               :table (do (each [k# v# (pairs (or from# []))]
                                            (tset to# k# v#))
                                          to#)
                               :empty to#
                               :else (error "expected table as second argument" 2))
                      ;; set both ordered set and hash set
                      (Set# ? (or (= Set# :cljlib/ordered-set) (= Set# :cljlib/hash-set)))
                      (do (each [_# v# (ipairs (seq# (or from# [])))]
                            (tset to# v# v#))
                          to#)
                      ;; sometimes it is handy to pass nil too
                      :nil (match (table-type# from#)
                             :nil nil
                             :empty to#
                             :seq (do (each [k# v# (pairs (or from# []))]
                                        (tset to# k# v#))
                                      to#)
                             :table (do (each [k# v# (pairs (or from# []))]
                                          (tset to# k# v#))
                                        to#)
                             :else (error "expected table as second argument" 2))
                      :else (error "expected table as first argument" 2))]
           (if res#
               (let [m# (or (getmetatable res#) {})]
                 (set m#.cljlib/type (match to-type#
                                       :seq :seq
                                       :empty :seq
                                       :table :table
                                       t# t#))
                 (setmetatable res# m#)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; empty ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn empty [x]
  "Return empty table of the same kind as input table `x`, with
additional metadata indicating its type.

# Example
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
See [`into`](#into) for more info on how conversion is done."
  (match (table-type x)
    :seq `(setmetatable {} {:cljlib/type :seq})
    :table `(setmetatable {} {:cljlib/type :table})
    _ `(let [x# ,x]
         (match (,(table-type-fn) x#)
           :cljlib/ordered-set (: x# :cljlib/empty)
           :cljlib/hash-set (: x# :cljlib/empty)
           t# (setmetatable {} {:cljlib/type t#})))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multimethods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn seq->table [seq]
  (let [tbl {}]
    (for [i 1 (length seq) 2]
      (tset tbl (. seq i) (. seq (+ i 1))))
    tbl))

(fn defmulti [...]
  (let [[name & options] (if (> (select :# ...) 0) [...]
                          (error "wrong argument amount for defmulti"))
        docstring (if (string? (first options)) (first options))
        options (if docstring (rest options) options)
        dispatch-fn (first options)
        options (rest options)]
    (assert (= (% (length options) 2) 0) "wrong argument amount for defmulti")
    (let [options (seq->table options)]
      (if (in-scope? name)
          `nil
          `(local ,name
                  (setmetatable
                   ,(with-meta {} {:fnl/docstring docstring})
                   {:__index
                    (fn [tbl# key#]
                      (let [eq# ,(eq-fn)]
                        (var res# nil)
                        (each [k# v# (pairs tbl#)]
                          (when (eq# k# key#)
                            (set res# v#)
                            (lua :break)))
                        res#))
                    :__call
                    (fn [t# ...]
                      ,docstring
                      (let [dispatch-value# (,dispatch-fn ...)
                            (res# view#) (pcall require :fennelview)
                            tostr# (if res# view# tostring)]
                        ((or (. t# dispatch-value#)
                             (. t# (or (. ,options :default) :default))
                             (error (.. "No method in multimethod '"
                                        ,(tostring name)
                                        "' for dispatch value: "
                                        (tostr# dispatch-value#))
                                    2)) ...)))
                    :__name (.. "multifn " ,(tostring name))
                    :__fennelview tostring
                    :cljlib/type :multifn}))))))

(attach-meta defmulti {:fnl/arglist [:name :docstring? :dispatch-fn :attr-map?]
                       :fnl/docstring "Create multifunction with
runtime dispatching based on results from `dispatch-fn`.  Returns an
empty table with `__call` metamethod, that calls `dispatch-fn` on its
arguments.  Amount of arguments passed, should be the same as accepted
by `dispatch-fn`.  Looks for multimethod based on result from
`dispatch-fn`.

By default, multifunction has no multimethods, see
[`multimethod`](#multimethod) on how to add one."})


(fn defmethod [multifn dispatch-val ...]
  (when (= (select :# ...) 0) (error "wrong argument amount for defmethod"))
  `(doto ,multifn (tset ,dispatch-val (do (fn* f# ,...) f#))))

(attach-meta defmethod {:fnl/arglist [:multifn :dispatch-val :fnspec]
                        :fnl/docstring "Attach new method to multi-function dispatch value. accepts the `multi-fn`
as its first argument, the dispatch value as second, and function tail
starting from argument list, followed by function body as in
[`fn*`](#fn).

# Examples
Here are some examples how multimethods can be used.

## Factorial example
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

## Multi-arity dispatching
Multi-arity function tails are also supported:

``` fennel
(defmulti foo (fn* ([x] [x]) ([x y] [x y])))

(defmethod foo [10] [_] (print \"I've knew I'll get 10\"))
(defmethod foo [10 20] [_ _] (print \"I've knew I'll get both 10 and 20\"))
(defmethod foo :default ([x] (print (.. \"Umm, got\" x)))
                        ([x y] (print (.. \"Umm, got both \" x \" and \" y))))
```

Calling `(foo 10)` will print `\"I've knew I'll get 10\"`, and calling
`(foo 10 20)` will print `\"I've knew I'll get both 10 and 20\"`.
However, calling `foo` with any other numbers will default either to
`\"Umm, got x\"` message, when called with single value, and `\"Umm, got
both x and y\"` when calling with two values.

## Dispatching on object's type
We can dispatch based on types the same way we dispatch on values.
For example, here's a naive conversion from Fennel's notation for
tables to Lua's one:

``` fennel
(defmulti to-lua-str (fn [x] (type x)))

(defmethod to-lua-str :number [x] (tostring x))
(defmethod to-lua-str :table [x] (let [res []]
                                   (each [k v (pairs x)]
                                     (table.insert res (.. \"[\" (to-lua-str k) \"] = \" (to-lua-str v))))
                                   (.. \"{\" (table.concat res \", \") \"}\")))
(defmethod to-lua-str :string [x] (.. \"\\\"\" x \"\\\"\"))
(defmethod to-lua-str :default [x] (tostring x))
```

And if we call it on some table, we'll get a valid Lua table:

``` fennel
(print (to-lua-str {:a {:b 10}}))
;; prints {[\"a\"] = {[\"b\"] = 10}}

(print (to-lua-str [:a :b :c [:d {:e :f}]]))
;; prints {[1] = \"a\", [2] = \"b\", [3] = \"c\", [4] = {[1] = \"d\", [2] = {[\"e\"] = \"f\"}}}
```

Which we can then reformat as we want and use in Lua if we want."})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; def and defonce ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn def [...]
  (let [[attr-map name expr] (match (select :# ...)
                               2 [{} ...]
                               3 [...]
                               _ (error "wrong argument amount for def" 2))
        attr-map (if (table? attr-map) attr-map
                     (string? attr-map) {attr-map true}
                     (error "def: expected keyword or literal table as first argument" 2))
        (s multi) (multisym->sym name)
        docstring (or (. attr-map :doc)
                      (. attr-map :fnl/docstring))
        f (if (. attr-map :mutable) 'var 'local)]
    (if multi
        `(,f ,s (do (,f ,s ,expr)
                    (set ,name ,s)
                    ,(with-meta s {:fnl/docstring docstring})))
        `(,f ,name ,(with-meta expr {:fnl/docstring docstring})))))

(attach-meta def {:fnl/arglist [:attr-map? :name :expr]
                  :fnl/docstring "Wrapper around `local` which can
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
(def {:doc \"speed of light in m/s\"} c 299792458)
(set c 0) ;; => error, can't mutate `c`

(def :mutable address \"Lua St.\") ;; same as (def {:mutable true} address \"Lua St.\")
(set address \"Lisp St.\") ;; can mutate `address`
```

However, attaching documentation metadata to anything other than
tables and functions considered bad practice, due to how Lua
works. More info can be found in [`with-meta`](#with-meta)
description."})

(fn defonce [...]
  (let [[attr-map name expr] (match (select :# ...)
                               2 [{} ...]
                               3 [...]
                               _ (error "wrong argument amount for def" 2))]
    (if (in-scope? name)
        nil
        (def attr-map name expr))))

(attach-meta defonce {:fnl/arglist [:attr-map? :name :expr]
                      :fnl/docstring "Works the same as [`def`](#def), but ensures that later `defonce`
calls will not override existing bindings:

``` fennel
(defonce a 10)
(defonce a 20)
(print a) ;; => prints 10
```"})


{: fn*
 : if-let
 : when-let
 : if-some
 : when-some
 : empty
 : into
 : when-meta
 : with-meta
 : meta
 : defmulti
 : defmethod
 : def
 : defonce
 :_VERSION #"0.3.0"
 :_LICENSE #"[MIT](https://gitlab.com/andreyorst/fennel-cljlib/-/raw/master/LICENSE)"
 :_COPYRIGHT #"Copyright (C) 2020 Andrey Orst"
 :_DOC_ORDER #[:fn*
               :def :defonce :defmulti :defmethod
               :into :empty
               :when-meta :with-meta :meta
               :if-let :when-let :if-some :when-some]
 :_DESCRIPTION #"Macros for Cljlib that implement various facilities from Clojure."}

;; LocalWords:  arglist fn runtime arities arity multi destructuring
;; LocalWords:  docstring Variadic LocalWords multisym sym tbl eq Lua
;; LocalWords:  defonce metadata metatable fac defmulti Umm defmethod
;; LocalWords:  multimethods multimethod multifn REPL fnl AST Lua's
;; LocalWords:  lua tostring str concat namespace ns Cljlib Clojure
