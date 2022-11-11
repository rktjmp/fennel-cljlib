(comment
 "MIT License

Copyright (c) 2022 Andrey Listopadov

Permission is hereby granted‚ free of charge‚ to any person obtaining a copy
of this software and associated documentation files (the “Software”)‚ to deal
in the Software without restriction‚ including without limitation the rights
to use‚ copy‚ modify‚ merge‚ publish‚ distribute‚ sublicense‚ and/or sell
copies of the Software‚ and to permit persons to whom the Software is
furnished to do so‚ subject to the following conditions：

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”‚ WITHOUT WARRANTY OF ANY KIND‚ EXPRESS OR
IMPLIED‚ INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY‚
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM‚ DAMAGES OR OTHER
LIABILITY‚ WHETHER IN AN ACTION OF CONTRACT‚ TORT OR OTHERWISE‚ ARISING FROM‚
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.")

(local core
  (if (and ... (string.match ... "init%-macros$"))
      (string.gsub ... "init%-macros$" "init")
      (or ... :init)))

(fn string? [x]
  (= :string (type x)))

(fn has? [tbl sym]
  ;; searches for the given symbol in a table.
  (var has false)
  (each [_ elt (ipairs tbl) :until has]
    (set has (= sym elt)))
  has)

;;; ns

(local cljlib-namespaces
  {}
  ;; A map of files and their respective namespaces.  Each entry is a
  ;; filename followed by a table with two keys: `:current` and
  ;; `:known`.  The second one holds all namespaces that were defined
  ;; for the file via the `ns` macro, and thus are available to switch
  ;; with the `in-ns` macro. The `:current` key represents currently
  ;; active namespace that is used for binding via the `def` macro and
  ;; its derivatives.
  )

(fn current-file [ast]
  (. (ast-source ast) :filename))

(fn create-ns [name]
  (let [file (current-file name)]
    (when (not (. cljlib-namespaces file))
      (tset cljlib-namespaces file {:known {}}))
    (tset cljlib-namespaces file :current name)
    (tset cljlib-namespaces file :known (tostring name) true))
  `(setmetatable
    {}
    {:__name "namespace"
     :__fennelview #(do ,(: "#<namespace: %s>" :format (tostring name)))}))

(fn known-ns? [name]
  (let [file (current-file name)]
    (?. cljlib-namespaces file :known (tostring name))))

(fn current-ns [ast]
  (?. cljlib-namespaces (current-file ast) :current))

(fn in-ns [name]
  "Sets the compile-time variable `cljlib-namespaces` to the given `name`.
Affects such macros as `def`, `defn`, which will bind names to the
specified namespace.

# Examples
Creating several namespaces in the file, and defining functions in each:

``` fennel
(ns a)
(defn f [] \"f from a\")
(ns b)
(defn f [] \"f from b\")
(in-ns a)
(defn g [] \"g from a\")
(in-ns b)
(defn g [] \"g from b\")

(assert-eq (a.f) \"f from a\")
(assert-eq (b.f) \"f from b\")
(assert-eq (a.g) \"g from a\")
(assert-eq (b.g) \"g from b\")
```

Note, switching namespaces in the REPL doesn't affect non-namespaced
local bindings.  In other words, when defining a local with `def`, a
bot a local binding and a namespaced binding are created, and
switching current namespace won't change the local binding:

``` fennel :skip-test
>> (ns foo)
nil
>> (def x 42)
nil
>> (ns bar)
nil
>> (def x 1337)
nil
>> (in-ns foo)
#<namespace: foo>
>> x ; user might have expected to see 42 here
1337
>> foo.x
42
>> bar.x
1337
```

Sadly, Fennel itself has no support for namespace switching in REPL,
so this feature can be only partially emulated by the cljlib library.
"
  (assert-compile (known-ns? name)
                  (: "no such namespace: %s" :format (tostring name))
                  name)
  (tset cljlib-namespaces (current-file name) :current name)
  name)

(fn ns [name commentary requirements]
  "Namespace declaration macro.
Accepts the `name` of the generated namespace, and creates a local
variable with this name holding a table. Optionally accepts
`commentary` describing what namespace is about and a `requirements`
spec, specifying what libraries should be required.

The `requirements` spec is a list that consists of vectors, specifying
library name and a possible alias or a vector of names to refer to
without a prefix:

``` fennel :skip-test
(ns some-namespace
  \"Description of the some-namespace.\"
  (:require [some.lib]
            [some.other.lib :as lib2]
            [another.lib :refer [foo bar baz]]))

(defn inc [x] (+ x 1))
```

Which is equivalent to:

``` fennel :skip-test
(local some-namespace {})
(local lib (require :some.lib))
(local lib2 (require :some.other.lib))
(local {:bar bar :baz baz :foo foo} (require :another.lib))
(comment \"Description of the some-namespace.\")
```

Note that when no `:as` alias is given, the library will be named
after the innermost part of the require path, i.e. `some.lib` is
transformed to `lib`.

See `in-ns` on how to switch namespaces."
  (let [bind-table [name]
        require-table [(create-ns name)]
        requirements (if (string? commentary)
                         requirements
                         commentary)]
    (match requirements
      [:require & requires]
      (each [_ spec (ipairs requires)]
        (match spec
          (where (or [module :as alias :refer names]
                     [module :refer names :as alias]))
          (do (table.insert bind-table (collect [_ name (ipairs names) :into {'&as alias}]
                                         (values (tostring name) name)))
              (table.insert require-table `(require ,(tostring module))))
          [module :as alias]
          (do (table.insert bind-table alias)
              (table.insert require-table `(require ,(tostring module))))
          [module :refer names]
          (do (table.insert bind-table (collect [_ name (ipairs names)]
                                         (values (tostring name) name)))
              (table.insert require-table `(require ,(tostring module))))
          [module]
          (do (->> (string.gsub (tostring module) ".+%.(.-)$" "%1")
                   (pick-values 1)
                   sym
                   (table.insert bind-table))
              (table.insert require-table `(require ,(tostring module))))
          _ (assert-compile false "wrong require syntax" name)))
      nil nil
      _ (assert-compile false "wrong require syntax" name))
    (if (string? commentary)
        `(local ,bind-table
           (values ,require-table (comment ,commentary)))
        `(local ,bind-table ,require-table))))

;;; def

(fn def [...]
  {:fnl/docstring "Name binding macro similar to `local` but acts in terms of current
namespace set with the `ns` macro, unless `:private` was passed before
the binding name. Accepts the `name` to be bound and the `initializer`
expression. `meta` can be either an associative table where keys are
strings, or a string representing a key from the table. If a sole
string is given, its value is set to `true` in the meta table."
   :fnl/arglist [([name initializer]) ([meta name initializer])]}
  (match [...]
    (where (or [:private name val]
               [{:private true} name val]))
    `(local ,name ,val)
    [name val]
    (let [namespace (current-ns name)]
      (if (in-scope? namespace)
          `(local ,name
             (let [v# ,val]
               (tset ,namespace ,(tostring name) v#)
               v#))
          `(local ,name ,val)))))

;;; defn

(local errors
  {:vararg "... is't allowed in the arglist, use & destructuring"
   :same-arity "Can't have 2 overloads with same arity"
   :arity-order "Overloads must be sorted by arities"
   :amp-arity "Variadic overload must be the last overload"
   :extra-rest-args "Only one argument allowed after &"
   :wrong-arg-amount "Wrong number of args (%s) passed to %s"
   :extra-amp "Can't have more than 1 variadic overload"})

(fn first [[x]] x)
(fn rest [[_ & xs]] xs)
(fn vfirst [x] x)
(fn vrest [_ ...] ...)

(fn length* [arglist]
  ;; Gets "length" of variadic arglist, stopping at first & plus 1 arg.
  ;; Additionally checks whether there are more than one arg after &.
  (var (l amp? n) (values 0 false nil))
  (each [i arg (ipairs arglist) :until amp?]
    (if (= arg '&)
        (set (amp? n) (values true i))
        (set l (+ l 1))))
  (when n
    (assert-compile (= (length arglist) (+ n 1))
                    errors.extra-rest-args
                    (. arglist (length arglist))))
  (if amp? (+ l 1) l))

(fn check-arglists [arglists]
  ;; performs a check that arglists are ordered correctly, and that
  ;; only one of multiarity arglists has the & symbol, additionally
  ;; checking for a presence of the multiple-values symbol.
  (var (size amp?) (values -1 false))
  (each [_ [arglist] (ipairs arglists)]
    (assert-compile (not (has? arglist '...)) errors.vararg arglist)
    (let [len (length* arglist)]
      (assert-compile (not= size len) errors.same-arity arglist)
      (assert-compile (< size len) errors.arity-order arglist)
      (assert-compile (not amp?) (if (has? arglist '&)
                                     errors.extra-amp
                                     errors.amp-arity) arglist)
      (set size len)
      (set amp? (has? arglist '&)))))

(fn with-immutable-rest [arglist body]
  `(let [core# (require ,core)
         ,arglist (core#.list ...)]
     ,(unpack body)))

(fn add-missing-arities! [arglists name]
  "Adds missing arity overloads for given `arglists`.
For example, given the [[[a] body] [[a b c] body]], will generate
[[[] error]
 [[a] body]
 [[arg_1_ arg_2_] error]
 [[a b c] body]]

Because inital arglist didn't specify arities of 0 and 2."
  (for [i (- (length* arglists) 1) 1 -1]
    (let [current-args (first (. arglists i))
          current-len (length* current-args)
          next-args (first (. arglists (+ i 1)))
          next-len (length* next-args)
          next-len (if (has? next-args '&) (- next-len 1) next-len)]
      (when (not= (+ current-len 1) next-len)
        (for [len (- next-len 1) (+ current-len 1) -1]
          (table.insert arglists (+ i 1) [(fcollect [i 1 len :into {:fake true}] (gensym :arg))
                                          `(error (: ,errors.wrong-arg-amount :format ,len ,(tostring name)))])))))
  (while (not= 0 (length* (first (first arglists))))
    (let [len (- (length* (first (first arglists))) 1)]
      (table.insert arglists 1 [(fcollect [i 1 len :into {:fake true}] (gensym :arg))
                                `(error (: ,errors.wrong-arg-amount :format ,len ,(tostring name)))]))))

;; TODO: implement pre-post conditions
(fn gen-match-fn [name doc arglists]
  ;; automated multi-arity dispatch generator
  (check-arglists arglists)
  (add-missing-arities! arglists name)
  (let [match-body `(match (select :# ...))]
    (var variadic? false)
    (each [_ [arglist & body] (ipairs arglists)]
      (table.insert match-body (if (has? arglist '&)
                                   (do (set variadic? true) (sym :_))
                                   (length arglist)))
      (table.insert match-body (if variadic?
                                   (with-immutable-rest arglist body)
                                   (if (and (> (length arglist) 0) (not arglist.fake))
                                       `(let [(,(unpack arglist)) (values ...)]
                                          ,(if (> (length body) 0)
                                               (unpack body)
                                               'nil))
                                       `(do ,(unpack body))))))
    (when (not variadic?)
      (table.insert match-body (sym :_))
      (table.insert match-body
                    `(error (: ,errors.wrong-arg-amount :format ,(sym :_) ,(tostring name)))))
    `(fn ,name [...]
       {:fnl/docstring ,doc
        :fnl/arglist ,(icollect [_ [arglist] (ipairs arglists)]
                        (when (not arglist.fake)
                          (list (sequence (unpack arglist)))))}
       ,match-body)))

;; TODO: implement pre-post conditions
(fn gen-fn [name doc arglist _pre-post body]
  (check-arglists [[arglist]])
  `(fn ,name [...]
     {:fnl/docstring ,doc
      :fnl/arglist ,(sequence arglist)}
     ,(if (has? arglist '&)
          (with-immutable-rest arglist [body])
          `(let ,(if (> (length arglist) 0)
                     `[(,(unpack arglist)) (values ...)]
                     `[])
             (let [cnt# (select "#" ...)]
               (when (not= ,(length arglist) cnt#)
                 (error (: ,errors.wrong-arg-amount :format cnt# ,(tostring name)))))
             ,body))))

(fn fn* [...]
  {:fnl/docstring
   "Clojure-inspired `fn' macro for defining functions.
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

(pre- and post-checks are not yet implemented)"
   :fnl/arglist [([name doc-string? [params*] pre-post? body])
                 ([name doc-string? ([params*] pre-post? body)+])]}
  (let [{: name? : doc? : args : pre-post? : body : multi-arity?}
        ;; descent into maddness
        (match (values ...)
          (where (name docstring [[] &as arity])
                 (and (sym? name)
                      (string? docstring)
                      (list? arity)))
          {:pat '(fn* foo "bar" ([baz]) ...)
           :name? name
           :doc? docstring
           :args [arity (select 4 ...)]
           :multi-arity? true}
          (where (name [[] &as arity])
                 (and (sym? name)
                      (list? arity)))
          {:pat '(fn* foo ([baz]) ...)
           :name? name
           :args [arity (select 3 ...)]
           :multi-arity? true}
          (where (docstring [[] &as arity])
                 (and (string? docstring)
                      (list? arity)))
          {:pat '(fn* "bar" ([baz]) ...)
           :name? (gensym :fn)
           :doc? docstring
           :args [arity (select 3 ...)]
           :multi-arity? true}
          (where ([[] &as arity])
                 (list? arity))
          {:pat '(fn* ([baz]) ...)
           :name? (gensym :fn)
           :args [arity (select 2 ...)]
           :multi-arity? true}
          (where (name docstring args {&as pre-post})
                 (and (sym? name)
                      (string? docstring)
                      (sequence? args)
                      (or (not= nil pre-post.pre)
                          (not= nil pre-post.post))))
          {:pat '(fn* foo "foo" [baz] {:pre qux :post quux} ...)
           :name? name
           :doc? docstring
           :args args
           :pre-post? pre-post
           :body [(select 5 ...)]}
          (where (name docstring args)
                 (and (sym? name)
                      (string? docstring)
                      (sequence? args)))
          {:pat '(fn* foo "foo" [baz] ...)
           :name? name
           :doc? docstring
           :args args
           :body [(select 4 ...)]}
          (where (name args {&as pre-post})
                 (and (sym? name)
                      (sequence? args)
                      (or (not= nil pre-post.pre)
                          (not= nil pre-post.post))))
          {:pat '(fn* foo [baz] {:pre qux :post quux} ...)
           :name? name
           :args args
           :pre-post? pre-post
           :body [(select 4 ...)]}
          (where (name args)
                 (and (sym? name) (sequence? args)))
          {:pat '(fn* foo [baz] ...)
           :name? name
           :args args
           :body [(select 3 ...)]}
          (where (docstring args {&as pre-post})
                 (and (string? docstring)
                      (sequence? args)
                      (or (not= nil pre-post.pre)
                          (not= nil pre-post.post))))
          {:pat '(fn* "bar" [baz] {:pre qux :post quux} ...)
           :name? (gensym :fn)
           :doc? docstring
           :args args
           :pre-post? pre-post
           :body [(select 4 ...)]}
          (where (docstring args)
                 (and (string? docstring)
                      (sequence? args)))
          {:pat '(fn* "bar" [baz] ...)
           :name? (gensym :fn)
           :doc? docstring
           :args args
           :body [(select 3 ...)]}
          (where (args {&as pre-post})
                 (and (sequence? args)
                      (or (not= nil pre-post.pre)
                          (not= nil pre-post.post))))
          {:pat '(fn* [baz] {:pre qux :post quux} ...)
           :name? (gensym :fn)
           :args args
           :pre-post? pre-post
           :body [(select 3 ...)]}
          (where (args)
                 (sequence? args))
          {:pat '(fn* [baz] ...)
           :name? (gensym :fn)
           :args args
           :body [(select 2 ...)]}
          _ (assert-compile (string.format
                             "Expression %s didn't match any pattern."
                             (view `(fn* ,...)))))]
    (if multi-arity?
        (gen-match-fn name? doc? args)
        (gen-fn name? doc? args pre-post? `(do ,(unpack body))))))

(fn defn [name ...]
  {:fnl/docstring
   "Same as `(def name (fn* name docstring? [params*] pre-post? exprs*))`
or `(def name (fn* name docstring? ([params*] pre-post? exprs*)+))`
with any doc-string or attrs added to the function metadata.  Accepts
`name` which will be used to refer to a function in the current
namespace, and optional `doc-string?`, a vector of function's
`params*`, `pre-post?` conditions, and the `body` of the function.
The body is wrapped in an implicit do.  See `fn*` for more info."
   :fnl/arglist [([name doc-string? [params*] pre-post? body])
                 ([name doc-string? ([params*] pre-post? body)+])]}
  (assert-compile (sym? name) "expected a function name, use `fn*` for anonymous functions" name)
  (def name (fn* name ...)))

(fn defn- [name ...]
  {:fnl/docstring
   "Same as `(def :private name (fn* name docstring? [params*] pre-post?
exprs*))` or `(def :private name (fn* name docstring? ([params*]
pre-post?  exprs*)+))` with any doc-string or attrs added to the
function metadata. Accepts `name` which will be used to refer to a
function, and optional `doc-string?`, a vector of function's
`params*`, `pre-post?` conditions, and the `body` of the function.
The body is wrapped in an implicit do. See `fn*` for more info."
   :fnl/arglist [([name doc-string? [params*] pre-post? body])
                 ([name doc-string? ([params*] pre-post? body)+])]}
  (assert-compile (sym? name) "expected a function name, use `fn*` for anonymous functions" name)
  (def :private name (fn* name ...)))

;;; Time

(fn time [expr]
  "Measure the CPU time spent executing `expr`."
  `(let [c# os.clock
         pack# #(doto [$...] (tset :n (select "#" $...)))
         s# (c#)
         res# (pack# ,expr)
         e# (c#)]
     (print (.. "Elapsed time: " (* (- e# s#) 1000) " msecs"))
     ((or table.unpack _G.unpack) res# 1 res#.n)))

;;; let variants

(fn when-let [[name test] ...]
  {:fnl/docstring "When `test` is logical `true`, evaluates the `body` with `name` bound
to the value of `test`."
   :fnl/arglist [[name test] & body]}
  `(let [val# ,test]
     (if val#
         (let [,name val#]
           ,...))))

(fn if-let [[name test] if-branch else-branch ...]
  {:fnl/docstring "When `test` is logical `true`, evaluates the `if-branch` with `name`
bound to the value of `test`. Otherwise, evaluates the `else-branch`"
   :fnl/arglist [[name test] if-branch else-branch]}
  (assert-compile (= 0 (select "#" ...)) "too many arguments to if-let" ...)
  `(let [val# ,test]
     (if val#
         (let [,name val#]
           ,if-branch)
         ,else-branch)))

(fn when-some [[name test] ...]
  {:fnl/docstring "When `test` is not `nil`, evaluates the `body` with `name` bound to
the value of `test`."
   :fnl/arglist [[name test] & body]}
  `(let [val# ,test]
     (if (not= nil val#)
         (let [,name val#]
           ,...))))

(fn if-some [[name test] if-branch else-branch ...]
  {:fnl/docstring "When `test` is not `nil`, evaluates the `if-branch` with `name`
bound to the value of `test`. Otherwise, evaluates the `else-branch`"
   :fnl/arglist [[name test] if-branch else-branch]}
  (assert-compile (= 0 (select "#" ...)) "too many arguments to if-some" ...)
  `(let [val# ,test]
     (if (not= nil val#)
         (let [,name val#]
           ,if-branch)
         ,else-branch)))

;;; Multimethods

(fn defmulti [...]
  {:fnl/arglist [name docstring? dispatch-fn options*]
   :fnl/docstring "Create multifunction `name' with runtime dispatching based on results
from `dispatch-fn'.  Returns a proxy table with `__call` metamethod,
that calls `dispatch-fn' on its arguments.  Amount of arguments
passed, should be the same as accepted by `dispatch-fn'.  Looks for
multimethod based on result from `dispatch-fn'.

Accepts optional `docstring?', and `options*' arguments, where
`options*' is a sequence of key value pairs representing additional
attributes.  Supported options:

`:default` - the default dispatch value, defaults to `:default`.

By default, multifunction has no multimethods, see
`defmethod' on how to add one."}
  (let [[name & options] (if (> (select :# ...) 0) [...]
                             (error "wrong argument amount for defmulti"))
        docstring (if (string? (first options)) (first options))
        options (if docstring (rest options) options)
        dispatch-fn (first options)
        options* (rest options)]
    (assert (= (% (length options*) 2) 0) "wrong argument amount for defmulti")
    (let [options {}]
      (for [i 1 (length options*) 2]
        (tset options (. options* i) (. options* (+ i 1))))
      (def name
        `(let [pairs# (fn [t#]
                        (match (getmetatable t#)
                          {:__pairs p#} (p# t#)
                          ,(sym :_) (pairs t#)))
               {:eq eq#} (require ,core)]
           (setmetatable
            {}
            {:__index (fn [t# key#]
                        (accumulate [res# nil
                                     k# v# (pairs# t#)
                                     :until res#]
                          (when (eq# k# key#)
                            v#)))
             :__call
             (fn [t# ...]
               ,docstring
               (let [dispatch-value# (,dispatch-fn ...)
                     view# (match (pcall require :fennel)
                             (true fennel#) #(fennel#.view $ {:one-line true})
                             ,(sym :_) tostring)]
                 ((or (. t# dispatch-value#)
                      (. t# (or (. ,options :default) :default))
                      (error (.. "No method in multimethod '"
                                 ,(tostring name)
                                 "' for dispatch value: "
                                 (view# dispatch-value#))
                             2)) ...)))
             :__name (.. "multifn " ,(tostring name))
             :__fennelview tostring
             :cljlib/type :multifn}))))))

(fn defmethod [multifn dispatch-val ...]
  {:fnl/arglist [multi-fn dispatch-value fnspec]
   :fnl/docstring "Attach new method to multi-function dispatch value. Accepts the
`multi-fn' as its first argument, the `dispatch-value' as second, and
`fnspec' - a function tail starting from argument list, followed by
function body as in `fn*'.

# Examples
Here are some examples how multimethods can be used.

## Factorial example
Key idea here is that multimethods can call itself with different
values, and will dispatch correctly.  Here, `fac' recursively calls
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

## Multi-arity dispatching
Multi-arity function tails are also supported:

``` fennel
(ns test)

(defmulti foo (fn* ([x] [x]) ([x y] [x y])))

(defmethod foo [10] [_] (print \"I knew I'll get 10\"))
(defmethod foo [10 20] [_ _] (print \"I knew I'll get both 10 and 20\"))
(defmethod foo :default ([x] (print (.. \"Umm, got\" x)))
                        ([x y] (print (.. \"Umm, got both \" x \" and \" y))))
```

Calling `(foo 10)` will print `\"I knew I'll get 10\"`, and calling
`(foo 10 20)` will print `\"I knew I'll get both 10 and 20\"`.
However, calling `foo' with any other numbers will default either to
`\"Umm, got x\"` message, when called with single value, and `\"Umm, got
both x and y\"` when calling with two values.

## Dispatching on object's type
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
      (table.insert res (.. \"[\" (to-lua-str k) \"] = \" (to-lua-str v))))
    (.. \"{\" (table.concat res \", \") \"}\")))
(defmethod to-lua-str :string [x] (.. \"\\\"\" x \"\\\"\"))
(defmethod to-lua-str :default [x] (tostring x))

(assert-eq (to-lua-str {:a {:b 10}}) \"{[\\\"a\\\"] = {[\\\"b\\\"] = 10}}\")

(assert-eq (to-lua-str [:a :b :c [:d {:e :f}]])
           \"{[1] = \\\"a\\\", [2] = \\\"b\\\", [3] = \\\"c\\\", [4] = {[1] = \\\"d\\\", [2] = {[\\\"e\\\"] = \\\"f\\\"}}}\")
```

And if we call it on some table, we'll get a valid Lua table, which we
can then reformat as we want and use in Lua.

All of this can be done with functions, and single entry point
function, that uses if statement and branches on the type, however one
of the additional features of multimethods, is that separate libraries
can extend such multimethod by adding additional claues to it without
needing to patch the source of the function.  For example later on
support for userdata or coroutines can be added to `to-lua-str'
function as a separate multimethods for respective types."}
  (when (= (select :# ...) 0) (error "wrong argument amount for defmethod"))
  `(let [dispatch# ,dispatch-val
         multifn# ,multifn]
     (and (not (. multifn# dispatch#))
          (doto multifn#
            (tset dispatch# ,(fn* ...))))))

;;; loop

(fn assert-tail [tail-sym body]
  "Asserts that the passed in tail-sym function is a tail-call position of the
passed-in body.

Throws an error if it is in a position to be returned or if the function is
situated to be called from a position other than the tail of the passed-in
body."
  (fn last-arg? [form i]
    (= (- (length form) 1) i))

  ;; Tail in special forms are (After macroexpanding):
  ;;
  ;; - Every second form in an if, or the last form
  ;; (if ... (sym ...) (sym ...))
  ;;
  ;; - Last form in a let
  ;; (let [] (sym ...))
  ;;
  ;; - Last form in a do
  ;; (do ... (sym ...))
  ;;
  ;; Anything else fails the assert
  (fn path-tail? [op i form]
    (if (= op 'if) (and (not= 1 i) (or (last-arg? form i) (= 0 (% i 2))))
        (= op 'let) (last-arg? form i)
        (= op 'do) (last-arg? form i)
        false))

  ;; Check the current form for the tail-sym, and if it's in a bad
  ;; place, error out. If we run into other forms, we recurse with the
  ;; comprehension if this is the tail form or not
  (fn walk [body ok]
    (let [[op & operands] body]
      (if (list? op) (walk op true)
          (assert-compile (not (and (= tail-sym op) (not ok)))
                          (.. (tostring tail-sym) " must be in tail position")
                          op)
          (each [i v (ipairs operands)]
            (if (list? v) (walk v (and ok (path-tail? op i body)))
                (assert-compile (not= tail-sym v)
                                (.. (tostring tail-sym) " must not be passed")
                                v))))))

  (walk `(do ,(macroexpand body)) true))


(fn loop [binding-vec ...]
  {:fnl/arglist [binding-vec body*]
   :fnl/docstring "Recursive loop macro.

Similar to `let`, but binds a special `recur` call that will reassign
the values of the `binding-vec` and restart the loop `body*`.  Unlike
`let`, doesn't support multiple-value destructuring.

The first argument is a binding table with alternating symbols (or destructure
forms), and the values to bind to them.

For example:

``` fennel
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

# Limitations

In order to only evaluate expressions once and support sequential
bindings, the binding table has to be transformed like this:

``` fennel :skip-test
(loop [[x & xs] (foo)
       y (+ x 1)]
  ...)

(let [_1_ (foo)
      [x & xs] _1_
      _2_ (+ x 1)
      y _2_]
  ((fn recur [[x & xs] y] ...) _1_ _2_)
```

This ensures that `foo` is called only once, its result is cached in a
`sym1#` binding, and that `y` can use the destructured value, obtained
from that binding.  The value of this binding is later passed to the
function to begin the first iteration.

This has two unfortunate consequences.  One is that the initial
destructuring happens twice - first, to make sure that later bindings
can be properly initialized, and second, when the first looping
function call happens.  Another one is that as a result, `loop` macro
can't work with multiple-value destructuring, because these can't be
cached as described above.  E.g. this will not work:

``` fennel :skip-test
(loop [(x y) (foo)] ...)
```

Because it would be transformed to:

``` fennel :skip-test
(let [_1_ (foo)
      (x y) _1_]
  ((fn recur [(x y)] ...) _1_)
```

`x` is correctly set, but `y` is completely lost.  Therefore, this
macro checks for lists in bindings."}
  (let [recur (sym :recur)
        keys []
        gensyms []
        bindings []]
    (assert-tail recur ...)
    (each [i v (ipairs binding-vec)]
      (when (= 0 (% i 2))
        (let [key (. binding-vec (- i 1))
              gs (gensym i)]
          (assert-compile (not (list? key)) "loop macro doesn't support multiple-value destructuring" key)
          ;; [sym1# sym2# etc...], for the function application below
          (table.insert gensyms gs)

          ;; let bindings
          (table.insert bindings gs)  ;; sym1#
          (table.insert bindings v)   ;; (expression)
          (table.insert bindings key) ;; [first & rest]
          (table.insert bindings gs)  ;; sym1#

          ;; The gensyms we use for function application
          (table.insert keys key))))
    `(let ,bindings
       ((fn ,recur ,keys
          ,...)
        ,(table.unpack gensyms)))))

;;; Try catch finally

(fn catch? [[fun]]
  "Test if expression is a catch clause."
  (= (tostring fun) :catch))

(fn finally? [[fun]]
  "Test if expression is a finally clause."
  (= (tostring fun) :finally))

(fn add-finally [finally form]
  "Stores `form' as body of `finally', which will be injected into
`match' branches at places appropriate for it to run.

Checks if there already was `finally' clause met, which can be only
one."
  (assert-compile (= (length finally) 0)
                  "Only one finally clause can exist in try expression"
                  [])
  (table.insert finally (list 'do ((or table.unpack _G.unpack) form 2))))

(fn add-catch [finally catches form]
  "Appends `catch' body to a sequence of catch bodies that will later
be used in `make-catch-clauses' to produce AST.

Checks if there already was `finally' clause met."
  (assert-compile (= (length finally) 0)
                  "finally clause must be last in try expression"
                  [])
  (table.insert catches (list 'do ((or table.unpack _G.unpack) form 2))))

(fn make-catch-clauses [catches finally]
  "Generates AST of error branches for `match' macro."
  (let [clauses []]
    (var add-catchall? true)
    (each [_ [_ binding-or-val & body] (ipairs catches)]
      (when (sym? binding-or-val)
        (set add-catchall? false))
      (table.insert clauses `(false ,binding-or-val))
      (table.insert clauses `(let [res# ((or table.pack #(doto [$...] (tset :n (select :# $...))))
                                         (do ,((or table.unpack _G.unpack) body)))]
                               ,(. finally 1)
                               (table.unpack res# 1 res#.n))))
    (when add-catchall?
      ;; implicit catchall which retrows error further is added only
      ;; if there were no catch clause that used symbol as catch value
      (table.insert clauses `(false _#))
      (table.insert clauses `(do ,(. finally 1) (error _#))))
    ((or table.unpack _G.unpack) clauses)))

(fn add-to-try [finally catches try form]
  "Append form to the try body.  There must be no `catch' of `finally'
clauses when we push body epression."
  (assert-compile (and (= (length finally) 0)
                       (= (length catches) 0))
                  "Only catch or finally clause can follow catch in try expression"
                  [])
  (table.insert try form))

(fn try [...]
  {:fnl/arglist [body* catch-clause* finally-clause?]
   :fnl/docstring "General purpose try/catch/finally macro.
Wraps its body in `pcall' and checks the return value with `match'
macro.

Catch clause is written either as `(catch symbol body*)`, thus acting
as catch-all, or `(catch value body*)` for catching specific errors.
It is possible to have several `catch' clauses.  If no `catch' clauses
specified, an implicit catch-all clause is created.  `body*', and
inner expressions of `catch-clause*', and `finally-clause?' are
wrapped in implicit `do'.

The `finally` clause is optional, and written as (finally body*).  If
present, it must be the last clause in the `try' form, and the only
`finally' clause.  Note that `finally' clause is for side effects
only, and runs either after succesful run of `try' body, or after any
`catch' clause body, before returning the result.  If no `catch'
clause is provided `finally' runs in implicit catch-all clause, and
trows error to upper scope using `error' function.

To throw error from `try' to catch it with `catch' clause use `error'
or `assert' functions.

# Examples
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
  (table.insert tbl \"a\")
  (table.insert tbl \"b\" \"c\")
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
```"}
  (let [try '(do)
        catches []
        finally []]
    (each [_ form (ipairs [...])]
      (if (list? form)
          (if (catch? form) (add-catch finally catches form)
              (finally? form) (add-finally finally form)
              (add-to-try finally catches try form))
          (add-to-try finally catches try form)))
    `(match (pcall (fn [] ((or table.pack #(doto [$...] (tset :n (select :# $...)))) ,try)))
       (true _#) (do ,(. finally 1) ((or table.unpack _G.unpack) _# 1 _#.n))
       ,(make-catch-clauses catches finally))))

;;; Misc

(fn cond [...]
  "Takes a set of test expression pairs. It evaluates each test one at a
time.  If a test returns logical true, `cond` evaluates and returns
the value of the corresponding expression and doesn't evaluate any of
the other tests or exprs. `(cond)` returns nil."
  (assert-compile (= 0 (% (select "#" ...) 2))
                  "cond requires an even number of forms"
                  ...)
  (if (= 0 (select "#" ...))
      `nil
      `(if ,...)))

;;; Lazy seq

(local lazy-seq-rel-path (if (and ... (string.match ... "init%-macros$"))
                           (string.gsub ... "init%-macros$" "lazy-seq.init-macros")
                           ... (.. ... ".lazy-seq.init-macros")
                           "lazy-seq.init-macros"))

(fn lazy-seq [...]
  {:fnl/docstring "Takes a `body` of expressions that returns a sequence, table or nil,
and yields a lazy sequence that will invoke the body only the first
time `seq` is called, and will cache the result and return it on all
subsequent `seq` calls. See also - `realized?`"
   :fnl/arglist [& body]}
  `(do
     (import-macros {:lazy-seq lazy-seq*} ,lazy-seq-rel-path)
     (let [core# (require ,core)
           res# (lazy-seq* ,...)]
       (match (getmetatable res#)
         mt# (doto mt#
               (tset :cljlib/type :seq)
               (tset :cljlib/conj
                     (fn [s# v#] (core#.cons v# s#)))
               (tset :cljlib/empty #(core#.list))))
       res#)))

(fn lazy-cat [...]
  {:fnl/docstring "Expands to code which yields a lazy sequence of the concatenation of
`colls` - expressions returning collections.  Each expression is not
evaluated until it is needed."
   :fnl/arglist [& colls]}
  `(do
     (import-macros {:lazy-cat lazy-cat*} ,lazy-seq-rel-path)
     (let [core# (require ,core)
           res# (lazy-cat* ,...)]
       (match (getmetatable res#)
         mt# (doto mt#
               (tset :cljlib/type :seq)
               (tset :cljlib/conj
                     (fn [s# v#] (core#.cons v# s#)))
               (tset :cljlib/empty #(core#.list))))
       res#)))

{: fn*
 : defn
 : defn-
 : in-ns
 : ns
 : def
 : time
 : when-let
 : when-some
 : if-let
 : if-some
 : defmulti
 : defmethod
 : cond
 : loop
 : try
 : lazy-seq
 : lazy-cat}
