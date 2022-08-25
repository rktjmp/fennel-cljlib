(import-macros
 {: defn
  : defn-
  : ns
  : def
  : fn*
  : if-let
  : if-some
  : cond}
 (if ... (if (= ... :init) :init-macros ...) :init-macros))

(ns core
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
SOFTWARE."
  (:require [lazy-seq :as lazy]
            [itable :as itable]))

;;; Utility functions

(fn unpack* [x ...]
  (if (core.seq? x)
      (lazy.unpack x)
      (itable.unpack x ...)))

(fn pack* [...]
  (doto [...] (tset :n (select "#" ...))))

(fn pairs* [t]
  (match (getmetatable t)
    {:__pairs p} (p t)
    _ (pairs t)))

(fn ipairs* [t]
  (match (getmetatable t)
    {:__ipairs i} (i t)
    _ (ipairs t)))

(fn length* [t]
  (match (getmetatable t)
    {:__len l} (l t)
    _ (length t)))

(defn apply
  "Apply `f` to the argument list formed by prepending intervening
arguments to `args`, and `f` must support variadic amount of
arguments.

# Examples
Applying `add` to different amount of arguments:

``` fennel
(assert-eq (apply add [1 2 3 4]) 10)
(assert-eq (apply add 1 [2 3 4]) 10)
(assert-eq (apply add 1 2 3 4 5 6 [7 8 9]) 45)
```"
  ([f args] (f (unpack* args)))
  ([f a args] (f a (unpack* args)))
  ([f a b args] (f a b (unpack* args)))
  ([f a b c args] (f a b c (unpack* args)))
  ([f a b c d & args]
   (let [flat-args []
         len (- (length* args) 1)]
     (for [i 1 len]
       (tset flat-args i (. args i)))
     (each [i a (pairs* (. args (+ len 1)))]
       (tset flat-args (+ i len) a))
     (f a b c d (unpack* flat-args)))))

(defn add
  "Sum arbitrary amount of numbers."
  ([] 0)
  ([a] a)
  ([a b] (+ a b))
  ([a b c] (+ a b c))
  ([a b c d] (+ a b c d))
  ([a b c d & rest] (apply add (+ a b c d) rest)))

(defn sub
  "Subtract arbitrary amount of numbers."
  ([] 0)
  ([a] (- a))
  ([a b] (- a b))
  ([a b c] (- a b c))
  ([a b c d] (- a b c d))
  ([a b c d & rest] (apply sub (- a b c d) rest)))

(defn mul
  "Multiply arbitrary amount of numbers."
  ([] 1)
  ([a] a)
  ([a b] (* a b))
  ([a b c] (* a b c))
  ([a b c d] (* a b c d))
  ([a b c d & rest] (apply mul (* a b c d) rest)))

(defn div
  "Divide arbitrary amount of numbers."
  ([a] (/ 1 a))
  ([a b] (/ a b))
  ([a b c] (/ a b c))
  ([a b c d] (/ a b c d))
  ([a b c d & rest] (apply div (/ a b c d) rest)))

(defn le
  "Returns true if nums are in monotonically non-decreasing order"
  ([a] true)
  ([a b] (<= a b))
  ([a b & [c d & more]]
   (if (<= a b)
       (if d (apply le b c d more)
           (<= b c))
       false)))

(defn lt
  "Returns true if nums are in monotonically decreasing order"
  ([a] true)
  ([a b] (< a b))
  ([a b & [c d & more]]
   (if (< a b)
       (if d (apply lt b c d more)
           (< b c))
       false)))

(defn ge
  "Returns true if nums are in monotonically non-increasing order"
  ([a] true)
  ([a b] (>= a b))
  ([a b & [c d & more]]
   (if (>= a b)
       (if d (apply ge b c d more)
           (>= b c))
       false)))

(defn gt
  "Returns true if nums are in monotonically increasing order"
  ([a] true)
  ([a b] (> a b))
  ([a b & [c d & more]]
   (if (> a b)
       (if d (apply gt b c d more)
           (> b c))
       false)))

(defn inc
  "Increase number `x` by one"
  [x]
  (+ x 1))

(defn dec
  "Decrease number `x` by one"
  [x]
  (- x 1))

(defn class
  "Return cljlib type of the `x`, or lua type."
  [x]
  (match (type x)
    :table (match (getmetatable x)
             {:cljlib/type t} t
             _ :table)
    t t))

(defn constantly
  "Returns a function that takes any number of arguments and returns `x`."
  [x]
  (fn [] x))

(defn complement
  "Takes a function `f` and returns the function that takes the same
amount of arguments as `f`, has the same effect, and returns the
oppisite truth value."
  [f]
  (fn*
    ([] (not (f)))
    ([a] (not (f a)))
    ([a b] (not (f a b)))
    ([a b & cs] (not (apply f a b cs)))))

(defn identity
  "Returns its argument."
  [x]
  x)

(defn comp
  "Compose functions."
  ([] identity)
  ([f] f)
  ([f g]
   (fn*
     ([] (f (g)))
     ([x] (f (g x)))
     ([x y] (f (g x y)))
     ([x y z] (f (g x y z)))
     ([x y z & args] (f (apply g x y z args)))))
  ([f g & fs]
   (core.reduce comp (core.cons f (core.cons g fs)))))

(defn eq
  "Comparison function.

Accepts arbitrary amount of values, and does the deep comparison.  If
values implement `__eq` metamethod, tries to use it, by checking if
first value is equal to second value, and the second value is equal to
the first value.  If values are not equal and are tables does the deep
comparison.  Tables as keys are supported."
  ([] true)
  ([_] true)
  ([a b]
   (if (and (= a b) (= b a))
       true
       (= :table (type a) (type b))
       (do (var (res count-a) (values true 0))
           (each [k v (pairs* a) :until (not res)]
             (set res (eq v (do (var (res done) (values nil nil))
                                (each [k* v (pairs* b) :until done]
                                  (when (eq k* k)
                                    (set (res done) (values v true))))
                                res)))
             (set count-a (+ count-a 1)))
           (when res
             (let [count-b (accumulate [res 0 _ _ (pairs* b)]
                             (+ res 1))]
               (set res (= count-a count-b))))
           res)
       false))
  ([a b & cs]
   (and (eq a b) (apply eq b cs))))

(fn deep-index [tbl key]
  "This function uses the `eq` function to compare keys of the given
table `tbl` and the given `key`.  Several other functions also reuse
this indexing method, such as sets."
  (accumulate [res nil
               k v (pairs* tbl)
               :until res]
    (when (eq k key)
      v)))

(fn deep-newindex [tbl key val]
  "This function uses the `eq` function to compare keys of the given
table `tbl` and the given `key`. If the key is found it's being
set, if not a new key is set."
  (var done false)
  (when (= :table (type key))
    (each [k _ (pairs* tbl) :until done]
      (when (eq k key)
        (rawset tbl k val)
        (set done true))))
  (when (not done)
    (rawset tbl key val)))

(defn memoize
  "Returns a memoized version of a referentially transparent function.
The memoized version of the function keeps a cache of the mapping from
arguments to results and, when calls with the same arguments are
repeated often, has higher performance at the expense of higher memory
use."
  [f]
  (let [memo (setmetatable {} {:__index deep-index})]
    (fn* [& args]
      (match (. memo args)
        res (unpack* res 1 res.n)
        _ (let [res (pack* (f ...))]
            (tset memo args res)
            (unpack* res 1 res.n))))))

(defn deref
  "Dereference an object."
  [x]
  (match (getmetatable x)
    {:cljlib/deref f} (f x)
    _ (error "object doesn't implement cljlib/deref metamethod" 2)))

(defn empty
  "Get an empty variant of a given collection."
  [x]
  (match (getmetatable x)
    {:cljlib/empty f} (f)
    _ (match (type x)
        :table []
        :string ""
        _ (error (.. "don't know how to create empty variant of type " _)))))

;;;Tests and predicates

(defn nil?
  "Test if `x` is nil."
  ([] true)
  ([x] (= x nil)))

(defn zero?
  "Test if `x` is equal to zero."
  [x]
  (= x 0))

(defn pos?
  "Test if `x` is greater than zero."
  [x]
  (> x 0))

(defn neg?
  "Test if `x` is less than zero."
  [x]
  (< x 0))

(defn even?
  "Test if `x` is even."
  [x]
  (= (% x 2) 0))

(defn odd?
  "Test if `x` is odd."
  [x]
  (not (even? x)))

(defn string?
  "Test if `x` is a string."
  [x]
  (= (type x) :string))

(defn boolean?
  "Test if `x` is a Boolean"
  [x]
  (= (type x) :boolean))

(defn true?
  "Test if `x` is `true`"
  [x]
  (= x true))

(defn false?
  "Test if `x` is `false`"
  [x]
  (= x false))

(defn int?
  "Test if `x` is a number without floating point data.

Number is rounded with `math.floor` and compared with original number."
  [x]
  (and (= (type x) :number)
       (= x (math.floor x))))

(defn pos-int?
  "Test if `x` is a positive integer."
  [x]
  (and (int? x)
       (pos? x)))

(defn neg-int?
  "Test if `x` is a negative integer."
  [x]
  (and (int? x)
       (neg? x)))

(defn double?
  "Test if `x` is a number with floating point data."
  [x]
  (and (= (type x) :number)
       (not= x (math.floor x))))

(defn empty?
  "Check if collection is empty."
  [x]
  (match (type x)
    :table
    (match (getmetatable x)
      {:cljlib/type :seq}
      (nil? (core.seq x))
      (where (or nil {:cljlib/type nil}))
      (let [(next*) (pairs* x)]
        (= (next* x) nil)))
    :string (= x "")
    :nil true
    _ (error "empty?: unsupported collection")))

(defn not-empty
  "If `x` is empty, returns `nil`, otherwise `x`."
  [x]
  (if (not (empty? x))
      x))

(defn map?
  "Check whether `x` is an associative table.

Non-empty tables are tested by calling `next`. If the length of the
table is greater than zero, the last integer key is passed to the
`next`, and if `next` returns a key, the table is considered
associative. If the length is zero, `next` is called with what `paris`
returns for the table, and if the key is returned, table is considered
associative.

Empty tables can't be analyzed with this method, and `map?` will
always return `false`.  If you need this test pass for empty table,
see `hash-map` for creating tables that have additional metadata
attached for this test to work.

# Examples
Non empty map:

``` fennel
(assert-is (map? {:a 1 :b 2}))
```

Empty tables don't pass the test:

``` fennel
(assert-not (map? {}))
```

Empty tables created with `hash-map` will pass the test:

``` fennel
(assert-is (map? (hash-map)))
```"
  [x]
  (if (= :table (type x))
      (match (getmetatable x)
        {:cljlib/type :hash-map} true
        {:cljlib/type :sorted-map} true
        (where (or nil {:cljlib/type nil}))
        (let [len (length* x)
              (nxt t k) (pairs* x)]
          (not= nil (nxt t (if (= len 0) k len))))
        _ false)
      false))

(defn vector?
  "Check whether `tbl` is an sequential table.

Non empty sequential tables are tested for two things:
- `next` returns the key-value pair,
- key, that is returned by the `next` is equal to `1`.

Empty tables can't be analyzed with this method, and `vector?` will
always return `false`.  If you need this test pass for empty table,
see `vector` for creating tables that have additional
metadata attached for this test to work.

# Examples
Non empty vector:

``` fennel
(assert-is (vector? [1 2 3 4]))
```

Empty tables don't pass the test:

``` fennel
(assert-not (vector? []))
```

Empty tables created with `vector` will pass the test:

``` fennel
(assert-is (vector? (vector)))
```"
  [x]
  (if (= :table (type x))
      (match (getmetatable x)
        {:cljlib/type :vector} true
        (where (or nil {:cljlib/type nil}))
        (let [len (length* x)
              (nxt t k) (pairs* x)]
          (if (not= nil (nxt t (if (= len 0) k len))) false
              (> len 0) true
              false))
        _ false)
      false))

(defn set?
  "Check if object is a set."
  [x]
  (match (getmetatable x)
    {:cljlib/type :hash-set} true
    _ false))

(defn seq?
  "Check if object is a sequence."
  [x]
  (lazy.seq? x))

(defn some?
  "Returns true if x is not nil, false otherwise."
  [x]
  (not= x nil))

;;; Vector

(fn vec->transient [immutable]
  (fn [vec]
    (var len (length vec))
    (->> {:__index (fn [_ i]
                     (if (<= i len)
                         (. vec i)))
          :__len #len
          :cljlib/type :transient
          :cljlib/conj #(error "can't `conj` onto transient vector, use `conj!`")
          :cljlib/assoc #(error "can't `assoc` onto transient vector, use `assoc!`")
          :cljlib/dissoc #(error "can't `dissoc` onto transient vector, use `dissoc!`")
          :cljlib/conj! (fn [tvec v]
                          (set len (+ len 1))
                          (doto tvec (tset len v)))
          :cljlib/assoc! (fn [tvec ...]
                           (let [len (length tvec)]
                             (for [i 1 (select "#" ...) 2]
                               (let [(k v) (select i ...)]
                                 (if (<= 1 i len)
                                     (tset tvec i v)
                                     (error (.. "index " i " is out of bounds"))))))
                           tvec)
          :cljlib/pop! (fn [tvec]
                         (if (= len 0)
                             (error "transient vector is empty" 2)
                             (let [val (table.remove tvec)]
                               (set len (- len 1))
                               tvec)))
          :cljlib/dissoc! #(error "can't `dissoc!` with a transient vector")
          :cljlib/persistent! (fn [tvec]
                                (let [v (fcollect [i 1 len] (. tvec i))]
                                  (while (> len 0)
                                    (table.remove tvec)
                                    (set len (- len 1)))
                                  (setmetatable tvec
                                                {:__index #(error "attempt to use transient after it was persistet")
                                                 :__newindex #(error "attempt to use transient after it was persistet")})
                                  (immutable (itable v))))}
         (setmetatable {}))))

(fn vec* [v len]
  (match (getmetatable v)
    mt (doto mt
         (tset :__len (constantly (or len (length* v))))
         (tset :cljlib/type :vector)
         (tset :cljlib/editable true)
         (tset :cljlib/conj
               (fn [t v]
                 (let [len (length* t)]
                   (vec* (itable.assoc t (+ len 1) v) (+ len 1)))))
         (tset :cljlib/pop
               (fn [t]
                 (let [len (- (length* t) 1)
                       coll []]
                   (when (< len 0)
                     (error "can't pop empty vector" 2))
                   (for [i 1 len]
                     (tset coll i (. t i)))
                   (vec* (itable coll) len))))
         (tset :cljlib/empty
               (fn [] (vec* (itable []))))
         (tset :cljlib/transient (vec->transient vec*))
         (tset :__fennelview (fn [coll view inspector indent]
                               (if (empty? coll)
                                   "[]"
                                   (let [lines (fcollect [i 1 (length* coll)]
                                                 (.. " " (view (. coll i) inspector indent)))]
                                     (tset lines 1 (.. "[" (string.gsub (or (. lines 1) "") "^%s+" "")))
                                     (tset lines (length lines) (.. (. lines (length lines)) "]"))
                                     lines)))))
    nil (vec* (setmetatable v {})))
  v)

(defn vec
  "Coerce collection `coll` to a vector."
  [coll]
  (cond (empty? coll) (vec* (itable []) 0)
        (vector? coll) (vec* (itable coll) (length* coll))
        :else (let [packed (-> coll core.seq lazy.pack)
                    len packed.n]
                (-> packed
                    (doto (tset :n nil))
                    (itable {:fast-index? true})
                    (vec* len)))))

(defn vector
  "Constructs sequential table out of it's arguments.

Sets additional metadata for function `vector?` to work.

# Examples

``` fennel
(def :private v (vector 1 2 3 4))
(assert-eq v [1 2 3 4])
```"
  [& args]
  (vec args))

(defn nth
  "Returns the value at the `index`. `get` returns `nil` if `index` out
of bounds, `nth` raises an error unless `not-found` is supplied.
`nth` also works for strings and sequences."
  ([coll i]
   (if (vector? coll)
       (if (or (< i 1) (< (length* coll) i))
           (error (string.format "index %d is out of bounds" i))
           (. coll i))
       (string? coll)
       (nth (vec coll) i)
       (seq? coll)
       (nth (vec coll) i)
       :else
       (error "expected an indexed collection")))
  ([coll i not-found]
   (assert (int? i) "expected an integer key")
   (if (vector? coll)
       (or (. coll i) not-found)
       (string? coll)
       (nth (vec coll) i not-found)
       (seq? coll)
       (nth (vec coll) i not-found)
       :else
       (error "expected an indexed collection"))))

;;; Sequences

(defn- seq*
  "Add cljlib sequence meta-info."
  [x]
  (match (getmetatable x)
    mt (doto mt
         (tset :cljlib/type :seq)
         (tset :cljlib/conj
               (fn [s v] (core.cons v s)))
         (tset :cljlib/empty #(core.list))))
  x)

(defn seq
  "Construct a sequnce from the given collection `coll`.  If `coll` is an
associative table, returns sequence of vectors with key and
value.  If `col` is sequential table, returns its shallow copy.  If
`col` is string, return sequential table of its codepoints.

# Examples
Sequential tables are transformed to sequences:

``` fennel
(seq [1 2 3 4]) ;; @seq(1 2 3 4)
```

Associative tables are transformed to format like this `[[key1 value1]
... [keyN valueN]]` and order is non deterministic:

``` fennel
(seq {:a 1 :b 2 :c 3}) ;; @seq([:b 2] [:a 1] [:c 3])
```"
  [coll]
  (seq* (match (getmetatable coll)
          {:cljlib/seq f} (f coll)
          _ (cond (lazy.seq? coll) (lazy.seq coll)
                  (map? coll) (lazy.map vec coll)
                  :else (lazy.seq coll)))))

(defn rseq
  "Returns, in possibly-constant time, a seq of the items in `rev` in reverse order.
Input must be traversable with `ipairs`.  Doesn't work in constant
time if `rev` implements a linear-time `__len` metamethod, or invoking
Lua `#` operator on `rev` takes linar time.  If `t` is empty returns
`nil`.

# Examples

``` fennel
(def :private v [1 2 3])
(def :private r (rseq v))

(assert-eq (reverse v) r)
```"
  [rev]
  (seq* (lazy.rseq rev)))

(defn lazy-seq
  "Create lazy sequence from the result of calling a function `f`.
Delays execution of `f` until sequence is consumed.  `f` must return a
sequence or a vector."
  [f]
  (seq* (lazy.lazy-seq f)))

(defn first
  "Return first element of a `coll`. Calls `seq` on its argument."
  [coll]
  (lazy.first (seq coll)))

(defn rest
  "Returns a sequence of all elements of a `coll` but the first one.
Calls `seq` on its argument."
  [coll]
  (seq* (lazy.rest (seq coll))))

(defn- next*
  "Return the tail of a sequence.

If the sequence is empty, returns nil."
  [s]
  (seq* (lazy.next s)))

(doto core (tset :next next*)) ; luajit doesn't like next redefinition

(defn count
  "Count amount of elements in the sequence."
  [s]
  (lazy.count s))

(defn cons
  "Construct a cons cell.
Prepends new `head` to a `tail`, which must be either a table,
sequence, or nil.

# Examples

``` fennel
(assert-eq [0 1] (cons 0 [1]))
(assert-eq (list 0 1 2 3) (cons 0 (cons 1 (list 2 3))))
```"
  [head tail]
  (seq* (lazy.cons head tail)))

(fn list
  [...]
  "Create eager sequence of provided values.

# Examples

``` fennel
(local l (list 1 2 3 4 5))
(assert-eq [1 2 3 4 5] l)
```"
  (seq* (lazy.list ...)))

(set core.list list)

(defn list*
  "Creates a new sequence containing the items prepended to the rest,
the last of which will be treated as a sequence.

# Examples

``` fennel
(local l (list* 1 2 3 [4 5]))
(assert-eq [1 2 3 4 5] l)
```"
  [& args]
  (seq* (apply lazy.list* args)))

(defn last
  "Returns the last element of a `coll`. Calls `seq` on its argument."
  [coll]
  (match (next* coll)
    coll* (last coll*)
    _ (first coll)))

(defn butlast
  "Returns everything but the last element of the `coll` as a new
  sequence.  Calls `seq` on its argument."
  [coll]
  (seq (lazy.drop-last coll)))

(defn map
  "Returns a lazy sequence consisting of the result of applying `f` to
the set of first items of each `coll`, followed by applying `f` to the
set of second items in each `coll`, until any one of the `colls` is
exhausted.  Any remaining items in other `colls` are ignored. Function
`f` should accept number-of-colls arguments. Returns a transducer when
no collection is provided.

# Examples

```fennel
(map #(+ $ 1) [1 2 3]) ;; => @seq(2 3 4)
(map #(+ $1 $2) [1 2 3] [4 5 6]) ;; => @seq(5 7 9)
(def :private res (map #(+ $ 1) [:a :b :c])) ;; will raise an error only when realized
```"
  ([f]
   (fn* [rf]
     (fn*
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (rf result (f input)))
       ([result input & inputs]
        (rf result (apply f input inputs))))))
  ([f coll]
   (seq* (lazy.map f coll)))
  ([f coll & colls]
   (seq* (apply lazy.map f coll colls))))

(defn mapv
  "Returns a vector consisting of the result of applying `f` to the
set of first items of each `coll`, followed by applying `f` to the set
of second items in each coll, until any one of the `colls` is exhausted.
Any remaining items in other colls are ignored.  Function `f` should
accept number-of-colls arguments."
  ([f coll]
   (->> coll
        (core.transduce (map f)
                        core.conj!
                        (core.transient (vector)))
        core.persistent!))
  ([f coll & colls] (vec (apply map f coll colls))))

(defn map-indexed
  "Returns a lazy sequence consisting of the result of applying `f` to 1
and the first item of `coll`, followed by applying `f` to 2 and the
second item in `coll`, etc, until `coll` is exhausted.  Returns a
transducer when no collection is provided."
  ([f]
   (fn* [rf]
     (var i -1)
     (fn*
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (set i (+ i 1))
        (rf result (f i input))))))
  ([f coll]
   (seq* (lazy.map-indexed f coll))))

(defn mapcat
  "Apply `concat` to the result of calling `map` with `f` and
collections `colls`. Returns a transducer when no collection is
provided."
  ([f]
   (comp (map f) core.cat))
  ([f & colls]
   (seq* (apply lazy.mapcat f colls))))

(defn filter
  "Returns a lazy sequence of the items in `coll` for which
`pred` returns logical true. Returns a transducer when no collection
is provided."
  ([pred]
   (fn* [rf]
     (fn*
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (if (pred input)
            (rf result input)
            result)))))
  ([pred coll]
   (seq* (lazy.filter pred coll))))

(defn filterv
  "Returns a vector of the items in `coll` for which
`pred` returns logical true."
  [pred coll]
  (vec (filter pred coll)))

(defn every?
  "Test if every item in `coll` satisfies the `pred`."
  [pred coll]
  (lazy.every? pred coll))

(defn some
  "Test if any item in `coll` satisfies the `pred`."
  [pred coll]
  (lazy.some? pred coll))

(defn not-any?
  "Test if no item in `coll` satisfy the `pred`."
  [pred coll]
  (some #(not (pred $)) coll))

(defn range
  "Returns lazy sequence of of numbers from `lower` to `upper` with optional `step`."
  ([] (seq* (lazy.range)))
  ([upper] (seq* (lazy.range upper)))
  ([lower upper] (seq* (lazy.range lower upper)))
  ([lower upper step] (seq* (lazy.range lower upper step))))

(defn concat
  "Return a lazy sequence of concatenated `colls`."
  [& colls]
  (seq* (apply lazy.concat colls)))

(defn reverse
  "Returns a lazy sequnce with same items as in `coll` but in reverse order."
  [coll]
  (seq* (lazy.reverse coll)))

(defn take
  "Returns a lazy sequence of the first `n` items in `coll`, or all items if
there are fewer than `n`."
  ([n]
   (fn* [rf]
     (var n n)
     (fn*
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (let [result (if (< 0 n)
                         (rf result input)
                         result)]
          (set n (- n 1))
          (if (not (< 0 n))
              (core.ensure-reduced result)
              result))))))
  ([n coll]
   (seq* (lazy.take n coll))))

(defn take-while
  "Take the elements from the collection `coll` until `pred` returns logical
false for any of the elemnts.  Returns a lazy sequence. Returns a
transducer when no collection is provided."
  ([pred]
   (fn* [rf]
     (fn*
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (if (pred input)
            (rf result input)
            (core.reduced result))))))
  ([pred coll]
   (seq* (lazy.take-while pred coll))))

(defn drop
  "Drop `n` elements from collection `coll`, returning a lazy sequence
of remaining elements. Returns a transducer when no collection is
provided."
  ([n]
   (fn* [rf]
     (var nv n)
     (fn*
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (let [n nv]
          (set nv (- nv 1))
          (if (pos? n)
              result
              (rf result input)))))))
  ([n coll]
   (seq* (lazy.drop n coll))))

(defn drop-while
  "Drop the elements from the collection `coll` until `pred` returns logical
false for any of the elemnts.  Returns a lazy sequence. Returns a
transducer when no collection is provided."
  ([pred]
   (fn* [rf]
     (var dv true)
     (fn*
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (let [drop? dv]
          (if (and drop? (pred input))
              result
              (do
                (set dv nil)
                (rf result input))))))))
  ([pred coll]
   (seq* (lazy.drop-while pred coll))))

(defn drop-last
  "Return a lazy sequence from `coll` without last `n` elements."
  ([] (seq* (lazy.drop-last)))
  ([coll] (seq* (lazy.drop-last coll)))
  ([n coll] (seq* (lazy.drop-last n coll))))

(defn take-last
  "Return a sequence of last `n` elements of the `coll`."
  [n coll]
  (seq* (lazy.take-last n coll)))

(defn take-nth
  "Return a lazy sequence of every `n` item in `coll`. Returns a
transducer when no collection is provided."
  ([n]
   (fn* [rf]
     (var iv -1)
     (fn*
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (set iv (+ iv 1))
        (if (= 0 (% iv n))
            (rf result input)
            result)))))
  ([n coll]
   (seq* (lazy.take-nth n coll))))

(defn split-at
  "Return a table with sequence `coll` being split at `n`"
  [n coll]
  (vec (lazy.split-at n coll)))

(defn split-with
  "Return a table with sequence `coll` being split with `pred`"
  [pred coll]
  (vec (lazy.split-with pred coll)))

(defn nthrest
  "Returns the nth rest of `coll`, `coll` when `n` is 0.

# Examples

``` fennel
(assert-eq (nthrest [1 2 3 4] 3) [4])
(assert-eq (nthrest [1 2 3 4] 2) [3 4])
(assert-eq (nthrest [1 2 3 4] 1) [2 3 4])
(assert-eq (nthrest [1 2 3 4] 0) [1 2 3 4])
```
"
  [coll n]
  (seq* (lazy.nthrest coll n)))

(defn nthnext
  "Returns the nth next of `coll`, (seq coll) when `n` is 0."
  [coll n]
  (lazy.nthnext coll n))

(defn keep
  "Returns a lazy sequence of the non-nil results of calling `f` on the
items of the `coll`. Returns a transducer when no collection is
provided."
  ([f]
   (fn* [rf]
     (fn*
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (let [v (f input)]
          (if (nil? v)
              result
              (rf result v)))))))
  ([f coll]
   (seq* (lazy.keep f coll))))

(defn keep-indexed
  "Returns a lazy sequence of the non-nil results of (f index item) in
the `coll`.  Note, this means false return values will be included.
`f` must be free of side-effects. Returns a transducer when no
collection is provided."
  ([f]
   (fn* [rf]
     (var iv -1)
     (fn*
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (set iv (+ iv 1))
        (let [v (f iv input)]
          (if (nil? v)
              result
              (rf result v)))))))
  ([f coll]
   (seq* (lazy.keep-indexed f coll))))

(defn partition
  "Given a collection `coll`, returns a lazy sequence of lists of `n`
items each, at offsets `step` apart. If `step` is not supplied,
defaults to `n`, i.e. the partitions do not overlap. If a `pad`
collection is supplied, use its elements as necessary to complete last
partition upto `n` items. In case there are not enough padding
elements, return a partition with less than `n` items."
  ([n coll] (map seq* (lazy.partition n coll)))
  ([n step coll] (map seq* (lazy.partition n step coll)))
  ([n step pad coll] (map seq* (lazy.partition n step pad coll))))

(fn array []
  (var len 0)
  (->> {:__len #len
        :__index {:clear (fn [self]
                           (while (not= 0 len)
                             (tset self len nil)
                             (set len (- len 1))
                             self))
                  :add (fn [self val]
                         (set len (+ len 1))
                         (tset self len val)
                         self)}}
       (setmetatable [])))

(defn partition-by
  "Applies `f` to each value in `coll`, splitting it each time `f`
returns a new value.  Returns a lazy seq of partitions.  Returns a
transducer, if collection is not supplied."
  ([f]
   (fn* [rf]
     (let [a (array)
           none {}]
       (var pv none)
       (fn*
         ([] (rf))
         ([result]
          (rf (if (empty? a)
                  result
                  (let [v (vec a)]
                    (a:clear)
                    (core.unreduced (rf result v))))))
         ([result input]
          (let [pval pv
                val (f input)]
            (set pv val)
            (if (or (= pval none)
                    (= val pval))
                (do
                  (a:add input)
                  result)
                (let [v (vec a)]
                  (a:clear)
                  (let [ret (rf result v)]
                    (when (not (core.reduced? ret))
                      (a:add input))
                    ret)))))))))
  ([f coll]
   (map seq* (lazy.partition-by f coll))))

(defn partition-all
  "Given a collection `coll`, returns a lazy sequence of lists like
`partition`, but may include partitions with fewer than n items at the
end. Accepts addiitonal `step` argument, similarly to `partition`.
Returns a transducer, if collection is not supplied."
  ([n]
   (fn* [rf]
     (let [a (array)]
       (fn*
         ([] (rf))
         ([result]
          (rf (if (= 0 (length a))
                  result
                  (let [v (vec a)]
                    (a:clear)
                    (core.unreduced (rf result v))))))
         ([result input]
          (a:add input)
          (if (= n (length a))
              (let [v (vec a)]
                (a:clear)
                (rf result v))
              result))))))
  ([n coll]
   (map seq* (lazy.partition-all n coll)))
  ([n step coll]
   (map seq* (lazy.partition-all n step coll))))

(defn reductions
  "Returns a lazy seq of the intermediate values of the reduction (as
per reduce) of `coll` by `f`, starting with `init`."
  ([f coll] (seq* (lazy.reductions f coll)))
  ([f init coll] (seq* (lazy.reductions f init coll))))

(defn contains?
  "Test if `elt` is in the `coll`.  May be a linear search depending on the type of the collection."
  [coll elt]
  (lazy.contains? coll elt))

(defn distinct
  "Returns a lazy sequence of the elements of the `coll` without
duplicates.  Comparison is done by equality. Returns a transducer when
no collection is provided."
  ([]
   (fn* [rf]
     (let [seen (setmetatable {} {:__index deep-index})]
       (fn*
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (. seen input)
              result
              (do
                (tset seen input true)
                (rf result input))))))))
  ([coll]
   (seq* (lazy.distinct coll))))

(defn dedupe
  "Returns a lazy sequence removing consecutive duplicates in coll.
Returns a transducer when no collection is provided."
  ([]
   (fn* [rf]
     (let [none {}]
       (var pv none)
       (fn*
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [prior pv]
            (set pv input)
            (if (= prior input)
                result
                (rf result input))))))))
  ([coll] (core.sequence (dedupe) coll)))

(defn random-sample
  "Returns items from coll with random probability of prob (0.0 -
1.0).  Returns a transducer when no collection is provided."
  ([prob]
   (filter (fn [] (< (math.random) prob))))
  ([prob coll]
   (filter (fn [] (< (math.random) prob)) coll)))

(defn doall
  "Realize whole lazy sequence `seq`.

Walks whole sequence, realizing each cell.  Use at your own risk on
infinite sequences."
  [seq]
  (seq* (lazy.doall seq)))

(defn dorun
  "Realize whole sequence `seq` for side effects.

Walks whole sequence, realizing each cell.  Use at your own risk on
infinite sequences."
  [seq]
  (lazy.dorun seq))

(defn line-seq
  "Accepts a `file` handle, and creates a lazy sequence of lines using
`lines` metamethod.

# Examples

Lazy sequence of file lines may seem similar to an iterator over a
file, but the main difference is that sequence can be shared onve
realized, and iterator can't.  Lazy sequence can be consumed in
iterator style with the `doseq` macro.

Bear in mind, that since the sequence is lazy it should be realized or
truncated before the file is closed:

```fennel
(let [lines (with-open [f (io.open \"init.fnl\" :r)]
              (line-seq f))]
  ;; this errors because only first line was realized, but the file
  ;; was closed before the rest of lines were cached
  (assert-not (pcall next lines)))
```

Sequence is realized with `doall` before file was closed and can be shared:

``` fennel
(let [lines (with-open [f (io.open \"init.fnl\" :r)]
              (doall (line-seq f)))]
  (assert-is (pcall next lines)))
```

Infinite files can't be fully realized, but can be partially realized
with `take`:

``` fennel
(let [lines (with-open [f (io.open \"/dev/urandom\" :r)]
              (doall (take 3 (line-seq f))))]
  (assert-is (pcall next lines)))
```"
  [file]
  (seq* (lazy.line-seq file)))

(defn iterate
  "Returns an infinete lazy sequence of x, (f x), (f (f x)) etc."
  [f x]
  (seq* (lazy.iterate f x)))

(defn remove
  "Returns a lazy sequence of the items in the `coll` without elements
for wich `pred` returns logical true. Returns a transducer when no
collection is provided."
  ([pred]
   (filter (complement pred)))
  ([pred coll]
   (seq* (lazy.remove pred coll))))

(defn cycle
  "Create a lazy infinite sequence of repetitions of the items in the
`coll`."
  [coll]
  (seq* (lazy.cycle coll)))

(defn repeat
  "Takes a value `x` and returns an infinite lazy sequence of this value.

# Examples

``` fennel
(assert-eq 10 (accumulate [res 0
                           _ x (pairs (take 10 (repeat 1)))]
                (+ res x)))
```"
  [x]
  (seq* (lazy.repeat x)))

(defn repeatedly
  "Takes a function `f` and returns an infinite lazy sequence of
function applications.  Rest arguments are passed to the function."
  [f & args]
  (seq* (apply lazy.repeatedly f args)))

(defn tree-seq
  "Returns a lazy sequence of the nodes in a tree, via a depth-first walk.

`branch?` must be a function of one arg that returns true if passed a
node that can have children (but may not).  `children` must be a
function of one arg that returns a sequence of the children.  Will
only be called on nodes for which `branch?` returns true.  `root` is
the root node of the tree.

# Examples

For the given tree `[\"A\" [\"B\" [\"D\"] [\"E\"]] [\"C\" [\"F\"]]]`:

        A
       / \\
      B   C
     / \\   \\
    D   E   F

Calling `tree-seq` with `next` as the `branch?` and `rest` as the
`children` returns a flat representation of a tree:

``` fennel
(assert-eq (map first (tree-seq next rest [\"A\" [\"B\" [\"D\"] [\"E\"]] [\"C\" [\"F\"]]]))
           [\"A\" \"B\" \"D\" \"E\" \"C\" \"F\"])
```"
  [branch? children root]
  (seq* (lazy.tree-seq branch? children root)))

(defn interleave
  "Returns a lazy sequence of the first item in each sequence, then the
second one, until any sequence exhausts."
  ([] (seq* (lazy.interleave)))
  ([s] (seq* (lazy.interleave s)))
  ([s1 s2] (seq* (lazy.interleave s1 s2)))
  ([s1 s2 & ss] (seq* (apply lazy.interleave s1 s2 ss))))

(defn interpose
  "Returns a lazy sequence of the elements of `coll` separated by
`separator`. Returns a transducer when no collection is provided."
  ([sep]
   (fn* [rf]
     (var started false)
     (fn*
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (if started
            (let [sepr (rf result sep)]
              (if (core.reduced? sepr)
                  sepr
                  (rf sepr input)))
            (do
              (set started true)
              (rf result input)))))))
  ([separator coll]
   (seq* (lazy.interpose separator coll))))

(defn halt-when
  "Returns a transducer that ends transduction when `pred` returns `true`
for an input. When `retf` is supplied it must be a `fn` of 2 arguments
- it will be passed the (completed) result so far and the input that
triggered the predicate, and its return value (if it does not throw an
exception) will be the return value of the transducer. If `retf` is
not supplied, the input that triggered the predicate will be
returned. If the predicate never returns `true` the transduction is
unaffected."
  ([pred]
   (halt-when pred nil))
  ([pred retf]
   (fn* [rf]
     (let [halt (setmetatable {} {:__fennelview #"#<halt>"})]
       (fn*
         ([] (rf))
         ([result]
          (if (and (map? result) (contains? result halt))
              result.value
              (rf result)))
         ([result input]
          (if (pred input)
              (core.reduced {halt true :value (if retf (retf (rf result) input) input)})
              (rf result input))))))))

(defn realized?
  "Check if sequence's first element is realized."
  [s]
  (lazy.realized? s))

(defn keys
  "Returns a sequence of the map's keys, in the same order as `seq`."
  [coll]
  (assert (or (map? coll) (empty? coll)) "expected a map")
  (if (empty? coll)
      (lazy.list)
      (lazy.keys coll)))

(defn vals
  "Returns a sequence of the table's values, in the same order as `seq`."
  [coll]
  (assert (or (map? coll) (empty? coll)) "expected a map")
  (if (empty? coll)
      (lazy.list)
      (lazy.vals coll)))

(defn find
  "Returns the map entry for `key`, or `nil` if key not present in `coll`."
  [coll key]
  (assert (or (map? coll) (empty? coll)) "expected a map")
  (match (. coll key)
    v [key v]))

(defn sort
  "Returns a sorted sequence of the items in `coll`. If no `comparator`
is supplied, uses `<`."
  ([coll]
   (match (seq coll)
     s (seq (itable.sort (vec s)))
     _ (list)))
  ([comparator coll]
   (match (seq coll)
     s (seq (itable.sort (vec s) comparator))
     _ (list))))

;;; Reduce

(defn reduce
  "`f` should be a function of 2 arguments. If `val` is not supplied,
returns the result of applying `f` to the first 2 items in `coll`,
then applying `f` to that result and the 3rd item, etc. If `coll`
contains no items, f must accept no arguments as well, and reduce
returns the result of calling `f` with no arguments.  If `coll` has
only 1 item, it is returned and `f` is not called.  If `val` is
supplied, returns the result of applying `f` to `val` and the first
item in `coll`, then applying `f` to that result and the 2nd item,
etc. If `coll` contains no items, returns `val` and `f` is not
called. Early termination is supported via `reduced`.

# Examples

``` fennel
(defn- add
  ([] 0)
  ([a] a)
  ([a b] (+ a b))
  ([a b & cs] (apply add (+ a b) cs)))
;; no initial value
(assert-eq 10 (reduce add [1 2 3 4]))
;; initial value
(assert-eq 10 (reduce add 1 [2 3 4]))
;; empty collection - function is called with 0 args
(assert-eq 0 (reduce add []))
(assert-eq 10.3 (reduce math.floor 10.3 []))
;; collection with a single element doesn't call a function unless the
;; initial value is supplied
(assert-eq 10.3 (reduce math.floor [10.3]))
(assert-eq 7 (reduce add 3 [4]))
```"
  ([f coll] (lazy.reduce f (seq coll)))
  ([f val coll] (lazy.reduce f val (seq coll))))

(defn reduced
  "Terminates the `reduce` early with a given `value`.

# Examples

``` fennel
(assert-eq :NaN
           (reduce (fn [acc x]
                     (if (not= :number (type x))
                         (reduced :NaN)
                         (+ acc x)))
                   [1 2 :3 4 5]))
```"
  [value]
  (doto (lazy.reduced value)
    (-> getmetatable (tset :cljlib/deref #$.value))))

(defn reduced?
  "Returns true if `x` is the result of a call to reduced"
  [x]
  (lazy.reduced? x))

(defn unreduced
  [x]
  (if (reduced? x) (deref x) x))

(defn ensure-reduced
  "If x is already reduced?, returns it, else returns (reduced x)"
  [x]
  (if (reduced? x)
      x
      (reduced x)))

(defn- preserving-reduced [rf]
  (fn* [a b]
    (let [ret (rf a b)]
      (if (reduced? ret)
          (reduced ret)
          ret))))

(defn cat
  "A transducer which concatenates the contents of each input, which must be a
  collection, into the reduction."
  [rf]
  (let [rrf (preserving-reduced rf)]
    (fn*
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (reduce rrf result input)))))

(defn reduce-kv
  "Reduces an associative table using function `f` and initial value `val`.

`f` should be a function of 3 arguments.  Returns the result of
applying `f` to `val`, the first key and the first value in `tbl`,
then applying `f` to that result and the 2nd key and value, etc.  If
`tbl` contains no entries, returns `val` and `f` is not called.  Note
that reduce-kv is supported on sequential tables and strings, where
the keys will be the ordinals.

Early termination is possible with the use of `reduced`
function.

# Examples
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
```"
  [f val s]
  (if (map? s)
      (reduce (fn [res [k v]] (f res k v)) val (seq s))
      (reduce (fn [res [k v]] (f res k v)) val (map vector (drop 1 (range)) (seq s)))))

(defn completing
  "Takes a reducing function `f` of 2 args and returns a function
suitable for transduce by adding an arity-1 signature that calls
`cf` (default - `identity`) on the result argument."
  ([f] (completing f identity))
  ([f cf]
   (fn*
     ([] (f))
     ([x] (cf x))
     ([x y] (f x y)))))

(defn transduce
  "`reduce` with a transformation of `f` (`xform`). If `init` is not
supplied, `f` will be called to produce it. f should be a reducing
step function that accepts both 1 and 2 arguments, if it accepts only
2 you can add the arity-1 with `completing`. Returns the result of
applying (the transformed) `xform` to `init` and the first item in
`coll`, then applying `xform` to that result and the 2nd item, etc. If
`coll` contains no items, returns `init` and `f` is not called. Note
that certain transforms may inject or skip items."
  ([xform f coll] (transduce xform f (f) coll))
  ([xform f init coll]
   (let [f (xform f)]
     (f (reduce f init (seq coll))))))

(defn sequence
  "Coerces coll to a (possibly empty) sequence, if it is not already
one. Will not force a lazy seq. `(sequence nil)` yields an empty list,
When a transducer is supplied, returns a lazy sequence of applications
of the transform to the items in `coll`, i.e. to the set of first
items of each `coll`, followed by the set of second items in each
`coll`, until any one of the `colls` is exhausted.  Any remaining
items in other `colls` are ignored. The transform should accept
number-of-colls arguments"
  ([coll]
   (if (seq? coll) coll
       (or (seq coll) (list))))
  ([xform coll]
   (let [f (xform (completing #(cons $2 $1)))]
     (or ((fn step [coll]
            (if-some [s (seq coll)]
              (let [res (f nil (first s))]
                (cond (reduced? res) (f (deref res))
                      (seq? res) (concat res (lazy-seq #(step (rest s))))
                      :else (step (rest s))))
              (f nil)))
          coll)
         (list))))
  ([xform coll & colls]
   (let [f (xform (completing #(cons $2 $1)))]
     (or ((fn step [colls]
            (if (every? seq colls)
                (let [res (apply f nil (map first colls))]
                  (cond (reduced? res) (f (deref res))
                        (seq? res) (concat res (lazy-seq #(step (map rest colls))))
                        :else (step (map rest colls))))
                (f nil)))
          (cons coll colls))
         (list)))))

;;; Hash map

(fn map->transient [immutable]
  (fn [map]
    (let [removed (setmetatable {} {:__index deep-index})]
      (->> {:__index (fn [_ k]
                       (if (not (. removed k))
                           (. map k)))
            :cljlib/type :transient
            :cljlib/conj #(error "can't `conj` onto transient map, use `conj!`")
            :cljlib/assoc #(error "can't `assoc` onto transient map, use `assoc!`")
            :cljlib/dissoc #(error "can't `dissoc` onto transient map, use `dissoc!`")
            :cljlib/conj! (fn [tmap [k v]]
                            (if (= nil v)
                                (tset removed k true)
                                (tset removed k nil))
                            (doto tmap (tset k v)))
            :cljlib/assoc! (fn [tmap ...]
                             (for [i 1 (select "#" ...) 2]
                               (let [(k v) (select i ...)]
                                 (tset tmap k v)
                                 (if (= nil v)
                                     (tset removed k true)
                                     (tset removed k nil))))
                             tmap)
            :cljlib/dissoc! (fn [tmap ...]
                              (for [i 1 (select "#" ...)]
                                (let [k (select i ...)]
                                  (tset tmap k nil)
                                  (tset removed k true)))
                              tmap)
            :cljlib/persistent! (fn [tmap]
                                  (let [t (collect [k v (pairs tmap)
                                                    :into (collect [k v (pairs map)]
                                                            (values k v))]
                                            (values k v))]
                                    (each [k (pairs removed)]
                                      (tset t k nil))
                                    (each [_ k (ipairs (icollect [k (pairs* tmap)] k))]
                                      (tset tmap k nil))
                                    (setmetatable tmap
                                                  {:__index #(error "attempt to use transient after it was persistet")
                                                   :__newindex #(error "attempt to use transient after it was persistet")})
                                    (immutable (itable t))))}
           (setmetatable {})))))

(fn hash-map* [x]
  "Add cljlib hash-map meta-info."
  (match (getmetatable x)
    mt (doto mt
         (tset :cljlib/type :hash-map)
         (tset :cljlib/editable true)
         (tset :cljlib/conj
               (fn [t [k v] ...]
                 (apply core.assoc
                        t k v
                        (accumulate [kvs [] _ [k v] (ipairs* [...])]
                          (doto kvs
                            (table.insert k)
                            (table.insert v))))))
         (tset :cljlib/transient (map->transient hash-map*))
         (tset :cljlib/empty #(hash-map* (itable {}))))
    _ (hash-map* (setmetatable x {})))
  x)

(defn assoc
  "Associate `val` under a `key`.
Accepts extra keys and values.

# Examples

``` fennel
(assert-eq {:a 1 :b 2} (assoc {:a 1} :b 2))
(assert-eq {:a 1 :b 2} (assoc {:a 1 :b 1} :b 2))
(assert-eq {:a 1 :b 2 :c 3} (assoc {:a 1 :b 1} :b 2 :c 3))
```"
  ([tbl]
   (hash-map* (itable {})))
  ([tbl k v]
   (assert (or (nil? tbl) (map? tbl) (empty? tbl)) "expected a map")
   (assert (not (nil? k)) "attempt to use nil as key")
   (hash-map* (itable.assoc (or tbl {}) k v)))
  ([tbl k v & kvs]
   (assert (or (nil? tbl) (map? tbl) (empty? tbl)) "expected a map")
   (assert (not (nil? k)) "attempt to use nil as key")
   (hash-map* (apply itable.assoc (or tbl {}) k v kvs))))

(defn assoc-in
  "Associate `val` into set of immutable nested tables `t`, via given `key-seq`.
Returns a new immutable table.  Returns a new immutable table.

# Examples

Replace value under nested keys:

``` fennel
(assert-eq
 {:a {:b {:c 1}}}
 (assoc-in {:a {:b {:c 0}}} [:a :b :c] 1))
```

Create new entries as you go:

``` fennel
(assert-eq
 {:a {:b {:c 1}} :e 2}
 (assoc-in {:e 2} [:a :b :c] 1))
```"
  [tbl key-seq val]
  (assert (or (nil? tbl) (map? tbl) (empty? tbl)) "expected a map or nil")
  (hash-map* (itable.assoc-in tbl key-seq val)))

(defn update
  "Update table value stored under `key` by calling a function `f` on
that value. `f` must take one argument, which will be a value stored
under the key in the table.

# Examples

Same as `assoc` but accepts function to produce new value based on key value.

``` fennel
(assert-eq
 {:data \"THIS SHOULD BE UPPERCASE\"}
 (update {:data \"this should be uppercase\"} :data string.upper))
```"
  [tbl key f]
  (assert (or (nil? tbl) (map? tbl) (empty? tbl)) "expected a map")
  (hash-map* (itable.update tbl key f)))


(defn update-in
  "Update table value stored under set of immutable nested tables, via
given `key-seq` by calling a function `f` on the value stored under the
last key.  `f` must take one argument, which will be a value stored
under the key in the table.  Returns a new immutable table.

# Examples

Same as `assoc-in` but accepts function to produce new value based on key value.

``` fennel
(fn capitalize-words [s]
  (pick-values 1
    (s:gsub \"(%a)([%w_`]*)\" #(.. ($1:upper) ($2:lower)))))

(assert-eq
 {:user {:name \"John Doe\"}}
 (update-in {:user {:name \"john doe\"}} [:user :name] capitalize-words))
```"
  [tbl key-seq f]
  (assert (or (nil? tbl) (map? tbl) (empty? tbl)) "expected a map or nil")
  (hash-map* (itable.update-in tbl key-seq f)))

(defn hash-map
  "Create associative table from `kvs` represented as sequence of keys
and values"
  [& kvs]
  (apply assoc {} kvs))

(defn get
  "Get value from the table by accessing it with a `key`.
Accepts additional `not-found` as a marker to return if value wasn't
found in the table."
  ([tbl key] (get tbl key nil))
  ([tbl key not-found]
   (assert (or (map? tbl) (empty? tbl)) "expected a map")
   (or (. tbl key) not-found)))

(defn get-in
  "Get value from nested set of tables by providing key sequence.
Accepts additional `not-found` as a marker to return if value wasn't
found in the table."
  ([tbl keys] (get-in tbl keys nil))
  ([tbl keys not-found]
   (assert (or (map? tbl) (empty? tbl)) "expected a map")
   (var (res t done) (values tbl tbl nil))
   (each [_ k (ipairs* keys) :until done]
     (match (. t k)
       v (set (res t) (values v v))
       _ (set (res done) (values not-found true))))
   res))

(defn dissoc
  "Remove `key` from table `tbl`.  Optionally takes more `keys`."
  ([tbl] tbl)
  ([tbl key]
   (assert (or (map? tbl) (empty? tbl)) "expected a map")
   (hash-map* (doto tbl (tset key nil))))
  ([tbl key & keys]
   (apply dissoc (dissoc tbl key) keys)))

(defn merge
  "Merge `maps` rght to left into a single hash-map."
  [& maps]
  (when (some identity maps)
    (->> maps
         (reduce (fn [a b] (collect [k v (pairs* b) :into a]
                             (values k v)))
                 {})
         itable
         hash-map*)))

(defn frequencies
  "Return a table of unique entries from table `t` associated to amount
of their appearances.

# Examples

Count each entry of a random letter:

``` fennel
(let [fruits [:banana :banana :apple :strawberry :apple :banana]]
  (assert-eq (frequencies fruits)
             {:banana 3
              :apple 2
              :strawberry 1}))
```"
  [t]
  (hash-map* (itable.frequencies t)))

(defn group-by
  "Group table items in an associative table under the keys that are
results of calling `f` on each element of sequential table `t`.
Elements that the function call resulted in `nil` returned in a
separate table.

# Examples

Group rows by their date:

``` fennel
(local rows
  [{:date \"2007-03-03\" :product \"pineapple\"}
   {:date \"2007-03-04\" :product \"pizza\"}
   {:date \"2007-03-04\" :product \"pineapple pizza\"}
   {:date \"2007-03-05\" :product \"bananas\"}])

(assert-eq (group-by #(. $ :date) rows)
           {\"2007-03-03\"
            [{:date \"2007-03-03\" :product \"pineapple\"}]
            \"2007-03-04\"
            [{:date \"2007-03-04\" :product \"pizza\"}
             {:date \"2007-03-04\" :product \"pineapple pizza\"}]
            \"2007-03-05\"
            [{:date \"2007-03-05\" :product \"bananas\"}]})
```"
  [f t]
  (hash-map* (pick-values 1 (itable.group-by f t))))

(defn zipmap
  "Return an associative table with the `keys` mapped to the
corresponding `vals`."
  [keys vals]
  (hash-map* (itable (lazy.zipmap keys vals))))

(defn replace
  "Given a map of replacement pairs and a vector/collection `coll`,
returns a vector/seq with any elements `=` a key in `smap` replaced
with the corresponding `val` in `smap`.  Returns a transducer when no
collection is provided."
  ([smap]
   (map #(if-let [e (find smap $)] (. e 2) $)))
  ([smap coll]
   (if (vector? coll)
       (->> coll
            (reduce (fn [res v]
                      (if-let [e (find smap v)]
                        (doto res (table.insert (. e 2)))
                        (doto res (table.insert v))))
                    [])
            itable
            vec*)
       (map #(if-let [e (find smap $)] (. e 2) $) coll))))

;;; Conj

(defn conj
  "Insert `x` as a last element of a table `tbl`.

If `tbl` is a sequential table or empty table, inserts `x` and
optional `xs` as final element in the table.

If `tbl` is an associative table, that satisfies `map?` test,
insert `[key value]` pair into the table.

Mutates `tbl`.

# Examples
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

See `hash-map` for creating empty associative tables."
  ([] (vector))
  ([s] s)
  ([s x]
   (match (getmetatable s)
     {:cljlib/conj f} (f s x)
     _ (if (vector? s) (vec* (itable.insert s x))
           (map? s) (apply assoc s x)
           (nil? s) (cons x s)
           (empty? s) (vector x)
           (error "expected collection, got" (type s)))))
  ([s x & xs]
   (apply conj (conj s x) xs)))

(defn disj
  "Returns a new set type, that does not contain the
specified `key` or `keys`."
  ([Set] Set)
  ([Set key]
   (match (getmetatable Set)
     {:cljlib/type :hash-set :cljlib/disj f} (f Set key)
     _ (error (.. "disj is not supported on " (class Set)) 2)))
  ([Set key & keys]
   (match (getmetatable Set)
     {:cljlib/type :hash-set :cljlib/disj f} (apply f Set key keys)
     _ (error (.. "disj is not supported on " (class Set)) 2))))

(defn pop
  "If `coll` is a list returns a new list without the first
item. If `coll` is a vector, returns a new vector without the last
item. If the collection is empty, raises an error. Not the same as
`next` or `butlast`."
  [coll]
  (match (getmetatable coll)
    {:cljlib/type :seq} (match (seq coll)
                          s (drop 1 s)
                          _ (error "can't pop empty list" 2))
    {:cljlib/pop f} (f coll)
    _ (error (.. "pop is not supported on " (class coll)) 2)))

;;; Transients

(defn transient
  "Returns a new, transient version of the collection."
  [coll]
  (match (getmetatable coll)
    {:cljlib/editable true :cljlib/transient f} (f coll)
    _ (error "expected editable collection" 2)))

(defn conj!
  "Adds `x` to the transient collection, and return `coll`."
  ([] (transient (vec* [])))
  ([coll] coll)
  ([coll x]
   (match (getmetatable coll)
     {:cljlib/type :transient :cljlib/conj! f} (f coll x)
     {:cljlib/type :transient} (error "unsupported transient operation" 2)
     _ (error "expected transient collection" 2))
   coll))

(defn assoc!
  "Remove `k`from transient map, and return `map`."
  [map k & ks]
  (match (getmetatable map)
    {:cljlib/type :transient :cljlib/dissoc! f} (apply f map k ks)
    {:cljlib/type :transient} (error "unsupported transient operation" 2)
    _ (error "expected transient collection" 2))
  map)

(defn dissoc!
  "Remove `k`from transient map, and return `map`."
  [map k & ks]
  (match (getmetatable map)
    {:cljlib/type :transient :cljlib/dissoc! f} (apply f map k ks)
    {:cljlib/type :transient} (error "unsupported transient operation" 2)
    _ (error "expected transient collection" 2))
  map)

(defn disj!
  "disj[oin]. Returns a transient set of the same type, that does not
contain `key`."
  ([Set] Set)
  ([Set key & ks]
   (match (getmetatable Set)
     {:cljlib/type :transient :cljlib/disj! f} (apply f Set key ks)
     {:cljlib/type :transient} (error "unsupported transient operation" 2)
     _ (error "expected transient collection" 2))))

(defn pop!
  "Removes the last item from a transient vector. If the collection is
empty, raises an error Returns coll"
  [coll]
  (match (getmetatable coll)
    {:cljlib/type :transient :cljlib/pop! f} (f coll)
    {:cljlib/type :transient} (error "unsupported transient operation" 2)
    _ (error "expected transient collection" 2)))

(defn persistent!
  "Returns a new, persistent version of the transient collection. The
transient collection cannot be used after this call, any such use will
raise an error."
  [coll]
  (match (getmetatable coll)
    {:cljlib/type :transient :cljlib/persistent! f} (f coll)
    _ (error "expected transient collection" 2)))

;;; Into

(defn into
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined. A transducer may be supplied.

# Examples

Thransofmr a hash-map into a sequence of key-value pairs:

```fennel
(assert-eq [[:a 1]] (into (vector) {:a 1}))
```

  Construct a hash-map from a sequence of key-value pairs:

```fennel
(assert-eq {:a 1 :b 2 :c 3}
           (into (hash-map) [[:a 1] [:b 2] [:c 3]]))
```"
  ([] (vector))
  ([to] to)
  ([to from]
   (match (getmetatable to)
     {:cljlib/editable true}
     (persistent! (reduce conj! (transient to) from))
     _ (reduce conj to from)))
  ([to xform from]
   (match (getmetatable to)
     {:cljlib/editable true}
     (persistent! (transduce xform conj! (transient to) from))
     _ (transduce xform conj to from))))

;;; Hash Set

(fn viewset [Set view inspector indent]
  (if (. inspector.seen Set)
      (.. "@set" (. inspector.seen Set) "{...}")
      (let [prefix (.. "@set"
                       (if (inspector.visible-cycle? Set)
                           (. inspector.seen Set) "")
                       "{")
            set-indent (length prefix)
            indent-str (string.rep " " set-indent)
            lines (icollect [v (pairs* Set)]
                    (.. indent-str
                        (view v inspector (+ indent set-indent) true)))]
        (tset lines 1 (.. prefix (string.gsub (or (. lines 1) "") "^%s+" "")))
        (tset lines (length lines) (.. (. lines (length lines)) "}"))
        lines)))

(fn hash-set->transient [immutable]
  (fn [hset]
    (let [removed (setmetatable {} {:__index deep-index})]
      (->> {:__index (fn [_ k]
                       (if (not (. removed k)) (. hset k)))
            :cljlib/type :transient
            :cljlib/conj #(error "can't `conj` onto transient set, use `conj!`")
            :cljlib/disj #(error "can't `disj` a transient set, use `disj!`")
            :cljlib/assoc #(error "can't `assoc` onto transient set, use `assoc!`")
            :cljlib/dissoc #(error "can't `dissoc` onto transient set, use `dissoc!`")
            :cljlib/conj! (fn [thset v]
                            (if (= nil v)
                                (tset removed v true)
                                (tset removed v nil))
                            (doto thset (tset v v)))
            :cljlib/assoc! #(error "can't `assoc!` onto transient set")
            :cljlib/assoc! #(error "can't `dissoc!` a transient set")
            :cljlib/disj! (fn [thset ...]
                            (for [i 1 (select "#" ...)]
                              (let [k (select i ...)]
                                (tset thset k nil)
                                (tset removed k true)))
                            thset)
            :cljlib/persistent! (fn [thset]
                                  (let [t (collect [k v (pairs thset)
                                                    :into (collect [k v (pairs hset)]
                                                            (values k v))]
                                            (values k v))]
                                    (each [k (pairs removed)]
                                      (tset t k nil))
                                    (each [_ k (ipairs (icollect [k (pairs* thset)] k))]
                                      (tset thset k nil))
                                    (setmetatable thset
                                                  {:__index #(error "attempt to use transient after it was persistet")
                                                   :__newindex #(error "attempt to use transient after it was persistet")})
                                    (immutable (itable t))))}
           (setmetatable {})))))

(fn hash-set* [x]
  (match (getmetatable x)
    mt (doto mt
         (tset :cljlib/type :hash-set)
         (tset :cljlib/conj
               (fn [s v ...]
                 (hash-set*
                  (itable.assoc
                   s v v
                   (unpack* (let [res []]
                              (each [ _ v (ipairs [...])]
                                (table.insert res v)
                                (table.insert res v))
                              res))))))
         (tset :cljlib/disj
               (fn [s k ...]
                 (let [to-remove
                       (collect [_ k (ipairs [...])
                                 :into (->> {:__index deep-index}
                                            (setmetatable {k true}))]
                         k true)]
                   (hash-set*
                    (itable.assoc {}
                                  (unpack*
                                   (let [res []]
                                     (each [_ v (pairs s)]
                                       (when (not (. to-remove v))
                                         (table.insert res v)
                                         (table.insert res v)))
                                     res)))))))
         (tset :cljlib/empty #(hash-set* (itable {})))
         (tset :cljlib/editable true)
         (tset :cljlib/transient (hash-set->transient hash-set*))
         (tset :cljlib/seq (fn [s] (map #(if (vector? $) (. $ 1) $) s)))
         (tset :__fennelview viewset)
         (tset :__fennelrest (fn [s i]
                               (var j 1)
                               (let [vals []]
                                 (each [v (pairs* s)]
                                   (if (>= j i)
                                       (table.insert vals v)
                                       (set j (+ j 1))))
                                 (core.hash-set (unpack* vals))))))
    _ (hash-set* (setmetatable x {})))
  x)

(defn hash-set
  "Create hash set.

Set is a collection of unique elements, which sore purpose is only to
tell you if something is in the set or not."
  [& xs]
  (let [Set (collect [_ val (pairs* xs)
                      :into (->> {:__newindex deep-newindex}
                                 (setmetatable {}))]
              (values val val))]
    (hash-set* (itable Set))))

;;; Multimethods

(defn multifn?
  "Test if `mf' is an instance of `multifn'.

`multifn' is a special kind of table, created with `defmulti' macros
from `macros.fnl'."
  [mf]
  (match (getmetatable mf)
    {:cljlib/type :multifn} true
    _ false))

(defn remove-method
  "Remove method from `multimethod' for given `dispatch-value'."
  [multimethod dispatch-value]
  (if (multifn? multimethod)
      (tset multimethod dispatch-value nil)
      (error (.. (tostring multimethod) " is not a multifn") 2))
  multimethod)

(defn remove-all-methods
  "Removes all of the methods of `multimethod'"
  [multimethod]
  (if (multifn? multimethod)
      (each [k _ (pairs multimethod)]
        (tset multimethod k nil))
      (error (.. (tostring multimethod) " is not a multifn") 2))
  multimethod)

(defn methods
  "Given a `multimethod', returns a map of dispatch values -> dispatch fns"
  [multimethod]
  (if (multifn? multimethod)
      (let [m {}]
        (each [k v (pairs multimethod)]
          (tset m k v))
        m)
      (error (.. (tostring multimethod) " is not a multifn") 2)))

(defn get-method
  "Given a `multimethod' and a `dispatch-value', returns the dispatch
`fn' that would apply to that value, or `nil' if none apply and no
default."
  [multimethod dispatch-value]
  (if (multifn? multimethod)
      (or (. multimethod dispatch-value)
          (. multimethod :default))
      (error (.. (tostring multimethod) " is not a multifn") 2)))

core

;; Local Variables:
;; eval: (add-to-list 'imenu-generic-expression `(nil "\\s(\\(?:defn-?\\|fn\\*\\)[[:space:]]+\\(\\(?:\\sw\\|\\s_\\|-\\|_\\)+\\)" 1))
;; End:
