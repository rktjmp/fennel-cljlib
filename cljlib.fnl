(local core {:_DESCRIPTION "Fennel-cljlib - functions from Clojure's core.clj implemented on top of Fennel."
             :_VERSION "0.1.0"
             :_COPYRIGHT "Copyright (C) 2020 Andrey Orst"})

(local insert table.insert)
(local unpack (or table.unpack _G.unpack))

(require-macros :cljlib-macros)

(fn* core.vector
  "Constructs sequential table out of it's arguments.

Sets additional metadata for function [`vector?`](#vector?) to work.

# Examples

``` fennel
(local v (vector 1 2 3 4))
(assert (eq v [1 2 3 4]))
```"
  [& args]
  (setmetatable args {:cljlib/table-type :seq}))

(fn* core.apply
  "Apply `f` to the argument list formed by prepending intervening
arguments to `args`, adn `f` must support variadic amount of
arguments.

# Examples
Applying `print` to different arguments:

``` fennel
(apply print [1 2 3 4])
;; prints 1 2 3 4
(apply print 1 [2 3 4])
;; => 1 2 3 4
(apply print 1 2 3 4 5 6 [7 8 9])
;; => 1 2 3 4 5 6 7 8 9
```"
  ([f args] (f (unpack args)))
  ([f a args] (f a (unpack args)))
  ([f a b args] (f a b (unpack args)))
  ([f a b c args] (f a b c (unpack args)))
  ([f a b c d & args]
   (let [flat-args (empty [])]
     (for [i 1 (- (length args) 1)]
       (insert flat-args (. args i)))
     (each [_ a (ipairs (. args (length args)))]
       (insert flat-args a))
     (f a b c d (unpack flat-args)))))

(fn fast-table-type [tbl]
  (let [m (getmetatable tbl)]
    (if-let [t (and m (. m :cljlib/table-type))]
      t)))

;; predicate functions
(fn* core.map?
  "Check whether `tbl` is an associative table.

Non empty associative tables are tested for two things:
- `next` returns the key-value pair,
- key, that is returned by the `next` is not equal to `1`.

Empty tables can't be analyzed with this method, and `map?` will
return `false`.  If you need this test pass for empty table, see
[`hash-map`](#hash-map) for creating tables that have additional
metadata attached for this test to work.

# Examples
Non empty tables:

``` fennel
(assert (map? {:a 1 :b 2}))

(local some-table {:key :value})
(assert (map? some-table))
```

Empty tables:

``` fennel
(local some-table {})
(assert (not (map? some-table)))
```

Empty tables created with [`hash-map`](#hash-map) will pass the test:

``` fennel
(local some-table (hash-map))
(assert (map? some-table))
```"
  [tbl]
  (if (= (type tbl) :table)
      (if-let [t (fast-table-type tbl)]
        (= t :table)
        (let [(k _) (next tbl)]
          (and (not= k nil)
               (not= k 1))))))

(fn* core.vector?
  "Check whether `tbl` is an sequential table.

Non empty sequential tables are tested for two things:
- `next` returns the key-value pair,
- key, that is returned by the `next` is equal to `1`.

Empty tables can't be analyzed with this method, and `vector?` will
always return `false`.  If you need this test pass for empty table,
see [`vector`](#vector) for creating tables that have additional
metadata attached for this test to work.

# Examples
Non empty vector:

``` fennel
(assert (vector? [1 2 3 4]))

(local some-table [1 2 3])
(assert (vector? some-table))
```

Empty tables:

``` fennel
(local some-table [])
(assert (not (vector? some-table)))
```

Empty tables created with [`vector`](#vector) will pass the test:

``` fennel
(local some-table (hash-map))
(assert (vector? some-table))
```"
  [tbl]
  (if (= (type tbl) :table)
      (if-let [t (fast-table-type tbl)]
        (= t :seq)
        (let [(k _) (next tbl)]
          (and (not= k nil) (= k 1))))))


(fn* core.nil?
  "Test if value is nil."
  ([] true)
  ([x] (= x nil)))

(fn* core.zero?
  "Test if value is equal to zero."
  [x]
  (= x 0))

(fn* core.pos?
  "Test if `x` is greater than zero."
  [x]
  (> x 0))

(fn* core.neg?
  "Test if `x` is less than zero."
  [x]
  (< x 0))

(fn* core.even?
  "Test if value is even."
  [x]
  (= (% x 2) 0))

(fn* core.odd?
  "Test if value is odd."
  [x]
  (not (even? x)))

(fn* core.string?
  "Test if `x` is a string."
  [x]
  (= (type x) :string))

(fn* core.boolean?
  "Test if `x` is a Boolean"
  [x]
  (= (type x) :boolean))

(fn* core.true?
  "Test if `x` is `true`"
  [x]
  (= x true))

(fn* core.false?
  "Test if `x` is `false`"
  [x]
  (= x false))

(fn* core.int?
  "Test if `x` is a number without floating point data.

Number is rounded with `math.floor` and compared with original number."
  [x]
  (and (= (type x) :number)
       (= x (math.floor x))))

(fn* core.pos-int?
  "Test if `x` is a positive integer."
  [x]
  (and (int? x)
       (pos? x)))

(fn* core.neg-int?
  "Test if `x` is a negetive integer."
  [x]
  (and (int? x)
       (neg? x)))

(fn* core.double?
  "Test if `x` is a number with floating point data."
  [x]
  (and (= (type x) :number)
       (not= x (math.floor x))))

(fn* core.empty?
  "Check if collection is empty."
  [x]
  (match (type x)
    :table (= (next x) nil)
    :string (= x "")
    _ (error "empty?: unsupported collection")))

(fn* core.not-empty
  "If `x` is empty, returns `nil`, otherwise `x`."
  [x]
  (if (not (empty? x))
      x))

;; sequence manipulating functions

(fn* core.seq
  "Create sequential table.

Transforms original table to sequential table of key value pairs
stored as sequential tables in linear time.  If `col` is an
associative table, returns sequential table of vectors with key and
value.  If `col` is sequential table, returns its shallow copy.

# Examples
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
```"
  [col]
  (let [res (empty [])]
    (match (type col)
      :table (when-some [_ (next col)]
               (var assoc? false)
               (let [assoc-res (empty [])]
                 (each [k v (pairs col)]
                   (if (and (not assoc?)
                            (not (= (type k) :number)))
                       (set assoc? true))
                   (insert res v)
                   (insert assoc-res [k v]))
                 (if assoc? assoc-res res)))
      :string (let [char utf8.char]
                (each [_ b (utf8.codes col)]
                  (insert res (char b)))
                res)
      :nil nil
      _ (error (.. "expected table, string or nil") 2))))

(fn* core.first
  "Return first element of a table. Calls `seq` on its argument."
  [col]
  (when-some [col (seq col)]
    (. col 1)))

(fn* core.rest
  "Returns table of all elements of a table but the first one. Calls
  `seq` on its argument."
  [col]
  (if-some [col (seq col)]
    (vector (unpack col 2))
    (empty [])))

(fn* core.last
  "Returns the last element of a table. Calls `seq` on its argument."
  [col]
  (when-some [col (seq col)]
    (var (i v) (next col))
    (while i
      (local (_i _v) (next col i))
      (if _i (set v _v))
      (set i _i))
    v))

(fn* core.butlast
  "Returns everything but the last element of a table as a new
  table. Calls `seq` on its argument."
  [col]
  (when-some [col (seq col)]
    (table.remove col (length col))
    (when (not (empty? col))
      col)))

(fn* core.conj
  "Insert `x` as a last element of a table `tbl`.

If `tbl` is a sequential table or empty table, inserts `x` and
optional `xs` as final element in the table.

If `tbl` is an associative table, that satisfies [`map?`](#map?) test,
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

See [`hash-map`](#hash-map) for creating empty associative tables."
  ([] (empty []))
  ([tbl] tbl)
  ([tbl x]
   (when-some [x x]
     (let [tbl (or tbl (empty []))]
       (if (map? tbl)
           (tset tbl (. x 1) (. x 2))
           (insert tbl x))))
   tbl)
  ([tbl x & xs]
   (apply conj (conj tbl x) xs)))

(fn consj [...]
  "Like conj but joins at the front. Modifies `tbl`."
  (let [[tbl x & xs] [...]]
    (if (nil? x) tbl
        (consj (doto tbl (insert 1 x)) (unpack xs)))))

(fn* core.cons
  "Insert `x` to `tbl` at the front. Modifies `tbl`."
  [x tbl]
  (if-some [x x]
    (doto (or (seq tbl) (empty []))
      (insert 1 x))
    tbl))

(fn* core.concat
  "Concatenate tables."
  ([] nil)
  ([x] (or (seq x) (empty [])))
  ([x y] (let [to (or (seq x) (empty []))
               from (or (seq y) (empty []))]
           (each [_ v (ipairs from)]
             (insert to v))
           to))
  ([x y & xs]
   (apply concat (concat x y) xs)))

(fn* core.reduce
  "Reduce collection `col` using function `f` and optional initial value `val`.

`f` should be a function of 2 arguments.  If val is not supplied,
returns the result of applying f to the first 2 items in coll, then
applying f to that result and the 3rd item, etc.  If coll contains no
items, f must accept no arguments as well, and reduce returns the
result of calling f with no arguments.  If coll has only 1 item, it is
returned and f is not called.  If val is supplied, returns the result
of applying f to val and the first item in coll, then applying f to
that result and the 2nd item, etc.  If coll contains no items, returns
val and f is not called.  Calls `seq` on `col`.

Early termination is possible with the use of [`reduced`](#reduced)
function.

# Examples
Reduce sequence of numbers with [`add`](#add)

``` fennel
(reduce add [1 2 3 4])
;; => 10
(reduce add 10 [1 2 3 4])
;; => 20
```

"
  ([f col]
   (let [col (or (seq col) (empty []))]
     (match (length col)
       0 (f)
       1 (. col 1)
       2 (f (. col 1) (. col 2))
       _ (let [[a b & rest] col]
           (reduce f (f a b) rest)))))
  ([f val col]
   (if-some [reduced (when-some [m (getmetatable val)]
                       (and m.cljlib/reduced
                            (= m.cljlib/reduced.status :ready)
                            m.cljlib/reduced.val))]
     reduced
     (let [col (or (seq col) (empty []))]
       (let [[x & xs] col]
         (if (nil? x)
             val
             (reduce f (f val x) xs)))))))

(fn* core.reduced
  "Wraps `x` in such a way so [`reduce`](#reduce) will terminate early
with this value.

# Examples
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
valid number, but we've terminated right before we've reached it."
  [x]
  (setmetatable
   {} {:cljlib/reduced {:status :ready
                        :val x}}))

(fn* core.reduce-kv
  "Reduces an associative table using function `f` and initial value `val`.

`f` should be a function of 3 arguments.  Returns the result of
applying `f` to `val`, the first key and the first value in `tbl`,
then applying `f` to that result and the 2nd key and value, etc.  If
`tbl` contains no entries, returns `val` and `f` is not called.  Note
that reduce-kv is supported on sequential tables and strings, where
the keys will be the ordinals.

Early termination is possible with the use of [`reduced`](#reduced)
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
  [f val tbl]
  (var res val)
  (each [_ [k v] (pairs (or (seq tbl) (empty [])))]
    (set res (f res k v))
    (when-some [reduced (when-some [m (getmetatable res)]
                          (and m.cljlib/reduced
                               (= m.cljlib/reduced.status :ready)
                               m.cljlib/reduced.val))]
      (set res reduced)
      (lua :break)))
  res)

(fn* core.mapv
  "Maps function `f` over one or more collections.

Accepts arbitrary amount of collections, calls `seq` on each of it.
Function `f` must take the same amount of arguments as the amount of
tables, passed to `mapv`. Applies `f` over first value of each
table. Then applies `f` to second value of each table. Continues until
any of the tables is exhausted. All remaining values are
ignored. Returns a sequential table of results.

# Examples
Map `string.upcase` over the string:

``` fennel
(mapv string.upper \"string\")
;; => [\"S\" \"T\" \"R\" \"I\" \"N\" \"G\"]
```

Map [`mul`](#mul) over two tables:

``` fennel
(mapv mul [1 2 3 4] [1 0 -1])
;; => [1 0 -3]
```

Basic `zipmap` implementation:

``` fennel
(fn zipmap [keys vals]
  (into {} (mapv vector keys vals)))

(zipmap [:a :b :c] [1 2 3 4])
;; => {:a 1 :b 2 :c 3}
```"
  ([f col]
   (local res (empty []))
   (each [_ v (ipairs (or (seq col) (empty [])))]
     (when-some [tmp (f v)]
       (insert res tmp)))
   res)
  ([f col1 col2]
   (let [res (empty [])
         col1 (or (seq col1) (empty []))
         col2 (or (seq col2) (empty []))]
     (var (i1 v1) (next col1))
     (var (i2 v2) (next col2))
     (while (and i1 i2)
       (when-some [tmp (f v1 v2)]
         (insert res tmp))
       (set (i1 v1) (next col1 i1))
       (set (i2 v2) (next col2 i2)))
     res))
  ([f col1 col2 col3]
   (let [res (empty [])
         col1 (or (seq col1) (empty []))
         col2 (or (seq col2) (empty []))
         col3 (or (seq col3) (empty []))]
     (var (i1 v1) (next col1))
     (var (i2 v2) (next col2))
     (var (i3 v3) (next col3))
     (while (and i1 i2 i3)
       (when-some [tmp (f v1 v2 v3)]
         (insert res tmp))
       (set (i1 v1) (next col1 i1))
       (set (i2 v2) (next col2 i2))
       (set (i3 v3) (next col3 i3)))
     res))
  ([f col1 col2 col3 & cols]
   (let [step (fn step [cols]
                (if (->> cols
                         (mapv #(not= (next $) nil))
                         (reduce #(and $1 $2)))
                    (cons (mapv #(. (or (seq $) (empty [])) 1) cols) (step (mapv #(do [(unpack $ 2)]) cols)))
                    (empty [])))
         res (empty [])]
     (each [_ v (ipairs (step (consj cols col3 col2 col1)))]
       (when-some [tmp (apply f v)]
         (insert res tmp)))
     res)))

(fn* core.filter
  "Returns a sequential table of the items in `col` for which `pred`
  returns logical true."
  [pred col]
  (if-let [col (seq col)]
    (let [f (. col 1)
          r [(unpack col 2)]]
      (if (pred f)
          (cons f (filter pred r))
          (filter pred r)))
    (empty [])))

(fn* core.kvseq
  "Transforms any table kind to key-value sequence."
  [tbl]
  (let [res (empty [])]
    (each [k v (pairs tbl)]
      (insert res [k v]))
    res))

(fn* core.identity "Returns its argument." [x] x)

(fn* core.comp
  "Compose functions."
  ([] identity)
  ([f] f)
  ([f g]
   (fn*
     ([] (f (g)))
     ([x] (f (g x)))
     ([x y] (f (g x y)))
     ([x y z] (f (g x y z)))
     ([x y z & args] (f (g x y z (unpack args))))))
  ([f g & fs]
   (reduce comp (consj fs g f))))

(fn* core.every?
  "Test if every item in `tbl` satisfies the `pred`."
  [pred tbl]
  (if (empty? tbl) true
      (pred (. tbl 1)) (every? pred [(unpack tbl 2)])
      false))

(fn* core.some
  "Test if any item in `tbl` satisfies the `pred`."
  [pred tbl]
  (when-let [tbl (seq tbl)]
    (or (pred (. tbl 1)) (some pred [(unpack tbl 2)]))))

(set core.not-any?
     (with-meta (comp #(not $) some)
                {:fnl/docstring "Test if no item in `tbl` satisfy the `pred`."
                 :fnl/arglist ["pred" "tbl"]}))

(fn* core.complement
  "Takes a function `f` and returns the function that takes the same
amount of arguments as `f`, has the same effect, and returns the
oppisite truth value."
  [f]
  (fn*
    ([] (not (f)))
    ([a] (not (f a)))
    ([a b] (not (f a b)))
    ([a b & cs] (not (apply f a b cs)))))

(fn* core.constantly
  "Returns a function that takes any number of arguments and returns `x`."
  [x]
  (fn [...] x))

(fn* core.range
  "return range of of numbers from `lower` to `upper` with optional `step`."
  ([upper] (range 0 upper 1))
  ([lower upper] (range lower upper 1))
  ([lower upper step]
   (let [res (empty [])]
     (for [i lower (- upper step) step]
       (insert res i))
     res)))

(fn* core.reverse
  "Returns table with same items as in `tbl` but in reverse order."
  [tbl]
  (when-some [tbl (seq tbl)]
    (reduce consj (empty []) tbl)))

(fn* core.inc "Increase number by one" [x] (+ x 1))
(fn* core.dec "Decrease number by one" [x] (- x 1))

(fn* core.assoc
  "Associate key `k` with value `v` in `tbl`."
  ([tbl k v]
   (setmetatable
    (doto tbl (tset k v))
    {:cljlib/table-type :table}))
  ([tbl k v & kvs]
   (assert (= (% (length kvs) 2) 0)
           (.. "no value supplied for key " (. kvs (length kvs))))
   (tset tbl k v)
   (var [k v] [nil nil])
   (var (i k) (next kvs))
   (while i
     (set (i v) (next kvs i))
     (tset tbl k v)
     (set (i k) (next kvs i)))
   (setmetatable tbl {:cljlib/table-type :table})))

(fn* core.hash-map
  "Create associative table from keys and values"
  ([] (empty {}))
  ([& kvs] (apply assoc {} kvs)))

(fn* core.get
  "Get value from the table by accessing it with a `key`.
Accepts additional `not-found` as a marker to return if value wasn't
found in the table."
  ([tbl key] (get tbl key nil))
  ([tbl key not-found]
   (if-some [res (. tbl key)]
     res
     not-found)))

(fn* core.get-in
  "Get value from nested set of tables by providing key sequence.
Accepts additional `not-found` as a marker to return if value wasn't
found in the table."
  ([tbl keys] (get-in tbl keys nil))
  ([tbl keys not-found]
   (var res tbl)
   (var t tbl)
   (each [_ k (ipairs keys)]
     (if-some [v (. t k)]
       (set [res t] [v v])
       (set res not-found)))
   res))

(fn* core.remove-method
  "Remove method from `multifn` for given `dispatch-val`."
  [multifn dispatch-val]
  (tset (. (getmetatable multifn) :multimethods) dispatch-val nil)
  multifn)

(fn* core.remove-all-methods
  "Removes all of the methods of multimethod"
  [multifn]
  (let [mtable (. (getmetatable multifn) :multimethods)]
    (each [k _ (pairs mtable)]
      (tset mtable k nil))
    multifn))

(fn* core.methods
  "Given a multimethod, returns a map of dispatch values -> dispatch fns"
  [multifn]
  (. (getmetatable multifn) :multimethods))

(fn* core.get-method
  "Given a multimethod and a dispatch value, returns the dispatch `fn`
that would apply to that value, or `nil` if none apply and no default."
  [multifn dispatch-val]
  (or (. (getmetatable multifn) :multimethods dispatch-val)
      (. (getmetatable multifn) :multimethods :default)))

(fn* core.add
  "Sum arbitrary amount of numbers."
  ([] 0)
  ([a] a)
  ([a b] (+ a b))
  ([a b c] (+ a b c))
  ([a b c d] (+ a b c d))
  ([a b c d & rest] (apply add (+ a b c d) rest)))

(fn* core.sub
  "Subtract arbitrary amount of numbers."
  ([] 0)
  ([a] (- a))
  ([a b] (- a b))
  ([a b c] (- a b c))
  ([a b c d] (- a b c d))
  ([a b c d & rest] (apply sub (- a b c d) rest)))

(fn* core.mul
  "Multiply arbitrary amount of numbers."
  ([] 1)
  ([a] a)
  ([a b] (* a b))
  ([a b c] (* a b c))
  ([a b c d] (* a b c d))
  ([a b c d & rest] (apply mul (* a b c d) rest)))

(fn* core.div
  "Divide arbitrary amount of numbers."
  ([a] (/ 1 a))
  ([a b] (/ a b))
  ([a b c] (/ a b c))
  ([a b c d] (/ a b c d))
  ([a b c d & rest] (apply div (/ a b c d) rest)))

(fn* core.le
  "Returns true if nums are in monotonically non-decreasing order"
  ([x] true)
  ([x y] (<= x y))
  ([x y & more]
   (if (<= x y)
       (if (next more 1)
           (le y (. more 1) (unpack more 2))
           (<= y (. more 1)))
       false)))

(fn* core.lt
  "Returns true if nums are in monotonically decreasing order"
  ([x] true)
  ([x y] (< x y))
  ([x y & more]
   (if (< x y)
       (if (next more 1)
           (lt y (. more 1) (unpack more 2))
           (< y (. more 1)))
       false)))

(fn* core.ge
  "Returns true if nums are in monotonically non-increasing order"
  ([x] true)
  ([x y] (>= x y))
  ([x y & more]
   (if (>= x y)
       (if (next more 1)
           (ge y (. more 1) (unpack more 2))
           (>= y (. more 1)))
       false)))

(fn* core.gt
  "Returns true if nums are in monotonically increasing order"
  ([x] true)
  ([x y] (> x y))
  ([x y & more]
   (if (> x y)
       (if (next more 1)
           (gt y (. more 1) (unpack more 2))
           (> y (. more 1)))
       false)))

(fn* core.eq
  "Deep compare values."
  ([x] true)
  ([x y]
   (if (and (= (type x) :table) (= (type y) :table))
       (let [oldmeta (getmetatable y)]
         ;; In case if we'll get something like
         ;; (eq {[1 2 3] {:a [1 2 3]}} {[1 2 3] {:a [1 2 3]}})
         ;; we have to do even deeper search
         (setmetatable y {:__index (fn [tbl key]
                                     (var res nil)
                                     (each [k v (pairs tbl)]
                                       (when (eq k key)
                                         (set res v)
                                         (lua :break)))
                                     res)})
         (var [res count-a count-b] [true 0 0])
         (each [k v (pairs x)]
           (set res (eq v (. y k)))
           (set count-a (+ count-a 1))
           (when (not res) (lua :break)))
         (when res
           (each [_ _ (pairs y)]
             (set count-b (+ count-b 1)))
           (set res (= count-a count-b)))
         (setmetatable y oldmeta)
         res)
       (= x y)))
  ([x y & xs]
   (reduce #(and $1 $2) (eq x y) (mapv #(eq x $) xs))))

(fn* core.memoize
  "Returns a memoized version of a referentially transparent function.
The memoized version of the function keeps a cache of the mapping from
arguments to results and, when calls with the same arguments are
repeated often, has higher performance at the expense of higher memory
use."
  [f]
  (let [memo (setmetatable {} {:__index
                               (fn [tbl key]
                                 (each [k v (pairs tbl)]
                                   (when (eq k key)
                                     (lua "return v"))))})]
    (fn [...]
      (let [args [...]]
        (if-some [res (. memo args)]
          res
          (let [res (f ...)]
            (tset memo args res)
            res))))))

core
