(local core {})

(local insert table.insert)
(local unpack (or table.unpack _G.unpack))
(require-macros :cljlib-macros)

(fn* core.vector
  "Constructs sequential table out of it's arguments."
  [& args]
  (setmetatable args {:cljlib/table-type :seq}))

(fn* core.apply
  "Apply `f' to the argument list formed by prepending intervening
arguments to `args'."
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
(fn& core.map?
  "Check whether `tbl' is an associative table."
  [tbl]
  (if (= (type tbl) :table)
      (if-let [t (fast-table-type tbl)]
        (= t :table)
        (let [(k _) (next tbl)]
          (and (not= k nil)
               (or (not= (type k) :number)
                   (not= k 1)))))))

(fn& core.seq?
  "Check whether `tbl' is an sequential table."
  [tbl]
  (if (= (type tbl) :table)
      (if-let [t (fast-table-type tbl)]
        (= t :seq)
        (let [(k _) (next tbl)]
          (and (not= k nil) (= (type k) :number) (= k 1))))))


(fn& core.nil?
  "Test if value is nil."
  [x]
  (= x nil))

(fn& core.zero?
  "Test if value is zero."
  [x]
  (= x 0))

(fn& core.pos?
  "Test if `x' is greater than zero."
  [x]
  (> x 0))

(fn& core.neg?
  "Test if `x' is less than zero."
  [x]
  (< x 0))

(fn& core.even?
  "Test if value is even."
  [x]
  (= (% x 2) 0))

(fn& core.odd?
  "Test if value is odd."
  [x]
  (not (even? x)))

(fn& core.string?
  "Test if `x' is a string."
  [x]
  (= (type x) :string))

(fn& core.boolean?
  "Test if `x' is a Boolean"
  [x]
  (= (type x) :boolean))

(fn& core.true?
  "Test if `x' is `true'"
  [x]
  (= x true))

(fn& core.false?
  "Test if `x' is `false'"
  [x]
  (= x false))

(fn& core.int?
  "Test if `x' is a number without floating point data."
  [x]
  (and (= (type x) :number)
       (= x (math.floor x))))

(fn& core.pos-int?
  "Test if `x' is a positive integer."
  [x]
  (and (int? x)
       (pos? x)))

(fn& core.neg-int?
  "Test if `x' is a negetive integer."
  [x]
  (and (int? x)
       (neg? x)))

(fn& core.double?
  "Test if `x' is a number with floating point data."
  [x]
  (and (= (type x) :number)
       (not= x (math.floor x))))

(fn& core.empty?
  "Check if collection is empty."
  [x]
  (match (type x)
    :table (= (next x) nil)
    :string (= x "")
    _ (error "empty?: unsupported collection")))

(fn& core.not-empty
  "If `x' is empty, returns `nil', otherwise `x'."
  [x]
  (if (not (empty? x))
      x))

;; sequence manipulating functions

(fn& core.seq
  "Create sequential table.
Transforms original table to sequential table of key value pairs
stored as sequential tables in linear time.  If `tbl' is an
associative table, returns `[[key1 value1] ... [keyN valueN]]' table.
If `tbl' is sequential table, returns its shallow copy."
  [col]
  (match (type col)
    :table
    (when-some [_ (and col (next col))]
      (var assoc? false)
      (let [assoc (empty [])
            seq (empty [])]
        (each [k v (pairs col)]
          (if (and (not assoc?)
                   (not (= (type k) :number)))
              (set assoc? true))
          (insert assoc [k v])
          (tset seq k v))
        (if assoc? assoc seq)))
    :string
    (let [res []
          char utf8.char]
      (each [_ b (utf8.codes col)]
        (insert res (char b)))
      res)
    :nil nil
    _ (error "expected table or string" 2)))

(macro safe-seq [tbl]
  "Create sequential table, or empty table if `seq' returned `nil'."
  `(or (seq ,tbl) (empty [])))

(fn& core.first
  "Return first element of a table. Calls `seq' on its argument."
  [tbl]
  (when-some [tbl (seq tbl)]
    (. tbl 1)))

(fn& core.rest
  "Returns table of all elements of a table but the first one. Calls
  `seq' on its argument."
  [tbl]
  (if-some [tbl (seq tbl)]
    (vector (unpack tbl 2))
    (empty [])))

(fn& core.last
  "Returns the last element of a table. Calls `seq' on its argument."
  [tbl]
  (when-some [tbl (seq tbl)]
    (var (i v) (next tbl))
    (while i
      (local (_i _v) (next tbl i))
      (if _i (set v _v))
      (set i _i))
    v))

(fn& core.butlast
  "Returns everything but the last element of a table as a new
  table. Calls `seq' on its argument."
  [tbl]
  (when-some [tbl (seq tbl)]
    (table.remove tbl (length tbl))
    (when (not (empty? tbl))
      tbl)))


(fn* core.conj
  "Insert `x' as a last element of indexed table `tbl'. Modifies `tbl'"
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
  "Like conj but joins at the front. Modifies `tbl'."
  (let [[tbl x & xs] [...]]
    (if (nil? x) tbl
        (consj (doto tbl (insert 1 x)) (unpack xs)))))

(fn& core.cons
  "Insert `x' to `tbl' at the front. Modifies `tbl'."
  [x tbl]
  (if-some [x x]
    (doto (safe-seq tbl)
      (insert 1 x))
    tbl))

(fn* core.concat
  "Concatenate tables."
  ([] nil)
  ([x] (safe-seq x))
  ([x y] (let [to (safe-seq x)
               from (safe-seq y)]
           (each [_ v (ipairs from)]
             (insert to v))
           to))
  ([x y & xs]
   (apply concat (concat x y) xs)))

(fn* core.reduce
  "Reduce indexed table using function `f' and optional initial value `val'.

([f table])
([f val table])

`f' should be a function of 2 arguments.  If val is not supplied,
returns the result of applying f to the first 2 items in coll, then
applying f to that result and the 3rd item, etc.  If coll contains no
items, f must accept no arguments as well, and reduce returns the
result of calling f with no arguments.  If coll has only 1 item, it is
returned and f is not called.  If val is supplied, returns the result
of applying f to val and the first item in coll, then applying f to
that result and the 2nd item, etc.  If coll contains no items, returns
val and f is not called."
  ([f tbl]
   (let [tbl (safe-seq tbl)]
     (match (length tbl)
       0 (f)
       1 (. tbl 1)
       2 (f (. tbl 1) (. tbl 2))
       _ (let [[a b & rest] tbl]
           (reduce f (f a b) rest)))))
  ([f val tbl]
   (let [tbl (safe-seq tbl)]
     (let [[x & xs] tbl]
       (if (nil? x)
           val
           (reduce f (f val x) xs))))))

(fn* core.reduce-kv
  "Reduces an associative table using function `f' and initial value `val'.

([f val table])

`f' should be a function of 3 arguments.  Returns the result of
applying `f' to `val', the first key and the first value in coll, then
applying `f' to that result and the 2nd key and value, etc.  If coll
contains no entries, returns `val' and `f' is not called.  Note that
reduce-kv is supported on vectors, where the keys will be the
ordinals."  [f val tbl]
  (var res val)
  (each [_ [k v] (pairs (safe-seq tbl))]
    (set res (f res k v)))
  res)

(fn* core.mapv
  "Maps function `f' over one or more tables.

Accepts arbitrary amount of tables, calls `seq' on each of it.
Function `f' must take the same amount of parameters as the amount of
tables passed to `mapv'. Applies `f' over first value of each
table. Then applies `f' to second value of each table. Continues until
any of the tables is exhausted. All remaining values are
ignored. Returns a table of results."
  ([f tbl]
   (local res (empty []))
   (each [_ v (ipairs (safe-seq tbl))]
     (when-some [tmp (f v)]
       (insert res tmp)))
   res)
  ([f t1 t2]
   (let [res (empty [])
         t1 (safe-seq t1)
         t2 (safe-seq t2)]
     (var (i1 v1) (next t1))
     (var (i2 v2) (next t2))
     (while (and i1 i2)
       (when-some [tmp (f v1 v2)]
         (insert res tmp))
       (set (i1 v1) (next t1 i1))
       (set (i2 v2) (next t2 i2)))
     res))
  ([f t1 t2 t3]
   (let [res (empty [])
         t1 (safe-seq t1)
         t2 (safe-seq t2)
         t3 (safe-seq t3)]
     (var (i1 v1) (next t1))
     (var (i2 v2) (next t2))
     (var (i3 v3) (next t3))
     (while (and i1 i2 i3)
       (when-some [tmp (f v1 v2 v3)]
         (insert res tmp))
       (set (i1 v1) (next t1 i1))
       (set (i2 v2) (next t2 i2))
       (set (i3 v3) (next t3 i3)))
     res))
  ([f t1 t2 t3 & tbls]
   (let [step (fn step [tbls]
                (if (->> tbls
                         (mapv #(not= (next $) nil))
                         (reduce #(and $1 $2)))
                    (cons (mapv #(. (safe-seq $) 1) tbls) (step (mapv #(do [(unpack $ 2)]) tbls)))
                    (empty [])))
         res (empty [])]
     (each [_ v (ipairs (step (consj tbls t3 t2 t1)))]
       (when-some [tmp (apply f v)]
         (insert res tmp)))
     res)))

(fn* core.filter [pred tbl]
  (when-let [tbl (seq tbl)]
    (let [f (. tbl 1) r [(unpack tbl 2)]]
      (if (pred f)
          (cons f (filter pred r))
          (filter pred r)))))

(fn kvseq [tbl]
  "Transforms any table kind to key-value sequence."
  (let [res (empty [])]
    (each [k v (pairs tbl)]
      (insert res [k v]))
    res))



(fn& core.identity
  "Returns its argument."
  [x]
  x)

(fn* core.comp
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
  "Test if every item in `tbl' satisfies the `pred'."
  [pred tbl]
  (if (empty? tbl) true
      (pred (. tbl 1)) (every? pred [(unpack tbl 2)])
      false))

(fn* core.some
  "Test if any item in `tbl' satisfies the `pred'."
  [pred tbl]
  (when-let [tbl (seq tbl)]
    (or (pred (. tbl 1)) (some pred [(unpack tbl 2)]))))

(set core.not-any?
     (with-meta (comp #(not $) some)
                {:fnl/docstring "Test if no item in `tbl' satisfy the `pred'."
                 :fnl/arglist ["pred" "tbl"]}))

(fn& core.complement
  "Takes a function `f' and returns the function that takes the same
amount of arguments as `f', has the same effect, and returns the
oppisite truth value."
  [f]
  (fn*
    ([] (not (f)))
    ([a] (not (f a)))
    ([a b] (not (f a b)))
    ([a b & cs] (not (apply f a b cs)))))

(fn& core.constantly
  "Returns a function that takes any number of arguments and returns `x'."
  [x]
  (fn [...] x))

(fn* core.range
  "return range of of numbers from `lower' to `upper' with optional `step'."
  ([upper] (range 0 upper 1))
  ([lower upper] (range lower upper 1))
  ([lower upper step]
   (let [res (empty [])]
     (for [i lower (- upper step) step]
       (insert res i))
     res)))

(fn& core.reverse
  "Returns table with same items as in `tbl' but in reverse order."
  [tbl]
  (when-some [tbl (seq tbl)]
    (reduce consj (empty []) tbl)))

(fn* core.inc "Increase number by one" [x] (+ x 1))
(fn* core.dec "Decrease number by one" [x] (- x 1))

(fn* core.assoc
  "Associate key `k' with value `v' in `tbl'."
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

(fn& core.hash-map
  "Create associative table from keys and values"
  [...]
  (if (> (select :# ...) 0)
      (assoc {} ...)
      (setmetatable {} {:cljlib/table-type :table})))

(fn* core.get
  "Get value from the table by accessing it with a `key'.
Accepts additional `not-found' as a marker to return if value wasn't
found in the table."
  ([tbl key] (get tbl key nil))
  ([tbl key not-found]
   (if-some [res (. tbl key)]
     res
     not-found)))

(fn* core.get-in
  "Get value from nested set of tables by providing key sequence.
Accepts additional `not-found' as a marker to return if value wasn't
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
  "Given a multimethod and a dispatch value, returns the dispatch `fn'
that would apply to that value, or `nil' if none apply and no default."
  [multifn dispatch-val]
  (or (. (getmetatable multifn) :multimethods dispatch-val)
      (. (getmetatable multifn) :multimethods :default)))

(fn* core.add
  ([] 0)
  ([a] a)
  ([a b] (+ a b))
  ([a b c] (+ a b c))
  ([a b c d] (+ a b c d))
  ([a b c d & rest] (apply add (+ a b c d) rest)))

(fn* core.sub
  ([] 0)
  ([a] (- a))
  ([a b] (- a b))
  ([a b c] (- a b c))
  ([a b c d] (- a b c d))
  ([a b c d & rest] (apply sub (- a b c d) rest)))

(fn* core.mul
  ([] 1)
  ([a] a)
  ([a b] (* a b))
  ([a b c] (* a b c))
  ([a b c d] (* a b c d))
  ([a b c d & rest] (apply mul (* a b c d) rest)))

(fn* core.div
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
       (and (reduce #(and $1 $2) true (mapv (fn [[k v]] (eq (. y k) v)) (kvseq x)))
            (reduce #(and $1 $2) true (mapv (fn [[k v]] (eq (. x k) v)) (kvseq y))))
       (= x y)))
  ([x y & xs]
   (reduce #(and $1 $2) (eq x y) (mapv #(eq x $) xs))))

core
