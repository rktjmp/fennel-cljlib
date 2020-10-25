(local insert table.insert)
(local unpack (or table.unpack _G.unpack))
(import-macros {: fn*} :macros.fn)
(import-macros {: when-some : if-some : when-let : into} :macros.core)

(fn* apply
  "Apply `f' to the argument list formed by prepending intervening
arguments to `args'."
  ([f args] (f (unpack args)))
  ([f a args] (f a (unpack args)))
  ([f a b args] (f a b (unpack args)))
  ([f a b c args] (f a b c (unpack args)))
  ([f a b c d & args]
   (let [flat-args []]
     (for [i 1 (- (length args) 1)]
       (insert flat-args (. args i)))
     (each [_ a (ipairs (. args (length args)))]
       (insert flat-args a))
     (f a b c d (unpack flat-args)))))

;; predicate functions

(fn map? [tbl]
  "Check whether `tbl' is an associative table."
  (if (= (type tbl) :table)
      (let [(k _) (next tbl)]
        (and (~= k nil) (or (~= (type k) :number)
                            (~= k 1))))))

(fn seq? [tbl]
  "Check whether `tbl' is an sequential table."
  (if (= (type tbl) :table)
      (let [(k _) (next tbl)]
        (and (~= k nil) (= (type k) :number) (= k 1)))))

(fn nil? [x]
  "Test if value is nil."
  (= x nil))

(fn zero? [x]
  "Test if value is zero."
  (= x 0))

(fn pos? [x]
  "Test if `x' is greater than zero."
  (> x 0))

(fn neg? [x]
  "Test if `x' is less than zero."
  (< x 0))

(fn even? [x]
  "Test if value is even."
  (= (% x 2) 0))

(fn odd? [x]
  "Test if value is odd."
  (not (even? x)))

(fn string? [x]
  "Test if `x' is a string."
  (= (type x) :string))

(fn int? [x]
  "Test if `x' is a number without floating point data."
  (and (= (type x) :number)
       (= x (math.floor x))))

(fn pos-int? [x]
  "Test if `x' is a positive integer."
  (and (int? x)
       (pos? x)))

(fn neg-int? [x]
  "Test if `x' is a negetive integer."
  (and (int? x)
       (neg? x)))

(fn double? [x]
  "Test if `x' is a number with floating point data."
  (and (= (type x) :number)
       (~= x (math.floor x))))

(fn empty? [x]
  "Check if collection is empty."
  (match (type x)
    :table (= (next x) nil)
    :string (= x "")
    _ (error "empty?: unsupported collection")))

(fn not-empty [x]
  "If `x' is empty, returns `nil', otherwise `x'."
  (if (not (empty? x))
      x))

;; sequence manipulating functions

(fn seq [tbl]
  "Create sequential table.
Transforms original table to sequential table of key value pairs
stored as sequential tables in linear time.  If `tbl' is an
associative table, returns `[[key1 value1] ... [keyN valueN]]' table.
If `tbl' is sequential table, leaves it unchanged."
  (when-some [_ (and tbl (next tbl))]
    (var assoc? false)
    (let [res []]
      (each [k v (pairs tbl)]
        (if (and (not assoc?)
                 (not (= (type k) :number)))
            (set assoc? true))
        (insert res [k v]))
      (if assoc? res tbl))))

(macro -safe-seq [tbl]
  "Create sequential table, or empty table if `seq' returned `nil'."
  `(or (seq ,tbl) []))

(fn first [tbl]
  "Return first element of an indexed table."
  (when-some [tbl tbl]
    (. (seq tbl) 1)))

(fn rest [tbl]
  "Returns table of all elements of indexed table but the first one."
  (if-some [tbl tbl]
    [(unpack (seq tbl) 2)]
    []))

(fn* conj
  "Insert `x' as a last element of indexed table `tbl'. Modifies `tbl'"
  ([] [])
  ([tbl] tbl)
  ([tbl x]
   (when-some [x x]
     (let [tbl (or tbl [])]
       (if (map? tbl)
           (tset tbl (. x 1) (. x 2))
           (insert tbl x))
       tbl)))
  ([tbl x & xs]
   (if (> (length xs) 0)
       (let [[y & xs] xs] (apply conj (conj tbl x) y xs))
       (conj tbl x))))

(fn* consj
  "Like conj but joins at the front. Modifies `tbl'."
  ([] [])
  ([tbl] tbl)
  ([tbl x]
   (when-some [x x]
     (doto tbl (insert 1 x))))
  ([tbl x & xs]
   (if (> (length xs) 0)
       (let [[y & xs] xs] (apply consj (consj tbl x) y xs))
       (consj tbl x))))

(fn cons [x tbl]
  "Insert `x' to `tbl' at the front. Modifies `tbl'."
  (when-some [x x]
    (doto (-safe-seq tbl)
      (insert 1 x))))

(fn* concat
  "Concatenate tables."
  ([] nil)
  ([x] (-safe-seq x))
  ([x y] (into (-safe-seq x) (-safe-seq y)))
  ([x y & xs]
   (apply concat (into (-safe-seq x) (-safe-seq y)) xs)))

(fn* reduce
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
   (when-some [tbl (seq tbl)]
     (match (length tbl)
       0 (f)
       1 (. tbl 1)
       2 (f (. tbl 1) (. tbl 2))
       _ (let [[a b & rest] tbl]
           (reduce f (f a b) rest)))))
  ([f val tbl]
   (if-some [tbl (seq tbl)]
     (let [[x & xs] tbl]
       (if (not (= x nil))
           (reduce f (f val x) xs)
           val))
     val)))

(fn* reduce-kv
  "Reduces an associative table using function `f' and initial value `val'.

([f val table])

`f' should be a function of 3 arguments.  Returns the result of
applying `f' to `val', the first key and the first value in coll, then
applying `f' to that result and the 2nd key and value, etc.  If coll
contains no entries, returns `val' and `f' is not called.  Note that
reduce-kv is supported on vectors, where the keys will be the
ordinals."  [f val tbl]
  (var res val)
  (each [_ [k v] (pairs (-safe-seq tbl))]
    (set res (f res k v)))
  res)

(fn* mapv
  "Maps function `f' over one or more tables.

Accepts arbitrary amount of tables, calls `seq' on each of it.
Function `f' must take the same amount of parameters as the amount of
tables passed to `mapv'. Applies `f' over first value of each
table. Then applies `f' to second value of each table. Continues until
any of the tables is exhausted. All remaining values are
ignored. Returns a table of results."
  ([f tbl]
   (local res [])
   (each [_ v (ipairs (-safe-seq tbl))]
     (when-some [tmp (f v)]
       (insert res tmp)))
   res)
  ([f t1 t2]
   (let [res []
         t1 (-safe-seq t1)
         t2 (-safe-seq t2)]
     (var (i1 v1) (next t1))
     (var (i2 v2) (next t2))
     (while (and i1 i2)
       (when-some [tmp (f v1 v2)]
         (insert res tmp))
       (set (i1 v1) (next t1 i1))
       (set (i2 v2) (next t2 i2)))
     res))
  ([f t1 t2 t3]
   (let [res []
         t1 (-safe-seq t1)
         t2 (-safe-seq t2)
         t3 (-safe-seq t3)]
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
                (when (->> tbls
                           (mapv #(~= (next $) nil))
                           (reduce #(and $1 $2)))
                  (cons (mapv #(first (-safe-seq $)) tbls) (step (mapv rest tbls)))))
         res []]
     (each [_ v (ipairs (step (consj tbls t3 t2 t1)))]
       (when-some [tmp (apply f v)]
         (insert res tmp)))
     res)))

(fn filter [pred tbl]
  (when-let [tbl (seq tbl)]
    (let [f (first tbl) r (rest tbl)]
          (if (pred f)
              (cons f (filter pred r))
              (filter pred r)))))

(fn -kvseq [tbl]
  "Transforms any table kind to key-value sequence."
  (let [res []]
    (each [k v (pairs tbl)]
      (insert res [k v]))
    res))

(fn* eq?
  "Deep compare values."
  ([x] true)
  ([x y]
   (if (and (= (type x) :table) (= (type y) :table))
       (and (reduce #(and $1 $2) true (mapv (fn [[k v]] (eq? (. y k) v)) (-kvseq x)))
            (reduce #(and $1 $2) true (mapv (fn [[k v]] (eq? (. x k) v)) (-kvseq y))))
       (= x y)))
  ([x y & xs]
   (reduce #(and $1 $2) (eq? x y) (mapv #(eq? x $) xs))))

(fn identity [x] x)

(fn* comp
  ([] identity)
  ([f] f)
  ([f g]
   (fn*
     ([] (f (g)))
     ([x] (f (g x)))
     ([x y] (f (g x y)))
     ([x y z] (f (g x y z)))
     ([x y z & args] (apply f g x y z args))))
  ([f g & fs]
   (apply reduce comp (conj [f g] fs))))

(fn* every?
  [pred tbl]
  (if (empty? tbl) true
      (pred (first tbl)) (every? pred (rest tbl))
      false))

(fn* some
  [pred tbl]
  (when-let [tbl (seq tbl)]
    (or (pred (first tbl)) (some pred (rest tbl)))))

(local not-any? (comp #(not $) some))

(fn complement [f]
  "Takes a function `f' and returns the function that takes the same
amount of arguments as `f', has the same effect, and returns the
oppisite truth value."
  (fn*
    ([] (not (f)))
    ([a] (not (f a)))
    ([a b] (not (f a b)))
    ([a b & cs] (not (apply f a b cs)))))

(fn constantly [x]
  "Returns a function that takes any number of arguments and returns `x'."
  (fn [...] x))

(fn* range
  "return range of of numbers from `lower' to `upper' with optional `step'."
  ([upper] (range 0 upper 1))
  ([lower upper] (range lower upper 1))
  ([lower upper step]
   (let [res []]
     (for [i lower (- upper step) step]
       (insert res i))
     res)))

(fn reverse [tbl]
  (when-some [tbl (seq tbl)]
    (reduce consj [] tbl)))

{: apply      ;; not tested
 : seq        ;; tested
 : first      ;; not tested
 : rest       ;; not tested
 : conj       ;; not tested
 : cons       ;; not tested
 : concat     ;; tested
 : reduce     ;; tested
 : reduce-kv  ;; tested
 : mapv       ;; tested
 : filter     ;; tested
 : map?       ;; tested
 : seq?       ;; tested
 : nil?       ;; tested
 : zero?      ;; tested
 : pos?       ;; tested
 : neg?       ;; tested
 : even?      ;; tested
 : odd?       ;; tested
 : int?       ;; tested
 : pos-int?   ;; tested
 : neg-int?   ;; tested
 : double?    ;; tested
 : string?    ;; tested
 : empty?     ;; not tested
 : not-empty  ;; not tested
 : eq?        ;; tested
 : identity   ;; not tested
 : comp       ;; not tested
 : every?     ;; not tested
 : some       ;; not tested
 : complement ;; not tested
 : constantly ;; not tested
 : range      ;; tested
 : reverse    ;; not tested
 }
