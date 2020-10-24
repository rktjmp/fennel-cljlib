(local insert table.insert)
(local _unpack (or table.unpack unpack))
(import-macros {: fn*} :macros.fn)
(import-macros {: when-some} :macros.core)

(fn seq [tbl]
  "Create sequential table.
Transforms original table to sequential table of key value pairs
stored as sequential tables in linear time.  If `tbl' is an
associative table, returns `[[key1 value1] ... [keyN valueN]]' table.
If `tbl' is sequential table, leaves it unchanged."
  (var assoc? false)
  (let [res []]
    (each [k v (pairs tbl)]
      (if (and (not assoc?)
               (not (= (type k) "number")))
          (set assoc? true))
      (insert res [k v]))
    (if assoc? res tbl)))

(fn first [itbl]
  "Return first element of an indexed table."
  (. (seq itbl) 1))


(fn rest [itbl]
  "Returns table of all elements of indexed table but the first one."
  [(_unpack (seq itbl) 2)])


(fn* conj
  "Insert `x' as a last element of indexed table `itbl'. Modifies `itbl'"
  ([] [])
  ([itbl] itbl)
  ([itbl x] (when-some [x x]
              (doto itbl (insert x))))
  ([itbl x & xs]
   (if (> (length xs) 0)
       (let [[y & xs] xs] (conj (conj itbl x) y (_unpack xs)))
       (conj itbl x))))


(fn* consj
  "Like conj but joins at the front. Modifies `itbl'."
  ([] [])
  ([itbl] itbl)
  ([itbl x] (when-some [x x]
              (doto itbl (insert 1 x))))
  ([itbl x & xs]
   (if (> (length xs) 0)
       (let [[y & xs] xs] (consj (consj itbl x) y (_unpack xs)))
       (consj itbl x))))


(fn cons [x itbl]
  "Insert `x' to `itbl' at the front. Modifies `itbl'."
  (when-some [x x]
    (doto (or itbl [])
      (insert 1 x))))


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
  ([f itbl]
   (let [itbl (seq itbl)]
     (match (length itbl)
       0 (f)
       1 (. itbl 1)
       2 (f (. itbl 1) (. itbl 2))
       _ (let [[a b & rest] itbl]
           (reduce f (f a b) rest)))))
  ([f val [x & xs]]
   (if (not (= x nil))
       (reduce f (f val x) xs)
      val)))

(fn* reduce-kv
  "Reduces an associative table using function `f' and initial value `val'.

([f val table])

`f' should be a function of 3 arguments.  Returns the result of
applying `f' to `val', the first key and the first value in coll, then
applying `f' to that result and the 2nd key and value, etc.  If coll
contains no entries, returns `val' and `f' is not called.  Note that
reduce-kv is supported on vectors, where the keys will be the
ordinals."  [f val kvtbl]
  (var res val)
  (each [_ [k v] (pairs (seq kvtbl))]
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
  ([f itbl]
   (local res [])
   (each [_ v (ipairs (seq itbl))]
     (when-some [tmp (f v)]
       (insert res tmp)))
   res)
  ([f t1 t2]
   (let [res []
         t1 (seq t1)
         t2 (seq t2)]
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
         t1 (seq t1)
         t2 (seq t2)
         t3 (seq t3)]
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
                  (cons (mapv #(first (seq $)) tbls) (step (mapv rest tbls)))))
         res []]
     (each [_ v (ipairs (step (consj tbls t3 t2 t1)))]
       (when-some [tmp (f (_unpack v))]
         (insert res tmp)))
     res)))

(fn kvseq [kvtbl]
  (let [res []]
    (each [k v (pairs kvtbl)]
      (insert res [k v]))
    res))

(fn* eq?
  "Deep compare values."
  ([x] true)
  ([x y]
   (if (and (= (type x) "table") (= (type y) "table"))
       (and (reduce #(and $1 $2) true (mapv (fn [[k v]] (eq? (. y k) v)) (kvseq x)))
            (reduce #(and $1 $2) true (mapv (fn [[k v]] (eq? (. x k) v)) (kvseq y))))
       (= x y)))
  ([x y & xs]
   (reduce #(and $1 $2) (eq? x y) (mapv #(eq? x $) xs))))

;;;;;;;;;; fn stuff ;;;;;;;;
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
     ([x y z & args] (f g x y z (_unpack args)))))
  ([f g & fs]
   (reduce comp (conj [f g] (_unpack fs)))))

(fn* every?
  [pred itbl]
  (if (= 0 (length itbl)) true
      (pred (first itbl)) (every? pred (rest itbl))
      false))

;; (fn* some
;;   [pred itbl]
;;   (if (> (length itbl) 0)
;;       ))

(fn* range
  "return range of of numbers from `lower' to `upper' with optional `step'."
  ([upper] (range 0 upper 1))
  ([lower upper] (range lower upper 1))
  ([lower upper step]
   (let [res []]
     (for [i lower (- upper step) step]
       (insert res i))
     res)))

{: seq
 : mapv
 : reduce
 : reduce-kv
 : conj
 : cons
 : first
 : rest
 : eq?
 : identity
 : comp
 : every?
 : range}
