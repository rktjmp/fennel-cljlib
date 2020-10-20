(local insert table.insert)
(local _unpack (or table.unpack unpack))

(fn first [itbl]
  "Return first element of an indexed table."
  (. itbl 1))


(fn rest [itbl]
  "Returns table of all elements of inexed table but the first one."
  (let [[_ & xs] itbl]
    xs))


(fn conj [...]
  "Insert `x' as a last element of indexed table `itbl'. Modifies `itbl'"
  (match (length [...])
    0 []
    1 (let [[itbl] [...]] itbl)
    2 (let [[itbl x] [...]] (insert itbl x) itbl)
    _ (let [[itbl x & xs] [...]]
        (if (> (length xs) 0)
            (let [[y & xs] xs] (conj (conj itbl x) y (_unpack xs)))
            (conj itbl x)))))


(fn consj [...]
  "Like conj but joins at the front. Modifies `itbl'."
  (match (length [...])
    0 []
    1 (let [[itbl] [...]] itbl)
    2 (let [[itbl x] [...]] (insert itbl 1 x) itbl)
    _ (let [[itbl x & xs] [...]]
        (if (> (length xs) 0)
            (let [[y & xs] xs] (consj (consj itbl x) y (_unpack xs)))
            (consj itbl x)))))


(fn cons [x itbl]
  "Insert `x' to `itbl' at the front. Modifies `itbl'."
  (doto (or itbl [])
    (insert 1 x)))


(fn reduce3 [f val [x & xs]]
  (if (not (= x nil))
      (reduce3 f (f val x) xs)
      val))

(fn reduce [...]
  "Reduce collection using function of two arguments and optional initial value.

f should be a function of 2 arguments.  If val is not supplied,
returns the result of applying f to the first 2 items in coll, then
applying f to that result and the 3rd item, etc.  If coll contains no
items, f must accept no arguments as well, and reduce returns the
result of calling f with no arguments.  If coll has only 1 item, it is
returned and f is not called.  If val is supplied, returns the result
of applying f to val and the first item in coll, then applying f to
that result and the 2nd item, etc.  If coll contains no items, returns
val and f is not called."
  (match (length [...])
    2 (let [[f itbl] [...]]
        (match (length itbl)
          0 (f)
          1 (. itbl 1)
          2 (f (. itbl 1) (. itbl 2))
          _ (let [[a b & rest] itbl]
              (reduce3 f (f a b) rest))))
    3 (let [[f val itbl] [...]]
        (reduce3 f val itbl))
    _ (error "wrong amount of arguments to reduce" 2)))


(fn mapv [...]
  "Maps function `f' over indexed tables.

Accepts arbitrary amount of tables.  Function `f' must take the same
amount of parameters as the amount of tables passed to `mapv'. Applyes
`f' over first value of each table. Then applies `f' to second value
of each table. Continues until any of the tables is exhausted. All
remaining values are ignored. Returns a table of results. "
  (let [res []]
    (match (length [...])
      1 (error "wrong argument amount for mapv" 2)
      2 (let [[f itbl] [...]]
          (each [_ v (ipairs itbl)]
            (insert res (f v))))
      3 (let [[f t1 t2] [...]]
          (var (i1 v1) (next t1))
          (var (i2 v2) (next t2))
          (while (and i1 i2)
            (insert res (f v1 v2))
            (set (i1 v1) (next t1 i1))
            (set (i2 v2) (next t2 i2))))
      4 (let [[f t1 t2 t3] [...]]
          (var (i1 v1) (next t1))
          (var (i2 v2) (next t2))
          (var (i3 v3) (next t3))
          (while (and i1 i2 i3)
            (insert res (f v1 v2 v3))
            (set (i1 v1) (next t1 i1))
            (set (i2 v2) (next t2 i2))
            (set (i3 v3) (next t3 i3))))
      _ (let [[f t1 t2 t3 & tbls] [...]
              step (fn step [tbls]
                     (when (->> tbls
                                (mapv #(if (next $) true false))
                                (reduce #(and $1 $2)))
                       (cons (mapv first tbls) (step (mapv rest tbls)))))]
          (each [_ v (ipairs (step (consj tbls t3 t2 t1)))]
            (insert res (f (_unpack v))))))
    res))

(fn kvseq [kvtbl]
  (let [res []]
    (each [k v (pairs kvtbl)]
      (insert res [k v]))
    res))

(fn mapkv [...]
  "Maps function `f' over one or more associative tables.

`f' should be a function of 2 arguments. If more than one table
supplied, `f' must take double the table amount of arguments.  Returns
indexed table of results.  Order of results depends on the order
returned by the `pairs' function. If you want consistent results, consider
sorting tables first."
  (match (length [...])
    2 (let [[f kvtbl] [...]]
        (var res [])
        (each [k v (pairs kvtbl)]
          (insert res (f k v)))
        res)
    _ (let [[f & kvtbls] [...]
            itbls []]
        (each [_ t (ipairs kvtbls)]
          (insert itbls (kvseq t)))
        (mapv f (_unpack itbls)))))


(fn eq2 [a b]
  (if (and (= (type a) "table") (= (type b) "table"))
      (and (reduce #(and $1 $2) (mapkv (fn [k v] (eq2 (. b k) v)) a))
           (reduce #(and $1 $2) (mapkv (fn [k v] (eq2 (. a k) v)) b)))
      (= a b)))

(fn eq? [...]
  "Deep compare values."
  (let [[x & xs] [...]]
    (reduce #(and $1 $2) (mapv #(eq2 x $) xs))))


{: mapv
 : mapkv
 : reduce
 : conj
 : cons
 : first
 : rest
 : eq?}

;; (local {: mapv : mapkv : reduce : conj : cons : first : rest : eq?} (require :core))
