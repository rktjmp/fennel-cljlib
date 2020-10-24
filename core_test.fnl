(import-macros {: fn*} :macros.fn)
(import-macros {: into} :macros.core)
(import-macros {: assert-eq : assert-ne : assert* : test} :test)

(local {: seq
        : mapv
        : filter
        : reduce
        : reduce-kv
        : conj
        : cons
        : first
        : rest
        : map?
        : seq?
        : eq?
        : identity
        : comp
        : every?
        : some
        : not-any?
        : range
        : even?
        : odd?}
       (require :core))

(test equality-test
  ;; comparing basetypes
  (assert-eq 1 1)
  (assert-ne 1 2)
  (assert* (eq? 1 1 1 1 1))
  (assert-eq 1.0 1.0)
  (assert* (eq? 1.0 1.0 1.0))
  (assert* (eq? 1.0 1.0 1.0))
  (assert* (eq? "1" "1" "1" "1" "1"))

  ;; deep comparison
  (assert* (eq? []))
  (assert-eq [] [])
  (assert-eq [] {})
  (assert-eq [1 2] [1 2])
  (assert-ne [1] [1 2])
  (assert-ne [1 2] [1])
  (assert* (eq? [1 [2]] [1 [2]] [1 [2]]))
  (assert* (eq? [1 [2]] [1 [2]] [1 [2]]))
  (assert* (not (eq? [1 [2]] [1 [2]] [1 [2 [3]]])))
  (assert-eq (range 10) [0 1 2 3 4 5 6 7 8 9])
  (assert-eq (range -5 5) [-5 -4 -3 -2 -1 0 1 2 3 4])
  (assert-eq [0 0.2 0.4 0.6 0.8] [0 0.2 0.4 0.6 0.8])
  (assert-eq (range 0 1 0.2) (range 0 1 0.2))

  (let [a {:a 1 :b 2}
        b {:a 1 :b 2}]
    (table.insert b 10)
    (assert-ne a b))

  (let [a [1 2 3]
        b [1 2 3]]
    (tset b :a 10)
    (assert-ne a b))

  (assert-eq [1 2 3] {1 1 2 2 3 3})

  ;; TODO: decide if this is right or not.  Looking from `seq'
  ;; perspective, it is correct, as `(seq {4 1})' and `(seq [nil nil
  ;; nil 1])' both yield `{4 1}'.  From Lua's point this is not the
  ;; same thing, for example because the sizes of these tables are
  ;; different.
  (assert-eq {4 1} [nil nil nil 1]))

(test seq-test
  (assert-eq (seq []) nil)
  (assert-eq (seq {}) nil)
  (assert-eq (seq [1]) [1])
  (assert-eq (seq [1 2 3]) [1 2 3])
  (assert-eq (seq {:a 1}) [["a" 1]]))

(test mapv-test
  (assert-eq (mapv #(* $ $) [1 2 3 4]) [1 4 9 16])

  (assert-eq (into {} (mapv (fn [[k v]] [k (* v v)]) {:a 1 :b 2 :c 3}))
             (into {} [[:a 1] [:b 4] [:c 9]]))

  (assert-eq (into {} (mapv (fn [[k1 v1] [k2 v2]] [k1 (* v1 v2)])
                            {:a 1 :b 2 :c 3}
                            {:a -1 :b 0 :c 2}))
             {:a -1 :b 0 :c 6})
  (assert-eq (mapv string.upper ["a" "b" "c"]) ["A" "B" "C"])
  (assert-eq (mapv #(+ $1 $2 $3 $4) [1 -1] [2 -2] [3 -3] [4 -4]) [(+ 1 2 3 4) (+ -1 -2 -3 -4)])
  (assert-eq (mapv (fn [f-name s-name company position]
                     (.. f-name " " s-name " works as " position " at " company))
                   ["Bob" "Alice"]
                   ["Smith" "Watson"]
                   ["Happy Days co." "Coffee With You"]
                   ["secretary" "chief officer"])
             ["Bob Smith works as secretary at Happy Days co."
              "Alice Watson works as chief officer at Coffee With You"]))

(test reduce-test
  (fn* ++
    ([] 0)
    ([a] a)
    ([a b] (+ a b))
    ([a b & c]
     (var res (+ a b))
     (each [_ v (ipairs c)]
       (set res (+ res v)))
     res))

  (assert-eq (reduce ++ (range 10)) 45)
  (assert-eq (reduce ++ -3 (range 10)) 42)
  (assert-eq (reduce ++ 10 nil) 10)


  (fn mapping [f]
    (fn [reducing]
      (fn [result input]
        (reducing result (f input)))))

  (fn reduce- [f init tbl]
    (if (and tbl (> (length tbl) 0))
        (reduce f (f init (first tbl)) (rest tbl))
        init))

  (assert-eq (reduce ++ (range 10)) (reduce- ++ 0 (range 10))))

(test filter-test
  (assert-eq (filter even? (range 10)) [0 2 4 6 8])
  (assert-eq (filter odd? (range 10)) [1 3 5 7 9])
  (assert-eq (filter map? [{:a 1} {5 1} [1 2] [] {}]) [{:a 1} {5 1}])
  (assert-eq (filter seq? [{:a 1} {5 1} [1 2] [] {}]) [[1 2]]))
