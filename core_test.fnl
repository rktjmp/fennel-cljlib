(import-macros {: fn*} :macros.fn)
(import-macros {: into} :macros.core)
(import-macros {: assert-eq : assert-ne : assert* : test} :test)

(local
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
  : empty?     ;; tested
  : not-empty  ;; tested
  : eq?        ;; tested
  : identity   ;; tested
  : comp       ;; not tested
  : every?     ;; tested
  : some       ;; tested
  : complement ;; tested
  : constantly ;; tested
  : range      ;; tested
  : reverse    ;; tested
  }
 (require :core))

(test equality
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

(test seq
  (assert-eq (seq []) nil)
  (assert-eq (seq {}) nil)
  (assert-eq (seq [1]) [1])
  (assert-eq (seq [1 2 3]) [1 2 3])
  (assert-eq (seq {:a 1}) [["a" 1]]))

(test mapv
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

(test reduce
  (fn* plus
    ([] 0)
    ([a] a)
    ([a b] (+ a b))
    ([a b & c]
     (var res (+ a b))
     (each [_ v (ipairs c)]
       (set res (+ res v)))
     res))

  (assert-eq (reduce plus (range 10)) 45)
  (assert-eq (reduce plus -3 (range 10)) 42)
  (assert-eq (reduce plus 10 nil) 10)


  (fn mapping [f]
    (fn [reducing]
      (fn [result input]
        (reducing result (f input)))))

  (fn reduce- [f init tbl]
    (if (and tbl (> (length tbl) 0))
        (reduce f (f init (first tbl)) (rest tbl))
        init))

  (assert-eq (reduce plus (range 10)) (reduce- plus 0 (range 10))))

(test predicate
  (assert* (int? 1))
  (assert* (not (int? 1.1)))
  (assert* (pos? 1))
  (assert* (and (not (pos? 0)) (not (pos? -1))))
  (assert* (neg? -1))
  (assert* (and (not (neg? 0)) (not (neg? 1))))
  (assert* (pos-int? 42))
  (assert* (not (pos-int? 4.2)))
  (assert* (neg-int? -42))
  (assert* (not (neg-int? -4.2)))
  (assert* (string? :s))
  (assert* (double? 3.3))
  (assert* (not (double? 3.0)))
  (assert* (map? {:a 1}))
  (assert* (not (map? {})))
  (assert* (not (seq? [])))
  (assert* (seq? [{:a 1}]))
  (assert* (not (seq? {})))
  (assert* (not (seq? {:a 1})))
  (assert* (nil?))
  (assert* (nil? nil))
  (assert* (not (nil? 1))))

(test filter
  (assert-eq (filter even? (range 10)) [0 2 4 6 8])
  (assert-eq (filter odd? (range 10)) [1 3 5 7 9])
  (assert-eq (filter map? [{:a 1} {5 1} [1 2] [] {}]) [{:a 1} {5 1}])
  (assert-eq (filter seq? [{:a 1} {5 1} [1 2] [] {}]) [[1 2]]))

(test concat
  (assert-eq (concat) nil)
  (assert-eq (concat []) [])
  (assert-eq (concat [1 2 3]) [1 2 3])
  (assert-eq (concat [1 2 3] [4 5 6]) [1 2 3 4 5 6])
  (assert-eq (concat [1 2] [3 4] [5 6]) [1 2 3 4 5 6])
  (assert-eq (concat {:a 1} {:b 2}) [[:a 1] [:b 2]]))

(test reverse
  (assert-eq (reverse [1 2 3]) [3 2 1])
  (assert-eq (reverse {:a 1}) [[:a 1]]))

(test constantly
  (let [always-nil (constantly nil)]
    (assert-eq (always-nil) nil)
    (assert-eq (always-nil 1) nil)
    (assert-eq (always-nil 1 2 3 4 "5") nil))

  (let [always-true (constantly true)]
    (assert* (always-true))
    (assert* (always-true false))))

(test complement
  (assert* ((complement nil?) 10)))

(test some
  (assert* (some pos-int? [-1 1.1 2.3 -5.5 42 10 -27]))
  (assert* (not (some pos-int? {:a 1})))
  (assert* (some pos-int? [{:a 1} "1" -1 1])))

(test every?
  (assert* (not (every? pos-int? [-1 1.1 2.3 -5.5 42 10 -27])))
  (assert* (not (every? pos-int? {:a 1})))
  (assert* (every? pos-int? [1 2 3 4 5])))

(test identity
  (assert-eq (identity 1) 1)
  (assert-eq (identity {:a 1 :b 2}) {:a 1 :b 2})
  (assert-eq (identity [1 2 3]) [1 2 3])
  (assert-eq (identity "abc") "abc"))

(test empty?
  (assert* (empty? []))
  (assert* (empty? {}))
  (assert* (empty? ""))
  (assert* (not (empty? "1")))
  (assert* (not (empty? [1])))
  (assert* (not (empty? {:a 1}))))

(test not-empty
  (assert-eq (not-empty []) nil)
  (assert-eq (not-empty {}) nil)
  (assert-eq (not-empty "") nil)
  (assert-eq (not-empty "1") "1")
  (assert-eq (not-empty [1]) [1])
  (assert-eq (not-empty {:a 1}) {:a 1}))
