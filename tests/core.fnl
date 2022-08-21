(import-macros clj :init-macros)
(require-macros :fennel-test)

(local core (require :init))

(deftest test-equality
  (testing "comparing base-types"
    (assert-is (core.eq))
    (assert-is (core.eq 1 1))
    (assert-not (core.eq 1 2))
    (assert-is (core.eq 1 1 1 1 1))
    (assert-is (core.eq 1.0 1.0))
    (assert-is (core.eq 1.0 1.0 1.0))
    (assert-is (core.eq 1.0 1.0 1.0))
    (assert-is (core.eq "1" "1" "1" "1" "1")))

  (testing "deep comparison"
    (assert-is (core.eq []))
    (assert-is (core.eq [] []))
    (assert-is (core.eq [] {}))
    (assert-is (core.eq [1 2] [1 2]))
    (assert-is (core.eq [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]
                        [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]))
    (assert-is (core.eq [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]
                        [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]))
    (assert-is (core.eq [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]
                        [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]
                        [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]))
    (assert-not (core.eq [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]
                         [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]
                         [[1 [2 [3]] {[6] {:a [1 [1 [1 [1]]]]}}]]))
    (assert-not (core.eq [1] [1 2]))
    (assert-not (core.eq [1 2] [1]))
    (assert-is (core.eq [1 [2]] [1 [2]] [1 [2]]))
    (assert-is (core.eq [1 [2]] [1 [2]] [1 [2]]))
    (assert-not (core.eq [1 [2]] [1 [2]] [1 [2 [3]]]))
    (assert-not (core.eq {:a {:b 2}} {:a {:b 2}} {:a {:b 3}}))

    (let [a {:a 1 :b 2}
          b {:a 1 :b 2}]
      (table.insert b 10)
      (assert-not (core.eq a b)))

    (let [a [1 2 3]
          b [1 2 3]]
      (tset b :a 10)
      (assert-not (core.eq a b)))

    (assert-is (core.eq [1 2 3] {1 1 2 2 3 3}))
    (assert-is (core.eq {4 1} [nil nil nil 1]))))

(deftest test-range
  (testing "range"
    (assert-is (core.range))
    (assert-eq (core.range 10) (core.list 0 1 2 3 4 5 6 7 8 9))
    (assert-eq (core.range -5 5) (core.list -5 -4 -3 -2 -1 0 1 2 3 4))
    (assert-eq (core.range 0 1 0.2) (core.range 0 1 0.2))))

(deftest test-empty
  (testing "empty map"
    (assert-not (pcall core.empty))
    (assert-eq (core.empty {}) {})
    (assert-eq (core.empty "") "")
    (assert-eq (core.hash-set) (core.empty (core.hash-set :a :b :c)))
    (assert-eq (core.hash-map) (core.empty (core.hash-map :a :b :c :d)))
    (assert-eq (core.vector) (core.empty (core.vector :a :b :c :d)))
    (assert-eq (core.list) (core.empty (core.list :a :b :c :d)))
    (let [a {:a 1 :b 2}]
      (assert-eq (core.empty a) {}))
    (let [a {}]
      (assert-eq (core.empty a) []))))

(deftest test-predicates
  (testing "zero?"
    (assert-is (core.zero? 0))
    (assert-is (core.zero? -0))
    (assert-not (core.zero? 1))
    (assert-not (pcall core.zero?))
    (assert-not (pcall core.zero? 1 2)))

  (testing "int?"
    (assert-is (core.int? 1))
    (assert-not (core.int? 1.1))
    (assert-not (pcall core.int?))
    (assert-not (pcall core.int? 1 2)))

  (testing "pos?"
    (assert-is (core.pos? 1))
    (assert-is (and (not (core.pos? 0)) (not (core.pos? -1))))
    (assert-not (pcall core.pos?))
    (assert-not (pcall core.pos? 1 2)))

  (testing "neg?"
    (assert-is (core.neg? -1))
    (assert-is (and (not (core.neg? 0)) (not (core.neg? 1))))
    (assert-not (pcall core.neg?))
    (assert-not (pcall core.neg? 1 2)))

  (testing "pos-int?"
    (assert-is (core.pos-int? 42))
    (assert-not (core.pos-int? 4.2))
    (assert-not (pcall core.pos-int?))
    (assert-not (pcall core.pos-int? 1 2)))

  (testing "neg-int?"
    (assert-is (core.neg-int? -42))
    (assert-not (core.neg-int? -4.2))
    (assert-not (pcall core.neg-int?))
    (assert-not (pcall core.neg-int? 1 2)))

  (testing "string?"
    (assert-is (core.string? :s))
    (assert-not (pcall core.string?))
    (assert-not (pcall core.string? 1 2)))

  (testing "double?"
    (assert-is (core.double? 3.3))
    (assert-not (core.double? 3.0))
    (assert-not (pcall core.double?))
    (assert-not (pcall core.double? 1 2)))

  (testing "map?"
    (assert-is (core.map? {:a 1}))
    (assert-not (core.map? {}))
    (assert-is (core.map? (core.empty (core.hash-map))))
    (assert-not (core.map? (core.empty [])))
    (assert-not (pcall core.map?))
    (assert-not (pcall core.map? 1 2)))

  (testing "vector?"
    (assert-not (core.vector? []))
    (assert-is (core.vector? [{:a 1}]))
    (assert-not (core.vector? {}))
    (assert-not (core.vector? {:a 1}))
    (assert-is (core.vector? (core.empty (core.vector))))
    (assert-not (core.vector? (core.empty {})))
    (assert-not (pcall core.vector?))
    (assert-not (pcall core.vector? 1 2)))

  (testing "multifn?"
    (assert-not (core.multifn? []))
    (assert-is (core.multifn? (do (clj.defmulti f core.identity) f)))
    (assert-not (pcall core.multifn?))
    (assert-not (pcall core.multifn? 1 2)))

  (testing "set?"
    (assert-is (core.set? (core.hash-set)))
    (assert-not (core.set? {}))
    (assert-not (core.set? (core.hash-map)))
    (assert-not (pcall core.set?))
    (assert-not (pcall core.set? 1 2)))

  (testing "seq?"
    (assert-is (core.seq? (core.list)))
    (assert-not (core.seq? {}))
    (assert-not (core.seq? (core.hash-map)))
    (assert-not (pcall core.seq?))
    (assert-not (pcall core.seq? 1 2)))

  (testing "nil?"
    (assert-is (core.nil?))
    (assert-is (core.nil? nil))
    (assert-not (core.nil? 1))
    (assert-not (pcall core.nil? 1 2)))

  (testing "odd?"
    (assert-is (core.odd? 3))
    (assert-is (core.odd? -3))
    (assert-not (core.odd? 2))
    (assert-not (core.odd? -2))
    (assert-not (pcall core.odd?))
    (assert-not (pcall core.odd? 1 2)))

  (testing "even?"
    (assert-is (core.even? 2))
    (assert-is (core.even? -2))
    (assert-not (core.even? 23))
    (assert-not (core.even? -23))
    (assert-not (pcall core.even?))
    (assert-not (pcall core.even? 1 2)))

  (testing "true?"
    (assert-is (core.true? true))
    (assert-not (core.true? false))
    (assert-not (core.true? 10))
    (assert-not (core.true? :true))
    (assert-not (pcall core.true?))
    (assert-not (pcall core.true? 1 2)))

  (testing "false?"
    (assert-is (core.false? false))
    (assert-not (core.false? true))
    (assert-not (core.false? 10))
    (assert-not (core.false? :true))
    (assert-not (pcall core.false?))
    (assert-not (pcall core.false? 1 2)))

  (testing "boolean?"
    (assert-is (core.boolean? true))
    (assert-is (core.boolean? false))
    (assert-not (core.boolean? :false))
    (assert-not (core.boolean? (fn [] true)))
    (assert-not (pcall core.boolean?))
    (assert-not (pcall core.boolean? 1 2))))

(deftest test-sequence-functions
  (testing "seq"
    (assert-not (pcall core.seq))
    (assert-not (pcall core.seq [] []))
    (assert-eq (core.seq []) nil)
    (assert-eq (core.seq {}) nil)
    (assert-eq (core.seq [1]) (core.list 1))
    (assert-eq (core.seq [1 2 3]) (core.list 1 2 3))
    (assert-eq (core.seq {:a 1}) (core.list (core.vec ["a" 1])))
    (assert-eq (core.seq "abc") (core.list "a" "b" "c"))
    (when _G.utf8 (assert-eq (core.seq "абв") (core.list "а" "б" "в")))
    (assert-eq (core.seq {12345 123}) (core.list (core.vec [12345 123])))
    (assert-eq (core.seq (core.hash-set 1)) (core.list 1))
    (assert-eq (length (core.seq (core.hash-set 1 2 3))) 3))

  (testing "mapv"
    (assert-not (pcall core.mapv))
    (assert-not (pcall core.mapv #(do nil)))
    (assert-eq (core.mapv #(* $ $) [1 2 3 4]) [1 4 9 16])

    (assert-eq (core.into (core.hash-map)
                          (core.mapv (fn [[k v]] [k (* v v)]) {:a 1 :b 2 :c 3}))
               (core.into (core.hash-map)
                          [[:a 1] [:b 4] [:c 9]]))

    (assert-eq (core.into (core.hash-map)
                          (core.mapv (fn [[k1 v1] [k2 v2]] [k1 (* v1 v2)])
                                     {:a 1 :b 2 :c 3}
                                     {:a -1 :b 0 :c 2}))
               {:a -1 :b 0 :c 6})
    (assert-eq (core.mapv #(* $1 $2 $3) [1] [2] [-1]) [-2])
    (assert-eq (core.mapv string.upper ["a" "b" "c"]) ["A" "B" "C"])
    (assert-eq (core.mapv #(+ $1 $2 $3 $4) [1 -1] [2 -2] [3 -3] [4 -4]) [(+ 1 2 3 4) (+ -1 -2 -3 -4)])
    (assert-eq (core.mapv (fn [f-name s-name company position]
                            (.. f-name " " s-name " works as " position " at " company))
                          ["Bob" "Alice"]
                          ["Smith" "Watson"]
                          ["Happy Days co." "Coffee With You"]
                          ["secretary" "chief officer"])
               ["Bob Smith works as secretary at Happy Days co."
                "Alice Watson works as chief officer at Coffee With You"])
    (assert-eq (table.concat (icollect [_ v (ipairs (core.mapv string.upper "vaiv"))] v)) "VAIV"))

  (testing "partition"
    (assert-not (pcall core.partition))
    (assert-not (pcall core.partition 1))
    (assert-eq (core.partition 1 [1 2 3 4]) (core.list (core.list 1) (core.list 2) (core.list 3) (core.list 4)))
    (assert-eq (core.partition 1 2 [1 2 3 4]) (core.list (core.list 1) (core.list 3)))
    (assert-eq (core.partition 3 2 [1 2 3 4 5]) (core.list (core.list 1 2 3) (core.list 3 4 5)))
    (assert-eq (core.partition 3 3 [0 -1 -2 -3] [1 2 3 4]) (core.list (core.list 1 2 3) (core.list 4 0 -1))))

  (testing "nthrest"
    (assert-not (pcall core.nthrest))
    (assert-not (pcall core.nthrest []))
    (assert-eq (core.nthrest [1 2 3] 0) [1 2 3])
    (assert-eq (core.nthrest [1 2 3] 1) (core.list 2 3))
    (assert-eq (core.nthrest [1 2 3] 2) (core.list 3))
    (assert-eq (core.nthrest [1 2 3] 3) (core.list)))

  (testing "take"
    (assert-not (pcall core.take))
    (assert-is (core.take []))
    (assert-not (pcall core.dorun (core.take :a [])))
    (assert-is (core.take -1 []))
    (assert-eq (core.take 0 [1 2 3]) [])
    (assert-eq (core.take 1 {:a 1}) (core.list [:a 1]))
    (assert-eq (core.take 10 [1 2 3]) (core.list 1 2 3))
    (assert-eq (core.take 1 [1 2 3]) (core.list 1)))

  (testing "reduce"
    (assert-eq (core.reduce core.add []) 0)
    (assert-eq (core.reduce core.add [1]) 1)
    (assert-eq (core.reduce core.add [1 2]) 3)
    (assert-eq (core.reduce core.add (core.range 10)) 45)
    (assert-eq (core.reduce core.add -3 (core.range 10)) 42)
    (assert-eq (core.reduce core.add 10 []) 10)
    (assert-eq (core.reduce core.add 10 [1]) 11)
    (assert-eq (core.reduce core.add 10 nil) 10)
    (assert-not (pcall core.reduce))
    (assert-not (pcall core.reduce core.add)))

  (testing "reduce reference implementation"
    (fn mapping [f]
      (fn [reducing]
        (fn [result input]
          (reducing result (f input)))))

    (fn -reduce [f init [x & tbl]]
      (if x (-reduce f (f init x) tbl) init))

    (assert-eq (core.reduce core.add (core.range 10)) (-reduce core.add 0 (core.range 10)))
    (assert-eq (core.reduce ((mapping core.inc) core.add) 0 (core.range 10))
               (-reduce ((mapping core.inc) core.add) 0 (core.range 10))))

  (testing "filter"
    (assert-not (pcall core.filter))
    (assert-is (core.filter core.even?))
    (assert-eq (core.filter core.even? (core.range 10)) (core.list 0 2 4 6 8))
    (assert-eq (core.filter core.odd? (core.range 10)) (core.list 1 3 5 7 9))
    (assert-eq (core.filter core.map? [{:a 1} {5 1} [1 2] [] {}]) (core.list {:a 1} {5 1}))
    (assert-eq (core.filter core.vector? [{:a 1} {5 1} [1 2] [] {}]) (core.list [1 2])))

  (testing "concat"
    (assert-eq (core.concat) (core.list))
    (assert-eq (core.concat []) (core.list))
    (assert-eq (core.concat [1 2 3]) (core.list 1 2 3))
    (assert-eq (core.concat [1 2 3] [4 5 6]) (core.list 1 2 3 4 5 6))
    (assert-eq (core.concat [1 2] [3 4] [5 6]) (core.list 1 2 3 4 5 6))
    (assert-eq (core.concat {:a 1} {:b 2}) (core.list [:a 1] [:b 2]))
    (assert-eq (core.concat [[:a 1]] {:b 2}) (core.list [:a 1] [:b 2]))
    (assert-eq (core.concat {:a 1} [[:b 2]]) (core.list [:a 1] [:b 2]))
    (assert-eq (core.concat [] [[:b 2]]) (core.list [:b 2]))
    (assert-eq (core.concat [] []) (core.list))
    (assert-not (pcall core.dorun (core.concat 1)))
    (assert-not (pcall core.dorun (core.concat 1 2)))
    (assert-not (pcall core.dorun (core.concat 1 [])))
    (assert-not (pcall core.dorun (core.concat [] 2)))
    (assert-not (pcall core.dorun (core.concat [1] 2))))

  (testing "reverse"
    (assert-not (pcall core.reverse))
    (assert-not (pcall core.reverse [] []))
    (assert-eq (core.reverse []) (core.list))
    (assert-eq (core.reverse [1 2 3]) (core.list 3 2 1))
    (assert-eq (core.reverse {:a 1}) (core.list [:a 1])))

  (testing "conj"
    (assert-eq (core.conj) [])
    (assert-eq (core.conj [1]) [1])
    (assert-eq (core.conj (core.vector) 1 2 3) [1 2 3])
    (assert-eq (core.conj [0] 1 2 3) [0 1 2 3])
    (assert-eq (core.conj {:a 1} [:b 2]) {:a 1 :b 2})
    (assert-eq (core.conj {:a 1}) {:a 1})
    (assert-eq (core.conj [1] 2 3 4 5 6 7) [1 2 3 4 5 6 7]))

  (testing "disj"
    (assert-not (pcall core.disj))
    (assert-is (core.disj [1]))
    (assert-not (pcall core.disj [1] 1))
    (assert-eq (core.disj (core.hash-set)) (core.hash-set))
    (assert-eq (core.disj (core.hash-set 1 3 2 5) 3) (core.hash-set 1 2 5))
    (assert-eq (core.disj (core.hash-set 1 3 2 5) 3 1 5) (core.hash-set 2)))

  (testing "cons"
    (assert-not (pcall core.cons))
    (assert-not (pcall core.cons [] [] []))
    (assert-eq (core.cons nil [1]) (core.list nil 1))
    (assert-eq (core.cons 1 []) (core.list 1))
    (assert-eq (core.cons 1 [0]) (core.list 1 0)))

  (testing "first"
    (assert-not (pcall core.first))
    (assert-not (pcall core.first [] []))
    (assert-eq (core.first [1 2 3]) 1)
    (assert-eq (core.first {:a 1}) [:a 1])
    (assert-eq (core.first []) nil))

  (testing "last"
    (assert-not (pcall core.last))
    (assert-not (pcall core.last [] []))
    (assert-eq (core.last [1 2 3]) 3)
    (assert-eq (core.last []) nil)
    (assert-eq (core.last nil) nil)
    (assert-eq (core.last {:a 1}) [:a 1]))

  (testing "rest"
    (assert-not (pcall core.rest))
    (assert-not (pcall core.rest [] []))
    (assert-eq (core.rest [1 2 3]) (core.list 2 3))
    (assert-eq (core.rest {:a 1}) (core.list))
    (assert-eq (core.rest []) (core.list))
    (assert-eq (core.rest nil) (core.list)))

  (testing "butlast"
    (assert-not (pcall core.butlast))
    (assert-not (pcall core.butlast [] []))
    (assert-eq (core.butlast [1 2 3]) (core.list 1 2))
    (assert-eq (core.butlast {:a 1}) nil)
    (assert-eq (core.butlast []) nil)
    (assert-eq (core.butlast nil) nil))

  (testing "reduce-kv"
    (assert-eq (core.reduce-kv #(+ $1 $3) 0 {:a 1 :b 2 :c 3}) 6)
    (assert-eq (core.reduce-kv #(+ $1 $3) 0 [1 2 3]) 6)
    (assert-not (pcall core.reduce-kv #(+ $1 $3) 0))
    (assert-not (pcall core.reduce-kv #(+ $1 $3)))
    (assert-not (pcall core.reduce-kv)))

  (testing "reduced"
    (assert-not (pcall core.reduced))
    (assert-not (pcall core.reduced 1 2 3))
    (assert-eq (core.reduce #(if (> $1 10) (core.reduced -1) (+ $1 $2)) [1]) 1)
    (assert-eq (core.reduce #(if (> $1 10) (core.reduced -1) (+ $1 $2)) [1 2]) 3)
    (assert-eq (core.reduce #(if (> $1 10) (core.reduced -1) (+ $1 $2)) [1 2 3 4]) 10)
    (assert-eq (core.reduce #(if (> $1 10) (core.reduced -1) (+ $1 $2)) [1 2 3 4 5]) 15)
    (assert-eq (core.reduce #(if (> $1 10) (core.reduced -1) (+ $1 $2)) [1 2 3 4 5 6]) -1)
    (assert-eq (core.reduce #(if (> $1 10) (core.reduced -1) (+ $1 $2)) 10 [1]) 11)
    (assert-eq (core.reduce #(if (> $1 10) (core.reduced -1) (+ $1 $2)) 10 [1 2]) -1)
    (assert-eq (core.reduce #(if (> $1 10) (core.reduced -1) (+ $1 $2)) 0 [10 5]) 15)
    (assert-eq (core.reduce #(if (> $1 10) (core.reduced -1) (+ $1 $2)) 1 [10 7]) -1)

    (assert-eq (core.reduce #(if (> $1 10) (core.reduced false) (+ $1 $2)) 1 [10 7]) false)
    (assert-eq (core.reduce #(if (> $1 10) (core.reduced nil) (+ $1 $2)) 1 [10 7]) nil)

    (assert-eq (core.reduce-kv (fn [res _ v] (if (> res 10) (core.reduced -1) (+ res v))) 0 {:a 1 :b 2}) 3)
    (assert-eq (core.reduce-kv (fn [res _ v] (if (> res 10) (core.reduced -1) (+ res v))) 0 {:a 10 :b 2}) 12)
    (assert-eq (core.reduce-kv (fn [res _ v] (if (> res 10) (core.reduced -1) (+ res v))) 1 {:a 3 :b 3 :c 3 :d 3}) 13)
    (assert-eq (core.reduce-kv (fn [res _ v] (if (> res 10) (core.reduced -1) (+ res v))) 2 {:a 3 :b 3 :c 3 :d 3}) -1)
    (assert-eq (core.reduce-kv (fn [res _ v] (if (> res 10) (core.reduced -1) (+ res v))) 1 [10 12]) -1)

    (assert-eq (core.reduce-kv (fn [res _ v] (if (> res 10) (core.reduced false) (+ res v))) 1 [10 12]) false)
    (assert-eq (core.reduce-kv (fn [res _ v] (if (> res 10) (core.reduced nil) (+ res v))) 1 [10 12]) nil))

  (testing "assoc"
    (assert-not (pcall core.assoc))
    (assert-is (core.assoc {}))
    (assert-eq (core.assoc {} :a 1) {:a 1})
    (assert-eq (core.assoc {} :a 1 :b 2 :c 3 :d 4) {:a 1 :b 2 :c 3 :d 4}))

  (testing "dissoc"
    (assert-not (pcall core.dissoc))
    (assert-eq (core.dissoc {}) {})
    (assert-eq (core.dissoc {:a 1 :b 2} :b) {:a 1})
    (assert-eq (core.dissoc {:a 1 :b 2 :c 3} :a :c) {:b 2}))

  (testing "find, keys and vals"
    (assert-not (pcall core.keys))
    (assert-not (pcall core.keys {} {} {}))
    (assert-not (pcall core.vals))
    (assert-not (pcall core.vals {} {} {}))
    (assert-not (pcall core.find))
    (assert-not (pcall core.find {} {} {}))
    (assert-eq (core.keys {:a 1}) (core.list :a))
    (assert-eq (core.vals {:a 1}) (core.list 1))
    (assert-eq (core.keys {:a 1}) (core.list :a))
    (assert-eq (core.vals {:a 1}) (core.list 1))
    (assert-eq (core.find {:a 1 :b 2 :c 3} :c) [:c 3])
    (assert-eq (core.find {:a 1 :b 2 :c 3} :d) nil)))

(deftest transients
  (testing "transient from collections"
    (assert-is (core.transient (core.vector)))
    (assert-is (core.transient (core.hash-map)))
    (assert-is (core.transient (core.hash-set)))
    (assert-not (pcall core.transient []))
    (assert-not (pcall core.transient (core.list))))

  (testing "transient conj!"
    (assert-eq (core.conj!) (core.transient (core.vector)))
    (assert-eq (core.conj! (core.transient (core.vec [1]))) [])
    (assert-eq (core.conj! (core.transient (core.vector)) 1) [1])
    (assert-eq (core.conj! (core.transient (core.vec [0])) 1) [nil 1])
    (assert-eq (core.conj! (core.transient (core.hash-map :a 1)) [:b 2]) {:b 2})
    (assert-eq (. (core.conj! (core.transient (core.hash-map :a 1)) [:a 2]) :a) 2)
    (assert-eq (core.conj! (core.transient (core.hash-map :a 1))) {})
    (assert-eq (. (core.conj! (core.transient (core.hash-map :a 1))) :a) 1))

  (testing "transient disj!"
    (assert-not (pcall core.disj!))
    (assert-is (core.disj! [1]))
    (assert-not (pcall core.disj! [1] 1))
    (assert-eq (core.disj! (core.transient (core.hash-set))) (core.hash-set))
    (assert-eq (core.disj! (core.transient (core.hash-set 1 3 2 5)) 3) {})
    (assert-eq (. (core.disj! (core.transient (core.hash-set 1 3 2 5)) 3) 2) 2)
    (assert-eq (. (core.disj! (core.transient (core.hash-set 1 3 2 5)) 3) 3) nil))

  (testing "transient pop!"
    (assert-not (pcall core.pop!))
    (assert-is (core.pop! (core.transient (core.vec [1]))))
    (assert-not (pcall core.pop! [1] 1))
    (assert-not (pcall core.pop! (core.transient (core.vector))))
    (assert-eq (core.pop! (-> (core.transient (core.vector))
                              (core.conj! 1)
                              (core.conj! 2)
                              (core.conj! 3)))
               [1 2]))

  (testing "transient can't be modified after persisted"
    (let [t (core.transient (core.vector))]
      (core.persistent! t)
      (assert-not (pcall core.conj! t 10))
      (assert-not (pcall #(tset $1 (length $1) $2) t 10)))
    (let [t (core.transient (core.hash-map))]
      (core.persistent! t)
      (assert-not (pcall core.conj! t 10))
      (assert-not (pcall #(tset $1 (length $1) $2) t 10)))
    (let [t (core.transient (core.hash-set))]
      (core.persistent! t)
      (assert-not (pcall core.conj! t 10))
      (assert-not (pcall #(tset $1 (length $1) $2) t 10)))))

(deftest test-into
  (testing "into"
    (assert-eq (core.into [] nil) [])
    (assert-eq (core.into nil nil) nil)
    (assert-eq (core.into nil [1 2 3]) (core.list 3 2 1))

    (assert-eq (core.into [] []) [])
    (assert-eq (core.into [1 2 3] []) [1 2 3])
    (assert-eq (core.into [1 2 3] [4 5 6]) [1 2 3 4 5 6])

    (assert-eq (core.into {} {}) {})
    (assert-eq (core.into {:a 1} {}) {:a 1})
    (assert-eq (core.into {:a 1} {:b 2}) {:a 1 :b 2})

    ;; different bodies are being used at compile time so worth testing
    (assert-eq (core.into [] {}) [])
    (assert-eq (core.into {} []) [])
    (assert-eq (. (getmetatable (core.into (core.vector) {})) :cljlib/type) :vector)
    (assert-eq (. (getmetatable (core.into (core.hash-map) [])) :cljlib/type) :hash-map)
    (let [a (core.vector)]
      (assert-eq (. (getmetatable (core.into a a)) :cljlib/type) :vector))

    ;; can't transform table with more than one key-value pair, as order
    ;; is undeterminitive
    (assert-eq (core.into [] {:a 1}) [[:a 1]])
    (assert-eq (core.into [[:b 2]] {:a 1}) [[:b 2] [:a 1]])
    (assert-eq (core.into [[:c 3]] {}) [[:c 3]])

    (assert-eq (core.into (core.hash-map) [[:c 3] [:a 1] [:b 2]]) {:a 1 :b 2 :c 3})
    (assert-eq (core.into {:d 4} [[:c 3] [:a 1] [:b 2]]) {:a 1 :b 2 :c 3 :d 4})
    (assert-eq (core.into {:a 0 :b 0 :c 0} [[:c 3] [:a 1] [:b 2]]) {:a 1 :b 2 :c 3})

    (let [a (fn [] {:a 1})
          b (fn [] [[:b 2]])]
      (assert-eq (core.into (a) (b)) {:a 1 :b 2})
      (assert-eq (core.into (b) (a)) [[:b 2] [:a 1]])
      (let [c []]
        (assert-eq (core.into c (b)) [[:b 2]]))
      (let [c []]
        (assert-eq (core.into c (a)) [[:a 1]]))
      (let [c []]
        (assert-eq (core.into (b) c) (b))
        (assert-eq (core.into (a) c) (a))))

    (let [a {}
          b []]
      (assert-eq (core.into a [1 2 3]) [1 2 3])
      (assert-eq (core.into b [1 2 3]) [1 2 3]))
    (let [a {}
          b []]
      (assert-eq (core.into b {:a 1}) [[:a 1]]))

    (let [a {}
          b []]
      (assert-eq (core.into a "vaiv") ["v" "a" "i" "v"])
      (when _G.utf8 (assert-eq (core.into b "ваыв") ["в" "а" "ы" "в"])))
    (assert-eq (core.into [] "vaiv") ["v" "a" "i" "v"])
    (when _G.utf8 (assert-eq (core.into [] "ваыв") ["в" "а" "ы" "в"]))))

(deftest transducers-test
  (testing "transduce"
    (assert-eq (core.transduce (core.map core.inc)
                               core.conj
                               (core.vector)
                               (core.vector 1 2 3))
               [2 3 4])
    (assert-eq (core.transduce (core.comp (core.map core.inc)
                                          (core.filter core.odd?))
                               core.conj
                               (core.vector)
                               (core.vector 1 2 3))
               [3]))
  (testing "sequence"
    (assert-eq (core.sequence (core.map core.inc)
                              (core.vector 1 2 3))
               (core.list 2 3 4))
    (assert-eq (core.sequence (core.comp (core.map core.inc)
                                         (core.filter core.odd?))
                              (core.vector 1 2 3))
               (core.list 3)))
  (testing "into"
    (assert-eq (core.into (core.vector)
                          (core.map core.inc)
                          (core.vector 1 2 3))
               [2 3 4])
    (assert-eq (core.into (core.vector)
                          (core.comp (core.map core.inc)
                                     (core.filter core.odd?))
                          (core.vector 1 2 3))
               [3])))

(deftest class-test
  (testing "class"
    (assert-eq (core.class (core.hash-set)) "hash-set")
    (assert-eq (core.class (core.hash-map)) "hash-map")
    (assert-eq (core.class (core.vector)) "vector")
    (assert-eq (core.class [1 2 3]) "table")))

(deftest deref-test
  (assert-not (pcall core.deref []))
  (assert-eq 10 (core.deref (core.reduced 10))))

(deftest function-manipulation-test
  (testing "constantly"
    (assert-not (pcall core.constantly))
    (assert-not (pcall core.constantly nil nil))
    (let [always-nil (core.constantly nil)]
      (assert-eq (always-nil) nil)
      (assert-eq (always-nil 1) nil)
      (assert-eq (always-nil 1 2 3 4 "5") nil))

    (let [always-true (core.constantly true)]
      (assert-is (always-true))
      (assert-is (always-true false))))

  (testing "complement"
    (assert-not (pcall core.complement))
    (assert-not (pcall core.complement #nil #nil))
    (assert-is ((core.complement #(do false))))
    (assert-is ((core.complement core.nil?) 10))
    (assert-is ((core.complement core.every?) core.double? [1 2 3 4]))
    (assert-is ((core.complement #(= $1 $2 $3)) 1 1 2 1))
    (assert-is ((core.complement #(= $1 $2)) 1 2)))

  (testing "apply"
    (assert-eq (core.apply core.add [1 2 3 4]) 10)
    (assert-eq (core.apply core.add -1 [1 2 3 4]) 9)
    (assert-eq (core.apply core.add -2 -1 [1 2 3 4]) 7)
    (assert-eq (core.apply core.add -3 -2 -1 [1 2 3 4]) 4)
    (assert-eq (core.apply core.add -4 -3 -2 -1 [1 2 3 4]) 0)
    (assert-eq (core.apply core.add -5 -4 -3 -2 -1 [1 2 3 4]) -5)
    (assert-eq (core.apply core.add -7 -6 -5 -4 -3 -2 -1 [1 2 3 4]) -18)
    (assert-not (pcall core.apply))
    (assert-not (pcall core.apply core.add)))

  (testing "comp"
    (assert-eq ((core.comp) 10) 10)
    (assert-eq ((core.comp #10)) 10)
    (fn square [x] (* x x))
    (assert-eq (core.comp square) square)
    (assert-eq ((core.comp square core.inc) 6) 49)
    (assert-eq ((core.comp #(- $ 7) square core.inc core.inc core.inc core.inc core.inc core.inc core.inc) 0) 42)
    (fn sum-squares [x y] (+ (* x x) (* y y)))
    (assert-eq ((core.comp square core.inc sum-squares) 2 3) 196)
    (fn f [a b c] (+ a b c))
    (assert-eq ((core.comp core.inc f) 1 2 3) 7)
    (fn g [a b c d] (+ a b c d))
    (assert-eq ((core.comp core.inc g) 1 2 3 4) 11)
    (fn h [a b c d e f] (+ a b c d e f))
    (assert-eq ((core.comp core.inc h) 1 2 3 4 5 6) 22))

  (testing "identity"
    (fn f [] nil)
    (local a {})
    (assert-not (pcall core.identity))
    (assert-not (pcall core.identity 1 2))
    (assert-eq (core.identity 1) 1)
    (assert-eq (core.identity {:a 1 :b 2}) {:a 1 :b 2})
    (assert-eq (core.identity [1 2 3]) [1 2 3])
    (assert-eq (core.identity "abc") "abc")
    (assert-eq (core.identity f) f)
    (assert-eq (core.identity a) a)))

(deftest test-sequence-predicates
  (testing "some"
    (assert-not (pcall core.some))
    (assert-not (pcall core.some core.pos-int?))
    (assert-is (core.some core.pos-int? [-1 1.1 2.3 -55 42 10 -27]))
    (assert-not (core.some core.pos-int? {:a 1}))
    (assert-is (core.some core.pos-int? [{:a 1} "1" -1 1])))

  (testing "not-any?"
    (assert-not (pcall core.not-any?))
    (assert-not (pcall core.not-any? core.pos-int?))
    (assert-is (core.not-any? core.pos-int? [-1 1.1 2.3 -55 -42 -10 -27]))
    (assert-is (core.not-any? core.pos-int? {:a 1}))
    (assert-not (core.not-any? core.pos-int? [1 2 3 4 5])))

  (testing "every?"
    (assert-not (pcall core.every?))
    (assert-not (pcall core.every? core.pos-int?))
    (assert-not (core.every? core.pos-int? [-1 1.1 2.3 -55 42 10 -27]))
    (assert-not (core.every? core.pos-int? {:a 1}))
    (assert-is (core.every? core.pos-int? [1 2 3 4 5])))

  (testing "empty?"
    (assert-not (pcall core.empty?))
    (assert-is (core.empty? []))
    (assert-is (core.empty? {}))
    (assert-is (core.empty? ""))
    (assert-not (core.empty? "1"))
    (assert-not (core.empty? [1]))
    (assert-not (core.empty? {:a 1}))
    (assert-not (pcall core.empty? 10)))

  (testing "not-empty"
    (assert-not (pcall core.not-empty))
    (assert-eq (core.not-empty []) nil)
    (assert-eq (core.not-empty {}) nil)
    (assert-eq (core.not-empty "") nil)
    (assert-eq (core.not-empty "1") "1")
    (assert-eq (core.not-empty [1]) [1])
    (assert-eq (core.not-empty {:a 1}) {:a 1})))

(deftest test-math-functions
  (testing "inc"
    (assert-eq (core.inc 1) 2)
    (assert-eq (core.inc -1) 0)
    (assert-not (pcall core.inc))
    (assert-not (pcall core.inc nil)))

  (testing "dec"
    (assert-eq (core.dec 1) 0)
    (assert-eq (core.dec -1) -2)
    (assert-not (pcall core.dec))
    (assert-not (pcall core.dec nil))))

(deftest test-table-access
  (testing "get"
    (assert-eq (core.get {:key1 10 :key2 20} :key1) 10)
    (assert-eq (core.get {:key1 10 :key2 20} :key1 false) 10)
    (assert-eq (core.get {:key1 10 :key2 20} :key3 false) false)
    (assert-eq (core.get {:key1 10 :key2 20} :key3) nil)
    (assert-not (pcall core.get))
    (assert-not (pcall core.get {})))

  (testing "get-in"
    (local t {:a {:b {:c 10}}})
    (assert-eq (core.get-in t [:a]) {:b {:c 10}})
    (assert-eq (core.get-in t [:a :b]) {:c 10})
    (assert-eq (core.get-in t [:a :b :c]) 10)
    (assert-eq (core.get-in t [:a :b :c] false) 10)
    (assert-eq (core.get-in t [:a :b :d] false) false)
    (assert-eq (core.get-in t [:a :b :d]) nil)
    (assert-eq (core.get-in t []) t)
    (assert-not (pcall core.get-in))
    (assert-not (pcall core.get-in {}))))

(deftest test-methods
  (testing "methods"
    (clj.defmulti f core.identity)
    (clj.defmethod f :a [_] :a)
    (clj.defmethod f :b [_] :b)
    (clj.defmethod f :c [x] (* x x))
    (assert-eq (core.methods f) f)
    (assert-not (pcall core.methods))
    (assert-not (pcall core.methods []))
    (assert-not (pcall core.methods f f)))

  (testing "get-method"
    (clj.defmulti f core.identity)
    (clj.defmethod f :a [_] :a)
    (clj.defmethod f :b [_] :b)
    (clj.defmethod f :c [x] (* x x))
    (assert-eq ((core.get-method f :a) 10) :a)
    (assert-eq ((core.get-method f :b) 20) :b)
    (assert-eq ((core.get-method f :c) 4) 16)
    (assert-not (pcall core.get-method))
    (assert-not (pcall core.get-method []))
    (assert-not (pcall core.get-method [] :a))
    (assert-not (pcall core.get-method f))
    (assert-not (pcall core.get-method f :a :b)))

  (testing "remove-method"
    (clj.defmulti f core.identity)
    (clj.defmethod f :a [_] :a)
    (clj.defmethod f :b [_] :b)
    (core.remove-method f :a)
    (assert-eq (core.get-method f :a) nil)
    (clj.defmethod f :default [_] :default)
    (assert-eq (core.get-method f :a) (core.get-method f :default))
    (assert-not (pcall core.remove-method))
    (assert-not (pcall core.remove-method []))
    (assert-not (pcall core.remove-method [] :a))
    (assert-not (pcall core.remove-method f))
    (assert-not (pcall core.remove-method f :a :b)))

  (testing "remove-all-methods"
    (clj.defmulti f core.identity)
    (clj.defmethod f :a [_] :a)
    (clj.defmethod f :b [_] :b)
    (clj.defmethod f :default [_] :default)
    (core.remove-all-methods f)
    (assert-eq (core.methods f) {})
    (assert-not (pcall core.remove-all-methods))
    (assert-not (pcall core.remove-all-methods []))
    (assert-not (pcall core.remove-all-methods f f))))

(deftest test-math-functions
  (testing "add"
    (assert-eq (core.add) 0)
    (assert-eq (core.add 1) 1)
    (assert-eq (core.add -1) -1)
    (assert-eq (core.add 1 2) 3)
    (assert-eq (core.add 1 2 3) 6)
    (assert-eq (core.add 1 2 3 4) 10)
    (assert-eq (core.add 1 2 3 4 5) 15))

  (testing "sub"
    (assert-eq (core.sub) 0)
    (assert-eq (core.sub 1) -1)
    (assert-eq (core.sub -1) 1)
    (assert-eq (core.sub 1 2) -1)
    (assert-eq (core.sub 1 2 3) -4)
    (assert-eq (core.sub 1 2 3 4) -8)
    (assert-eq (core.sub 1 2 3 4 5) -13))

  (testing "mul"
    (assert-eq (core.mul) 1)
    (assert-eq (core.mul 1) 1)
    (assert-eq (core.mul -1) -1)
    (assert-eq (core.mul 1 2) 2)
    (assert-eq (core.mul 1 2 3) 6)
    (assert-eq (core.mul 1 2 3 4) 24)
    (assert-eq (core.mul 1 2 3 4 5) 120))

  (testing "div"
    (assert-not (pcall core.div))
    (assert-eq (core.div 1) 1)
    (assert-eq (core.div -1) -1)
    (assert-eq (core.div 1 2) (/ 1 2))
    (assert-eq (core.div 1 2 3) (/ 1 2 3))
    (assert-eq (core.div 1 2 3 4) (/ 1 2 3 4))
    (assert-eq (core.div 1 2 3 4 5) (/ 1 2 3 4 5))))

(deftest test-comparison-functions
  (testing "le"
    (assert-not (pcall core.le))
    (assert-is (core.le 1))
    (assert-is (core.le 1 2))
    (assert-is (core.le 1 2 2))
    (assert-is (core.le 1 2 3 4))
    (assert-not (core.le 2 1))
    (assert-not (core.le 2 2 1))
    (assert-not (core.le 2 1 3))
    (assert-not (core.le 1 2 4 3)))

  (testing "lt"
    (assert-not (pcall core.lt))
    (assert-is (core.lt 1))
    (assert-is (core.lt 1 2))
    (assert-is (core.lt 1 2 3))
    (assert-is (core.lt 1 2 3 4))
    (assert-not (core.lt 2 1))
    (assert-not (core.lt 2 2 1))
    (assert-not (core.lt 2 1 3))
    (assert-not (core.lt 1 2 4 4)))

  (testing "ge"
    (assert-not (pcall core.ge))
    (assert-is (core.ge 2))
    (assert-is (core.ge 2 1))
    (assert-is (core.ge 3 3 2))
    (assert-is (core.ge 4 3 2 -1))
    (assert-not (core.ge 1 2))
    (assert-not (core.ge 1 1 2))
    (assert-not (core.ge 2 1 3))
    (assert-not (core.ge 1 2 4 4)))

  (testing "gt"
    (assert-not (pcall core.gt))
    (assert-is (core.gt 2))
    (assert-is (core.gt 2 1))
    (assert-is (core.gt 3 2 1))
    (assert-is (core.gt 4 3 2 -1))
    (assert-not (core.gt 1 2))
    (assert-not (core.gt 1 1 2))
    (assert-not (core.gt 2 1 3))
    (assert-not (core.gt 1 2 4 4))))

(deftest test-vector
  (testing "vector"
    (assert-eq (core.vector) [])
    (assert-eq (core.vector 1) [1])
    (assert-eq (core.vector 1 2 3) [1 2 3])
    (assert-eq (. (getmetatable (core.vector 1 2 3)) :cljlib/type) :vector)))

(deftest test-hash-map
  (testing "hash-map"
    (assert-not (pcall core.hash-map :a))
    (assert-eq (core.hash-map) {})
    (assert-eq (core.hash-map :a 1) {:a 1})
    (assert-eq (core.hash-map :a 1 :b 2 :c 3) {:a 1 :b 2 :c 3})
    (assert-eq (. (getmetatable (core.hash-map)) :cljlib/type) :hash-map)
    (assert-not (pcall core.hash-map nil 1))))

(deftest test-sets
  (testing "hash-set"
    (let [h1 (core.hash-set [1] [1] [2] [3] [:a])
          h2 (core.hash-set [1] [2] [3] [:a])]
      (assert-is (core.eq h1 h2)))

    (let [h3 (core.hash-set [1] [1] [2] [3] [:a])
          h4 (core.hash-set [1] [1] [3] [:a])]
      (assert-not (core.eq h3 h4)))

    (assert-eq (. (core.hash-set [1]) [1]) [1])
    (assert-eq (. (core.hash-set [1]) [2]) nil)
    (assert-eq ((core.hash-set [1]) [1]) [1])
    (assert-eq ((core.hash-set [1]) [2]) nil))

  (comment testing "ordered-set"
    (let [o1 (ordered-set [1] [1] [2] [3] [:a])
          o2 (ordered-set [1] [2] [3] [:a])]
      (assert-eq o1 o2))

    (let [o3 (ordered-set [1] [1] [2] [3] [:a])
          o4 (ordered-set [2] [1] [1] [3] [:a])]
      (assert-eq o3 o4))

    (assert-eq (. (ordered-set [1]) [1]) [1])
    (assert-eq ((ordered-set [1]) [1]) [1])
    (assert-eq (. (ordered-set [1]) [2]) nil)
    (assert-eq ((ordered-set [1]) [2]) nil))

  (testing "set equality"
    (let [o1 (core.hash-set [1] [[-1 0] 1] [2] [3] [:a] :a 2)
          h1 (core.hash-set [1] [[-1 0] 1] [2] [3] [:a] :a 2)]
      (assert-is (core.eq o1 h1)))

    (let [o2 (core.hash-set [1] [[-1 0] 1] [2] [3] [:a] :a 2)
          h2 (core.hash-set [1] [[-1 1] 1] [2] [3] [:a] :a 2)]
      (assert-not (core.eq o2 h2)))

    (let [o3 (core.hash-set [1] [[-1 0] 1] [2] [3] [:a] :a 2)
          h3 (core.hash-set [1] [[-1 0] 1] [2] [3] [:a] :a 2)]
      (assert-is (core.eq (core.disj o3 [2]) (core.disj h3 [2])))
      (assert-not (core.eq (core.disj o3 :a) h3)))

    (let [o4 (core.hash-set [1] [[-1 5] 1] [3] [:a] :a 2)
          h4 (core.hash-set [1] [[-1 5] 1] [2] [3] [:a] :a 2)]
      (assert-is (core.eq (core.conj o4 [2]) (core.conj (core.disj h4 [2]) [2])))))

  (testing "empty sets"
    (assert-eq (core.empty (core.hash-set)) (core.hash-set))
    (assert-eq (core.empty (core.hash-set 1 2 3)) (core.hash-set))
    (assert-eq (. (getmetatable (core.empty (core.hash-set))) :cljlib/type) :hash-set)

    (assert-eq (core.empty (core.hash-set)) (core.hash-set))
    (assert-eq (core.empty (core.hash-set 1 2 3)) (core.hash-set))
    (assert-eq (. (getmetatable (core.empty (core.hash-set))) :cljlib/type) :hash-set))

  (testing "into sets"
    (assert-eq (core.into (core.hash-set) [1 2 3]) (core.hash-set 1 2 3))
    (assert-eq (core.into (core.hash-set) {:a 1 :b 2}) (core.hash-set [:a 1] [:b 2]))
    (assert-eq (core.into (core.hash-set) "vaiv") (core.hash-set "v" "a" "i" "v"))
    (assert-eq (core.into (core.hash-set) [1 2 3]) (core.hash-set 1 2 3))
    (assert-eq (core.into (core.hash-set) {:a 1 :b 2}) (core.hash-set [:a 1] [:b 2]))
    (assert-eq (core.into (core.hash-set) "vaiv") (core.hash-set "v" "a" "i" "v")))

  (testing "sets into tables"
    (assert-eq (core.into (core.vector) (core.hash-set 1 2 3)) [1 2 3])
    (assert-eq (core.into (core.vector) (core.hash-set :a)) [:a])
    (assert-eq (core.into (core.hash-map) (core.hash-set [:a 1] [:b 2])) {:a 1 :b 2})))

(deftest test-memoization
  (testing "memoize"
    (macro time [expr]
      `(let [clock# os.clock
             start# (clock#)
             res# ,expr
             end# (clock#)]
         (values res# (* 1000 (- end# start#)))))

    (fn sleep [ms]
      (let [clock os.clock
            end (+ (clock) (/ ms 1000))]
        (while (< (clock) end) nil)))

    (fn slow [x] (sleep 100) x)

    (assert-not (pcall core.memoize))
    (assert-not (pcall core.memoize slow 2))

    (local fast (core.memoize slow))

    (let [(res1 time1) (time (fast 42))
          (res2 time2) (time (fast 42))]
      (assert-is (core.eq res1 res2 42))
      (assert-is (< time2 time1)))

    (let [(res1 time1) (time (fast [10]))
          (res2 time2) (time (fast [10]))]
      (assert-is (core.eq res1 res2 [10]))
      (assert-is (< time2 time1)))

    (let [(res1 time1) (time (fast {[[1] [2 [3]]] {:a 2} {{:a 1} {:b 1}} {{:c 3} {:d 4}}}))
          (res2 time2) (time (fast {[[1] [2 [3]]] {:a 2} {{:a 1} {:b 1}} {{:c 3} {:d 4}}}))]
      (assert-is (core.eq res1 res2 {[[1] [2 [3]]] {:a 2} {{:a 1} {:b 1}} {{:c 3} {:d 4}}}))
      (assert-is (< time2 time1)))))
