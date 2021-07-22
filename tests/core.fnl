(require-macros :init-macros)
(require-macros :fennel-test.test)

(macro require-module [module]
  `(local ,(collect [k (pairs (require module))]
             (values k (sym k)))
          (require ,module)))

(require-module :init)

(deftest equality
  (testing "comparing base-types"
    (assert-not (pcall eq))
    (assert-eq 1 1)
    (assert-ne 1 2)
    (assert-is (eq 1 1 1 1 1))
    (assert-eq 1.0 1.0)
    (assert-is (eq 1.0 1.0 1.0))
    (assert-is (eq 1.0 1.0 1.0))
    (assert-is (eq "1" "1" "1" "1" "1")))

  (testing "deep comparison"
    (assert-is (eq []))
    (assert-eq [] [])
    (assert-eq [] {})
    (assert-eq [1 2] [1 2])
    (assert-eq [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]
               [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]])
    (assert-is (eq [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]
                   [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]))
    (assert-is (eq [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]
                   [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]
                   [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]))
    (assert-not (eq [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]
                    [[1 [2 [3]] {[5] {:a [1 [1 [1 [1]]]]}}]]
                    [[1 [2 [3]] {[6] {:a [1 [1 [1 [1]]]]}}]]))
    (assert-ne [1] [1 2])
    (assert-ne [1 2] [1])
    (assert-is (eq [1 [2]] [1 [2]] [1 [2]]))
    (assert-is (eq [1 [2]] [1 [2]] [1 [2]]))
    (assert-not (eq [1 [2]] [1 [2]] [1 [2 [3]]]))
    (assert-not (eq {:a {:b 2}} {:a {:b 2}} {:a {:b 3}}))

    (let [a {:a 1 :b 2}
          b {:a 1 :b 2}]
      (table.insert b 10)
      (assert-ne a b))

    (let [a [1 2 3]
          b [1 2 3]]
      (tset b :a 10)
      (assert-ne a b))

    (assert-eq [1 2 3] {1 1 2 2 3 3})
    (assert-eq {4 1} [nil nil nil 1])))

(deftest range
  (testing "range"
    (assert-not (pcall range))
    (assert-eq (range 10) [0 1 2 3 4 5 6 7 8 9])
    (assert-eq (range -5 5) [-5 -4 -3 -2 -1 0 1 2 3 4])
    (assert-eq [0 0.2 0.4 0.6 0.8] [0 0.2 0.4 0.6 0.8])
    (assert-eq (range 0 1 0.2) (range 0 1 0.2))))

(deftest predicates
  (testing "zero?"
    (assert-is (zero? 0))
    (assert-is (zero? -0))
    (assert-not (zero? 1))
    (assert-not (pcall zero?))
    (assert-not (pcall zero? 1 2)))

  (testing "int?"
    (assert-is (int? 1))
    (assert-not (int? 1.1))
    (assert-not (pcall int?))
    (assert-not (pcall int? 1 2)))

  (testing "pos?"
    (assert-is (pos? 1))
    (assert-is (and (not (pos? 0)) (not (pos? -1))))
    (assert-not (pcall pos?))
    (assert-not (pcall pos? 1 2)))

  (testing "neg?"
    (assert-is (neg? -1))
    (assert-is (and (not (neg? 0)) (not (neg? 1))))
    (assert-not (pcall neg?))
    (assert-not (pcall neg? 1 2)))

  (testing "pos-int?"
    (assert-is (pos-int? 42))
    (assert-not (pos-int? 4.2))
    (assert-not (pcall pos-int?))
    (assert-not (pcall pos-int? 1 2)))

  (testing "neg-int?"
    (assert-is (neg-int? -42))
    ;; (assert-not (neg-int? -4.2))
    (assert-not (pcall neg-int?))
    (assert-not (pcall neg-int? 1 2)))

  (testing "string?"
    (assert-is (string? :s))
    (assert-not (pcall string?))
    (assert-not (pcall string? 1 2)))

  (testing "double?"
    (assert-is (double? 3.3))
    (assert-not (double? 3.0))
    (assert-not (pcall double?))
    (assert-not (pcall double? 1 2)))

  (testing "map?"
    (assert-is (map? {:a 1}))
    (assert-not (map? {}))
    (assert-is (map? (empty {})))
    (assert-not (map? (empty [])))
    (assert-not (pcall map?))
    (assert-not (pcall map? 1 2)))

  (testing "vector?"
    (assert-not (vector? []))
    (assert-is (vector? [{:a 1}]))
    (assert-not (vector? {}))
    (assert-not (vector? {:a 1}))
    (assert-is (vector? (empty [])))
    (assert-not (vector? (empty {})))
    (assert-not (pcall vector?))
    (assert-not (pcall vector? 1 2)))

  (testing "multifn?"
    (assert-not (multifn? []))
    (assert-is (multifn? (do (defmulti f identity) f)))
    (assert-not (pcall multifn?))
    (assert-not (pcall multifn? 1 2)))

  (testing "set?"
    (assert-is (set? (ordered-set)))
    (assert-is (set? (hash-set)))
    (assert-eq (set? (hash-set)) :cljlib/hash-set)
    (assert-eq (set? (ordered-set)) :cljlib/ordered-set)
    (assert-not (pcall set?))
    (assert-not (pcall set? 1 2)))

  (testing "nil?"
    (assert-is (nil?))
    (assert-is (nil? nil))
    (assert-not (nil? 1))
    (assert-not (pcall nil? 1 2)))

  (testing "odd?"
    (assert-is (odd? 3))
    (assert-is (odd? -3))
    (assert-not (odd? 2))
    (assert-not (odd? -2))
    (assert-not (pcall odd?))
    (assert-not (pcall odd? 1 2)))

  (testing "even?"
    (assert-is (even? 2))
    (assert-is (even? -2))
    (assert-not (even? 23))
    (assert-not (even? -23))
    (assert-not (pcall even?))
    (assert-not (pcall even? 1 2)))

  (testing "true?"
    (assert-is (true? true))
    (assert-not (true? false))
    (assert-not (true? 10))
    (assert-not (true? :true))
    (assert-not (pcall true?))
    (assert-not (pcall true? 1 2)))

  (testing "false?"
    (assert-is (false? false))
    (assert-not (false? true))
    (assert-not (false? 10))
    (assert-not (false? :true))
    (assert-not (pcall false?))
    (assert-not (pcall false? 1 2)))

  (testing "boolean?"
    (assert-is (boolean? true))
    (assert-is (boolean? false))
    (assert-not (boolean? :false))
    (assert-not (boolean? (fn [] true)))
    (assert-not (pcall boolean?))
    (assert-not (pcall boolean? 1 2))))

(deftest sequence-functions
  (testing "seq"
    (assert-not (pcall seq))
    (assert-not (pcall seq [] []))
    (assert-eq (seq []) nil)
    (assert-eq (seq {}) nil)
    (assert-eq (seq [1]) [1])
    (assert-eq (seq [1 2 3]) [1 2 3])
    (assert-eq (seq {:a 1}) [["a" 1]])
    (assert-eq (seq "abc") ["a" "b" "c"])
    (when _G.utf8 (assert-eq (seq "абв") ["а" "б" "в"]))
    (assert-eq (seq {12345 123}) [[12345 123]])
    (assert-eq (seq (ordered-set 1 2 3)) [1 2 3])
    (assert-eq (length (seq (ordered-set 1 2 3))) 3)
    (assert-eq (seq (hash-set 1)) [1])
    (assert-eq (length (seq (hash-set 1 2 3))) 3))

  (testing "kvseq"
    (assert-not (pcall kvseq))
    (assert-not (pcall kvseq [] []))
    (assert-eq (kvseq nil) nil)
    (assert-eq (kvseq []) nil)
    (assert-eq (kvseq {123 456}) [[123 456]])
    (assert-eq (kvseq {:a 1}) [[:a 1]])
    (assert-eq (kvseq [0 0 0 10]) [[1 0] [2 0] [3 0] [4 10]])
    (assert-eq (kvseq (ordered-set :a :b :c)) [[:a :a] [:b :b] [:c :c]])
    (assert-eq (kvseq (hash-set :a)) [[:a :a]])
    (assert-eq (kvseq "abc") [[1 "a"] [2 "b"] [3 "c"]]))

  (testing "mapv"
    (assert-not (pcall mapv))
    (assert-not (pcall mapv #(do nil)))
    (assert-eq (mapv #(* $ $) [1 2 3 4]) [1 4 9 16])

    (assert-eq (into {} (mapv (fn [[k v]] [k (* v v)]) {:a 1 :b 2 :c 3}))
               (into {} [[:a 1] [:b 4] [:c 9]]))

    (assert-eq (into {} (mapv (fn [[k1 v1] [k2 v2]] [k1 (* v1 v2)])
                              {:a 1 :b 2 :c 3}
                              {:a -1 :b 0 :c 2}))
               {:a -1 :b 0 :c 6})
    (assert-eq (mapv #(* $1 $2 $3) [1] [2] [-1]) [-2])
    (assert-eq (mapv string.upper ["a" "b" "c"]) ["A" "B" "C"])
    (assert-eq (mapv #(+ $1 $2 $3 $4) [1 -1] [2 -2] [3 -3] [4 -4]) [(+ 1 2 3 4) (+ -1 -2 -3 -4)])
    (assert-eq (mapv (fn [f-name s-name company position]
                       (.. f-name " " s-name " works as " position " at " company))
                     ["Bob" "Alice"]
                     ["Smith" "Watson"]
                     ["Happy Days co." "Coffee With You"]
                     ["secretary" "chief officer"])
               ["Bob Smith works as secretary at Happy Days co."
                "Alice Watson works as chief officer at Coffee With You"])
    (assert-eq (table.concat (mapv string.upper "vaiv")) "VAIV"))

  (testing "partition"
    (assert-not (pcall partition))
    (assert-not (pcall partition 1))
    (assert-eq (partition 1 [1 2 3 4]) [[1] [2] [3] [4]])
    (assert-eq (partition 1 2 [1 2 3 4]) [[1] [3]])
    (assert-eq (partition 3 2 [1 2 3 4 5]) [[1 2 3] [3 4 5]])
    (assert-eq (partition 3 3 [0 -1 -2 -3] [1 2 3 4]) [[1 2 3] [4 0 -1]]))

  (testing "nthrest"
    (assert-not (pcall nthrest))
    (assert-not (pcall nthrest []))
    (assert-eq (nthrest [1 2 3] 0) [1 2 3])
    (assert-eq (nthrest [1 2 3] 1) [2 3])
    (assert-eq (nthrest [1 2 3] 2) [3])
    (assert-eq (nthrest [1 2 3] 3) []))

  (testing "take"
    (assert-not (pcall take))
    (assert-not (pcall take []))
    (assert-not (pcall take :a []))
    (assert-not (pcall take -1 []))
    (assert-eq (take 0 [1 2 3]) [])
    (assert-eq (take 1 {:a 1}) [[:a 1]])
    (assert-eq (take 10 [1 2 3]) [1 2 3])
    (assert-eq (take 1 [1 2 3]) [1]))

  (testing "reduce"
    (fn* add
      ([] 0)
      ([a] a)
      ([a b] (+ a b))
      ([a b & c]
       (var res (+ a b))
       (each [_ v (ipairs c)]
         (set res (+ res v)))
       res))

    (assert-eq (reduce add []) 0)
    (assert-eq (reduce add [1]) 1)
    (assert-eq (reduce add [1 2]) 3)
    (assert-eq (reduce add (range 10)) 45)
    (assert-eq (reduce add -3 (range 10)) 42)
    (assert-eq (reduce add 10 []) 10)
    (assert-eq (reduce add 10 [1]) 11)
    (assert-eq (reduce add 10 nil) 10)
    (assert-not (pcall reduce))
    (assert-not (pcall reduce add)))

  (testing "reduce reference implementation"
    (fn mapping [f]
      (fn [reducing]
        (fn [result input]
          (reducing result (f input)))))

    (fn -reduce [f init [x & tbl]]
      (if x (-reduce f (f init x) tbl) init))

    (assert-eq (reduce add (range 10)) (-reduce add 0 (range 10)))
    (assert-eq (reduce ((mapping inc) add) 0 (range 10))
               (-reduce ((mapping inc) add) 0 (range 10))))

  (testing "filter"
    (assert-not (pcall filter))
    (assert-not (pcall filter even?))
    (assert-eq (filter even? (range 10)) [0 2 4 6 8])
    (assert-eq (filter odd? (range 10)) [1 3 5 7 9])
    (assert-eq (filter map? [{:a 1} {5 1} [1 2] [] {}]) [{:a 1} {5 1}])
    (assert-eq (filter vector? [{:a 1} {5 1} [1 2] [] {}]) [[1 2]]))

  (testing "concat"
    (assert-eq (concat) nil)
    (assert-eq (concat []) [])
    (assert-eq (concat [1 2 3]) [1 2 3])
    (assert-eq (concat [1 2 3] [4 5 6]) [1 2 3 4 5 6])
    (assert-eq (concat [1 2] [3 4] [5 6]) [1 2 3 4 5 6])
    (assert-eq (concat {:a 1} {:b 2}) [[:a 1] [:b 2]])
    (assert-eq (concat [[:a 1]] {:b 2}) [[:a 1] [:b 2]])
    (assert-eq (concat {:a 1} [[:b 2]]) [[:a 1] [:b 2]])
    (assert-eq (concat [] [[:b 2]]) [[:b 2]])
    (assert-eq (concat [] []) [])
    (assert-not (pcall concat 1))
    (assert-not (pcall concat 1 2))
    (assert-not (pcall concat 1 []))
    (assert-not (pcall concat [] 2))
    (assert-not (pcall concat [1] 2)))

  (testing "reverse"
    (assert-not (pcall reverse))
    (assert-not (pcall reverse [] []))
    (assert-eq (reverse []) nil)
    (assert-eq (reverse [1 2 3]) [3 2 1])
    (assert-eq (reverse {:a 1}) [[:a 1]]))

  (testing "conj"
    (assert-eq (conj) [])
    (assert-eq (conj [1]) [1])
    (assert-eq (conj [] 1 2 3) [1 2 3])
    (assert-eq (conj [0] 1 2 3) [0 1 2 3])
    (assert-eq (conj {:a 1} [:b 2]) {:a 1 :b 2})
    (assert-eq (conj {:a 1}) {:a 1})
    (assert-eq (conj [1] 2 3 4 5 6 7) [1 2 3 4 5 6 7]))

  (testing "disj"
    (assert-not (pcall disj))
    (assert-not (pcall disj [1]))
    (assert-not (pcall disj [1] 1))
    (assert-eq (disj (ordered-set)) (ordered-set))
    (assert-eq (disj (ordered-set 1 3 2 5) 3) (ordered-set 1 2 5))
    (assert-eq (disj (ordered-set 1 3 2 5) 3 1 5) (ordered-set 2)))

  (testing "cons"
    (assert-not (pcall cons))
    (assert-not (pcall cons [] [] []))
    (assert-eq (cons nil [1]) [1])
    (assert-eq (cons 1 []) [1])
    (assert-eq (cons 1 [0]) [1 0]))

  (testing "first"
    (assert-not (pcall first))
    (assert-not (pcall first [] []))
    (assert-eq (first [1 2 3]) 1)
    (assert-eq (first {:a 1}) [:a 1])
    (assert-eq (first []) nil))

  (testing "last"
    (assert-not (pcall last))
    (assert-not (pcall last [] []))
    (assert-eq (last [1 2 3]) 3)
    (assert-eq (last []) nil)
    (assert-eq (last nil) nil)
    (assert-eq (last {:a 1}) [:a 1]))

  (testing "rest"
    (assert-not (pcall rest))
    (assert-not (pcall rest [] []))
    (assert-eq (rest [1 2 3]) [2 3])
    (assert-eq (rest {:a 1}) [])
    (assert-eq (rest []) [])
    (assert-eq (rest nil) []))

  (testing "butlast"
    (assert-not (pcall butlast))
    (assert-not (pcall butlast [] []))
    (assert-eq (butlast [1 2 3]) [1 2])
    (assert-eq (butlast {:a 1}) nil)
    (assert-eq (butlast []) nil)
    (assert-eq (butlast nil) nil))

  (testing "reduce-kv"
    (assert-eq (reduce-kv #(+ $1 $3) 0 {:a 1 :b 2 :c 3}) 6)
    (assert-eq (reduce-kv #(+ $1 $3) 0 [1 2 3]) 6)
    (assert-not (pcall reduce-kv #(+ $1 $3) 0))
    (assert-not (pcall reduce-kv #(+ $1 $3)))
    (assert-not (pcall reduce-kv)))

  (testing "reduced"
    (assert-not (pcall reduced))
    (assert-not (pcall reduced 1 2 3))
    (assert-eq (reduce #(if (> $1 10) (reduced -1) (+ $1 $2)) [1]) 1)
    (assert-eq (reduce #(if (> $1 10) (reduced -1) (+ $1 $2)) [1 2]) 3)
    (assert-eq (reduce #(if (> $1 10) (reduced -1) (+ $1 $2)) [1 2 3 4]) 10)
    (assert-eq (reduce #(if (> $1 10) (reduced -1) (+ $1 $2)) [1 2 3 4 5]) 15)
    (assert-eq (reduce #(if (> $1 10) (reduced -1) (+ $1 $2)) [1 2 3 4 5 6]) -1)
    (assert-eq (reduce #(if (> $1 10) (reduced -1) (+ $1 $2)) 10 [1]) 11)
    (assert-eq (reduce #(if (> $1 10) (reduced -1) (+ $1 $2)) 10 [1 2]) -1)
    (assert-eq (reduce #(if (> $1 10) (reduced -1) (+ $1 $2)) 0 [10 5]) 15)
    (assert-eq (reduce #(if (> $1 10) (reduced -1) (+ $1 $2)) 1 [10 7]) -1)

    (assert-eq (reduce #(if (> $1 10) (reduced false) (+ $1 $2)) 1 [10 7]) false)
    (assert-eq (reduce #(if (> $1 10) (reduced nil) (+ $1 $2)) 1 [10 7]) nil)

    (assert-eq (reduce-kv (fn [res _ v] (if (> res 10) (reduced -1) (+ res v))) 0 {:a 1 :b 2}) 3)
    (assert-eq (reduce-kv (fn [res _ v] (if (> res 10) (reduced -1) (+ res v))) 0 {:a 10 :b 2}) 12)
    (assert-eq (reduce-kv (fn [res _ v] (if (> res 10) (reduced -1) (+ res v))) 1 {:a 3 :b 3 :c 3 :d 3}) 13)
    (assert-eq (reduce-kv (fn [res _ v] (if (> res 10) (reduced -1) (+ res v))) 2 {:a 3 :b 3 :c 3 :d 3}) -1)
    (assert-eq (reduce-kv (fn [res _ v] (if (> res 10) (reduced -1) (+ res v))) 1 [10 12]) -1)

    (assert-eq (reduce-kv (fn [res _ v] (if (> res 10) (reduced false) (+ res v))) 1 [10 12]) false)
    (assert-eq (reduce-kv (fn [res _ v] (if (> res 10) (reduced nil) (+ res v))) 1 [10 12]) nil))

  (testing "assoc"
    (assert-not (pcall assoc))
    (assert-not (pcall assoc {}))
    (assert-eq (assoc {} :a 1) {:a 1})
    (assert-eq (assoc {} :a 1 :b 2 :c 3 :d 4) {:a 1 :b 2 :c 3 :d 4}))

  (testing "dissoc"
    (assert-not (pcall dissoc))
    (assert-eq (dissoc {}) {})
    (assert-eq (dissoc {:a 1 :b 2} :b) {:a 1})
    (assert-eq (dissoc {:a 1 :b 2 :c 3} :a :c) {:b 2}))

  (testing "find, keys and vals"
    (assert-not (pcall keys))
    (assert-not (pcall keys {} {} {}))
    (assert-not (pcall vals))
    (assert-not (pcall vals {} {} {}))
    (assert-not (pcall find))
    (assert-not (pcall find {} {} {}))
    (assert-eq (keys {:a 1}) [:a])
    (assert-eq (vals {:a 1}) [1])
    (match (pcall #(assert-eq (keys {:a 1 :b 2 :c 3}) (hash-set :a :b :c)))
      (false msg) (io.stderr:write "WARNING: " msg))
    (match (pcall #(assert-eq (vals {:a 1 :b 2 :c 3}) (hash-set 1 2 3)))
      (false msg) (io.stderr:write "WARNING: " msg))
    (assert-eq (find {:a 1 :b 2 :c 3} :c) [:c 3])
    (assert-eq (find {:a 1 :b 2 :c 3} :d) nil)))


(deftest function-manipulation
  (testing "constantly"
    (assert-not (pcall constantly))
    (assert-not (pcall constantly nil nil))
    (let [always-nil (constantly nil)]
      (assert-eq (always-nil) nil)
      (assert-eq (always-nil 1) nil)
      (assert-eq (always-nil 1 2 3 4 "5") nil))

    (let [always-true (constantly true)]
      (assert-is (always-true))
      (assert-is (always-true false))))

  (testing "complement"
    (assert-not (pcall complement))
    (assert-not (pcall complement #nil #nil))
    (assert-is ((complement #(do false))))
    (assert-is ((complement nil?) 10))
    (assert-is ((complement every?) double? [1 2 3 4]))
    (assert-is ((complement #(= $1 $2 $3)) 1 1 2 1))
    (assert-is ((complement #(= $1 $2)) 1 2)))

  (testing "apply"
    (fn* add
      ([x] x)
      ([x y] (+ x y))
      ([x y & zs]
       (add (+ x y) ((or _G.unpack table.unpack) zs))))
    (assert-eq (apply add [1 2 3 4]) 10)
    (assert-eq (apply add -1 [1 2 3 4]) 9)
    (assert-eq (apply add -2 -1 [1 2 3 4]) 7)
    (assert-eq (apply add -3 -2 -1 [1 2 3 4]) 4)
    (assert-eq (apply add -4 -3 -2 -1 [1 2 3 4]) 0)
    (assert-eq (apply add -5 -4 -3 -2 -1 [1 2 3 4]) -5)
    (assert-not (pcall apply))
    (assert-not (pcall apply add)))

  (testing "comp"
    (assert-eq ((comp) 10) 10)
    (assert-eq ((comp #10)) 10)
    (fn square [x] (* x x))
    (assert-eq (comp square) square)
    (assert-eq ((comp square inc) 6) 49)
    (assert-eq ((comp #(- $ 7) square inc inc inc inc inc inc inc) 0) 42)
    (fn sum-squares [x y] (+ (* x x) (* y y)))
    (assert-eq ((comp square inc sum-squares) 2 3) 196)
    (fn f [a b c] (+ a b c))
    (assert-eq ((comp inc f) 1 2 3) 7)
    (fn g [a b c d] (+ a b c d))
    (assert-eq ((comp inc g) 1 2 3 4) 11)
    (fn h [a b c d e f] (+ a b c d e f))
    (assert-eq ((comp inc h) 1 2 3 4 5 6) 22))

  (testing "identity"
    (fn f [] nil)
    (local a {})
    (assert-not (pcall identity))
    (assert-not (pcall identity 1 2))
    (assert-eq (identity 1) 1)
    (assert-eq (identity {:a 1 :b 2}) {:a 1 :b 2})
    (assert-eq (identity [1 2 3]) [1 2 3])
    (assert-eq (identity "abc") "abc")
    (assert-eq (identity f) f)
    (assert-eq (identity a) a)))

(deftest sequence-predicates
  (testing "some"
    (assert-not (pcall some))
    (assert-not (pcall some pos-int?))
    (assert-is (some pos-int? [-1 1.1 2.3 -55 42 10 -27]))
    (assert-not (some pos-int? {:a 1}))
    (assert-is (some pos-int? [{:a 1} "1" -1 1])))

  (testing "not-any?"
    (assert-not (pcall not-any?))
    (assert-not (pcall not-any? pos-int?))
    (assert-is (not-any? pos-int? [-1 1.1 2.3 -55 -42 -10 -27]))
    (assert-is (not-any? pos-int? {:a 1}))
    (assert-not (not-any? pos-int? [1 2 3 4 5])))

  (testing "every?"
    (assert-not (pcall every?))
    (assert-not (pcall every? pos-int?))
    (assert-not (every? pos-int? [-1 1.1 2.3 -55 42 10 -27]))
    (assert-not (every? pos-int? {:a 1}))
    (assert-is (every? pos-int? [1 2 3 4 5])))

  (testing "empty?"
    (assert-not (pcall empty?))
    (assert-is (empty? []))
    (assert-is (empty? {}))
    (assert-is (empty? ""))
    (assert-not (empty? "1"))
    (assert-not (empty? [1]))
    (assert-not (empty? {:a 1}))
    (assert-not (pcall empty? 10)))

  (testing "not-empty"
    (assert-not (pcall not-empty))
    (assert-eq (not-empty []) nil)
    (assert-eq (not-empty {}) nil)
    (assert-eq (not-empty "") nil)
    (assert-eq (not-empty "1") "1")
    (assert-eq (not-empty [1]) [1])
    (assert-eq (not-empty {:a 1}) {:a 1})))

(deftest math-functions
  (testing "inc"
    (assert-eq (inc 1) 2)
    (assert-eq (inc -1) 0)
    (assert-not (pcall inc))
    (assert-not (pcall inc nil)))

  (testing "dec"
    (assert-eq (dec 1) 0)
    (assert-eq (dec -1) -2)
    (assert-not (pcall dec))
    (assert-not (pcall dec nil))))

(deftest table-access
  (testing "get"
    (assert-eq (get {:key1 10 :key2 20} :key1) 10)
    (assert-eq (get {:key1 10 :key2 20} :key1 false) 10)
    (assert-eq (get {:key1 10 :key2 20} :key3 false) false)
    (assert-eq (get {:key1 10 :key2 20} :key3) nil)
    (assert-not (pcall get))
    (assert-not (pcall get {})))

  (testing "get-in"
    (local t {:a {:b {:c 10}}})
    (assert-eq (get-in t [:a]) {:b {:c 10}})
    (assert-eq (get-in t [:a :b]) {:c 10})
    (assert-eq (get-in t [:a :b :c]) 10)
    (assert-eq (get-in t [:a :b :c] false) 10)
    (assert-eq (get-in t [:a :b :d] false) false)
    (assert-eq (get-in t [:a :b :d]) nil)
    (assert-eq (get-in t []) t)
    (assert-not (pcall get-in))
    (assert-not (pcall get-in {}))))

(deftest methods
  (testing "methods"
    (defmulti f identity)
    (defmethod f :a [_] :a)
    (defmethod f :b [_] :b)
    (defmethod f :c [x] (* x x))
    (assert-eq (methods f) f)
    (assert-not (pcall methods))
    (assert-not (pcall methods []))
    (assert-not (pcall methods f f)))

  (testing "get-method"
    (defmulti f identity)
    (defmethod f :a [_] :a)
    (defmethod f :b [_] :b)
    (defmethod f :c [x] (* x x))
    (assert-eq ((get-method f :a) 10) :a)
    (assert-eq ((get-method f :b) 20) :b)
    (assert-eq ((get-method f :c) 4) 16)
    (assert-not (pcall get-method))
    (assert-not (pcall get-method []))
    (assert-not (pcall get-method [] :a))
    (assert-not (pcall get-method f))
    (assert-not (pcall get-method f :a :b)))

  (testing "remove-method"
    (defmulti f identity)
    (defmethod f :a [_] :a)
    (defmethod f :b [_] :b)
    (remove-method f :a)
    (assert-eq (get-method f :a) nil)
    (defmethod f :default [_] :default)
    (assert-eq (get-method f :a) (get-method f :default))
    (assert-not (pcall remove-method))
    (assert-not (pcall remove-method []))
    (assert-not (pcall remove-method [] :a))
    (assert-not (pcall remove-method f))
    (assert-not (pcall remove-method f :a :b)))

  (testing "remove-all-methods"
    (defmulti f identity)
    (defmethod f :a [_] :a)
    (defmethod f :b [_] :b)
    (defmethod f :default [_] :default)
    (remove-all-methods f)
    (assert-eq (methods f) {})
    (assert-not (pcall remove-all-methods))
    (assert-not (pcall remove-all-methods []))
    (assert-not (pcall remove-all-methods f f))))

(deftest math-functions
  (testing "add"
    (assert-eq (add) 0)
    (assert-eq (add 1) 1)
    (assert-eq (add -1) -1)
    (assert-eq (add 1 2) 3)
    (assert-eq (add 1 2 3) 6)
    (assert-eq (add 1 2 3 4) 10)
    (assert-eq (add 1 2 3 4 5) 15))

  (testing "sub"
    (assert-eq (sub) 0)
    (assert-eq (sub 1) -1)
    (assert-eq (sub -1) 1)
    (assert-eq (sub 1 2) -1)
    (assert-eq (sub 1 2 3) -4)
    (assert-eq (sub 1 2 3 4) -8)
    (assert-eq (sub 1 2 3 4 5) -13))

  (testing "mul"
    (assert-eq (mul) 1)
    (assert-eq (mul 1) 1)
    (assert-eq (mul -1) -1)
    (assert-eq (mul 1 2) 2)
    (assert-eq (mul 1 2 3) 6)
    (assert-eq (mul 1 2 3 4) 24)
    (assert-eq (mul 1 2 3 4 5) 120))

  (testing "div"
    (assert-not (pcall div))
    (assert-eq (div 1) 1)
    (assert-eq (div -1) -1)
    (assert-eq (div 1 2) (/ 1 2))
    (assert-eq (div 1 2 3) (/ 1 2 3))
    (assert-eq (div 1 2 3 4) (/ 1 2 3 4))
    (assert-eq (div 1 2 3 4 5) (/ 1 2 3 4 5))))

(deftest comparison-functions
  (testing "le"
    (assert-not (pcall le))
    (assert-is (le 1))
    (assert-is (le 1 2))
    (assert-is (le 1 2 2))
    (assert-is (le 1 2 3 4))
    (assert-not (le 2 1))
    (assert-not (le 2 2 1))
    (assert-not (le 2 1 3))
    (assert-not (le 1 2 4 3)))

  (testing "lt"
    (assert-not (pcall lt))
    (assert-is (lt 1))
    (assert-is (lt 1 2))
    (assert-is (lt 1 2 3))
    (assert-is (lt 1 2 3 4))
    (assert-not (lt 2 1))
    (assert-not (lt 2 2 1))
    (assert-not (lt 2 1 3))
    (assert-not (lt 1 2 4 4)))

  (testing "ge"
    (assert-not (pcall ge))
    (assert-is (ge 2))
    (assert-is (ge 2 1))
    (assert-is (ge 3 3 2))
    (assert-is (ge 4 3 2 -1))
    (assert-not (ge 1 2))
    (assert-not (ge 1 1 2))
    (assert-not (ge 2 1 3))
    (assert-not (ge 1 2 4 4)))

  (testing "gt"
    (assert-not (pcall gt))
    (assert-is (gt 2))
    (assert-is (gt 2 1))
    (assert-is (gt 3 2 1))
    (assert-is (gt 4 3 2 -1))
    (assert-not (gt 1 2))
    (assert-not (gt 1 1 2))
    (assert-not (gt 2 1 3))
    (assert-not (gt 1 2 4 4))))

(deftest vector
  (testing "vector"
    (assert-eq (vector) [])
    (assert-eq (vector 1) [1])
    (assert-eq (vector 1 2 3) [1 2 3])
    (assert-eq (getmetatable (vector 1 2 3)) {:cljlib/type :seq})))

(deftest hash-map
  (testing "hash-map"
    (assert-not (pcall hash-map :a))
    (assert-eq (hash-map) {})
    (assert-eq (hash-map :a 1) {:a 1})
    (assert-eq (hash-map :a 1 :b 2 :c 3) {:a 1 :b 2 :c 3})
    (assert-eq (getmetatable (hash-map)) {:cljlib/type :table})
    (assert-not (pcall hash-map nil 1))))

(deftest sets
  (testing "hash-set"
    (let [h1 (hash-set [1] [1] [2] [3] [:a])
          h2 (hash-set [1] [2] [3] [:a])]
      (assert-is (eq h1 h2)))

    (let [h3 (hash-set [1] [1] [2] [3] [:a])
          h4 (hash-set [1] [1] [3] [:a])]
      (assert-not (eq h3 h4)))

    (assert-eq (. (hash-set [1]) [1]) [1])
    (assert-eq (. (hash-set [1]) [2]) nil)
    (assert-eq ((hash-set [1]) [1]) [1])
    (assert-eq ((hash-set [1]) [2]) nil))

  (testing "ordered-set"
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
    (let [o1 (ordered-set [1] [[-1 0] 1] [2] [3] [:a] :a 2)
          h1 (hash-set    [1] [[-1 0] 1] [2] [3] [:a] :a 2)]
      (assert-eq o1 h1))

    (let [o2 (ordered-set [1] [[-1 0] 1] [2] [3] [:a] :a 2)
          h2 (hash-set    [1] [[-1 1] 1] [2] [3] [:a] :a 2)]
      (assert-ne o2 h2))

    (let [o3 (ordered-set [1] [[-1 0] 1] [2] [3] [:a] :a 2)
          h3 (hash-set    [1] [[-1 0] 1] [2] [3] [:a] :a 2)]
      (assert-eq (disj o3 [2]) (disj h3 [2]))
      (assert-ne (disj o3 :a) h3)
      (assert-eq (disj h3 :a) o3))

    (let [o4 (ordered-set [1] [[-1 5] 1] [3] [:a] :a 2)
          h4 (hash-set    [1] [[-1 5] 1] [2] [3] [:a] :a 2)]
      (assert-eq (conj o4 [2]) (conj (disj h4 [2]) [2]))))

  (testing "empty sets"
    (assert-eq (empty (ordered-set)) (ordered-set))
    (assert-eq (empty (ordered-set 1 2 3)) (ordered-set))
    (assert-eq (. (getmetatable (empty (ordered-set))) :cljlib/type ) :cljlib/ordered-set)

    (assert-eq (empty (hash-set)) (hash-set))
    (assert-eq (empty (hash-set 1 2 3)) (hash-set))
    (assert-eq (. (getmetatable (empty (hash-set))) :cljlib/type ) :cljlib/hash-set))

  (testing "into sets"
    (assert-eq (into (ordered-set) [1 2 3]) (ordered-set 1 2 3))
    (assert-eq (into (ordered-set) {:a 1 :b 2}) (ordered-set [:a 1] [:b 2]))
    (assert-eq (into (ordered-set) "vaiv") (ordered-set "v" "a" "i" "v"))
    (assert-eq (into (hash-set) [1 2 3]) (hash-set 1 2 3))
    (assert-eq (into (hash-set) {:a 1 :b 2}) (hash-set [:a 1] [:b 2]))
    (assert-eq (into (hash-set) "vaiv") (hash-set "v" "a" "i" "v")))

  (testing "sets into tables"
    (assert-eq (into [] (ordered-set 1 2 3)) [1 2 3])
    (assert-eq (into [] (ordered-set :a :b :c)) [:a :b :c])
    (assert-eq (into {} (ordered-set [:a 1] [:b 2])) {:a 1 :b 2})))

(deftest memoization
  (testing "memoize"
    (macros {:time #`(let [clock# os.clock
                           start# (clock#)
                           res# ,$
                           end# (clock#)]
                       (values res# (* 1000 (- end# start#))))})

    (fn sleep [ms]
      (let [clock os.clock
            end (+ (clock) (/ ms 1000))]
        (while (< (clock) end) nil)))

    (fn slow [x] (sleep 100) x)

    (assert-not (pcall memoize))
    (assert-not (pcall memoize slow 2))

    (local fast (memoize slow))

    (let [(res1 time1) (time (fast 42))
          (res2 time2) (time (fast 42))]
      (assert-is (eq res1 res2 42))
      (assert-is (< time2 time1)))

    (let [(res1 time1) (time (fast [10]))
          (res2 time2) (time (fast [10]))]
      (assert-is (eq res1 res2 [10]))
      (assert-is (< time2 time1)))

    (let [(res1 time1) (time (fast {[[1] [2 [3]]] {:a 2} {{:a 1} {:b 1}} {{:c 3} {:d 4}}}))
          (res2 time2) (time (fast {[[1] [2 [3]]] {:a 2} {{:a 1} {:b 1}} {{:c 3} {:d 4}}}))]
      (assert-is (eq res1 res2 {[[1] [2 [3]]] {:a 2} {{:a 1} {:b 1}} {{:c 3} {:d 4}}}))
      (assert-is (< time2 time1)))))
