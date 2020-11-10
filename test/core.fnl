(require-macros :macros.fn)
(require-macros :macros.core)
(require-macros :test.test)

(local
 {: vector
  : hash-map
  : apply
  : seq
  : first
  : rest
  : last
  : butlast
  : conj
  : cons
  : concat
  : reduce
  : reduce-kv
  : mapv
  : filter
  : map?
  : seq?
  : nil?
  : zero?
  : pos?
  : neg?
  : even?
  : odd?
  : int?
  : pos-int?
  : neg-int?
  : double?
  : string?
  : boolean?
  : false?
  : true?
  : empty?
  : not-empty
  : eq
  : identity
  : comp
  : every?
  : some
  : not-any?
  : complement
  : constantly
  : range
  : reverse
  : inc
  : dec
  : assoc
  : get
  : get-in
  : get-method
  : methods
  : remove-method
  : remove-all-methods
  : add
  : sub
  : mul
  : div
  : le
  : ge
  : lt
  : gt}
 (require :core))

(deftest equality
  (testing "comparing basetypes"
    (assert* (not (pcall eq)))
    (assert-eq 1 1)
    (assert-ne 1 2)
    (assert* (eq 1 1 1 1 1))
    (assert-eq 1.0 1.0)
    (assert* (eq 1.0 1.0 1.0))
    (assert* (eq 1.0 1.0 1.0))
    (assert* (eq "1" "1" "1" "1" "1")))

  (testing "deep comparison"
    (assert* (eq []))
    (assert-eq [] [])
    (assert-eq [] {})
    (assert-eq [1 2] [1 2])
    (assert-ne [1] [1 2])
    (assert-ne [1 2] [1])
    (assert* (eq [1 [2]] [1 [2]] [1 [2]]))
    (assert* (eq [1 [2]] [1 [2]] [1 [2]]))
    (assert* (not (eq [1 [2]] [1 [2]] [1 [2 [3]]])))

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
    (assert-eq {4 1} [nil nil nil 1])))

(testing "range"
  (assert* (not (pcall range)))
  (assert-eq (range 10) [0 1 2 3 4 5 6 7 8 9])
  (assert-eq (range -5 5) [-5 -4 -3 -2 -1 0 1 2 3 4])
  (assert-eq [0 0.2 0.4 0.6 0.8] [0 0.2 0.4 0.6 0.8])
  (assert-eq (range 0 1 0.2) (range 0 1 0.2)))

(deftest predicates
  (testing "zero?"
    (assert* (zero? 0))
    (assert* (zero? -0))
    (assert* (not (zero? 1))))

  (testing "int?"
    (assert* (int? 1))
    (assert* (not (int? 1.1))))

  (testing "pos?"
    (assert* (pos? 1))
    (assert* (and (not (pos? 0)) (not (pos? -1)))))

  (testing "neg?"
    (assert* (neg? -1))
    (assert* (and (not (neg? 0)) (not (neg? 1)))))

  (testing "pos-int?"
    (assert* (pos-int? 42))
    (assert* (not (pos-int? 4.2))))

  (testing "neg-int?"
    (assert* (neg-int? -42))
    (assert* (not (neg-int? -4.2))))

  (testing "string?"
    (assert* (string? :s)))

  (testing "double?"
    (assert* (double? 3.3))
    (assert* (not (double? 3.0))))

  (testing "map?"
    (assert* (map? {:a 1}))
    (assert* (not (map? {})))
    (assert* (map? (empty {})))
    (assert* (not (map? (empty [])))))

  (testing "seq?"
    (assert* (not (seq? [])))
    (assert* (seq? [{:a 1}]))
    (assert* (not (seq? {})))
    (assert* (not (seq? {:a 1})))
    (assert* (seq? (empty [])))
    (assert* (not (seq? (empty {})))))

  (testing "nil?"
    (assert* (nil?))
    (assert* (nil? nil))
    (assert* (not (nil? 1))))

  (testing "odd?"
    (assert* (odd? 3))
    (assert* (odd? -3))
    (assert* (not (odd? 2)))
    (assert* (not (odd? -2))))

  (testing "even?"
    (assert* (even? 2))
    (assert* (even? -2))
    (assert* (not (even? 23)))
    (assert* (not (even? -23))))

  (testing "true?"
    (assert* (true? true))
    (assert* (not (true? false)))
    (assert* (not (true? 10)))
    (assert* (not (true? :true))))

  (testing "false?"
    (assert* (false? false))
    (assert* (not (false? true)))
    (assert* (not (false? 10)))
    (assert* (not (false? :true))))

  (testing "boolean?"
    (assert* (boolean? true))
    (assert* (boolean? false))
    (assert* (not (boolean? :false)))
    (assert* (not (boolean? (fn [] true))))))

(deftest sequence-functions
  (testing "seq"
    (assert-eq (seq []) nil)
    (assert-eq (seq {}) nil)
    (assert-eq (seq [1]) [1])
    (assert-eq (seq [1 2 3]) [1 2 3])
    (assert-eq (seq {:a 1}) [["a" 1]]))

  (testing "mapv"
    (assert* (not (pcall mapv)))
    (assert* (not (pcall mapv #(do nil))))
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
                "Alice Watson works as chief officer at Coffee With You"]))

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
    (assert* (not (pcall reduce)))
    (assert* (not (pcall reduce add))))

  (testing "reduce reference implementation"
    (fn mapping [f]
      (fn [reducing]
        (fn [result input]
          (reducing result (f input)))))

    (fn reduce- [f init [x & tbl]]
      (if x (reduce- f (f init x) tbl) init))

    (assert-eq (reduce add (range 10)) (reduce- add 0 (range 10)))
    (assert-eq (reduce ((mapping inc) add) 0 (range 10))
               (reduce- ((mapping inc) add) 0 (range 10))))

  (testing "filter"
    (assert* (not (pcall filter)))
    (assert* (not (pcall filter even?)))
    (assert-eq (filter even? (range 10)) [0 2 4 6 8])
    (assert-eq (filter odd? (range 10)) [1 3 5 7 9])
    (assert-eq (filter map? [{:a 1} {5 1} [1 2] [] {}]) [{:a 1} {5 1}])
    (assert-eq (filter seq? [{:a 1} {5 1} [1 2] [] {}]) [[1 2]]))

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
    (assert* (not (pcall concat 1)))
    (assert* (not (pcall concat 1 2)))
    (assert* (not (pcall concat 1 [])))
    (assert* (not (pcall concat [] 2)))
    (assert* (not (pcall concat [1] 2))))

  (testing "reverse"
    (assert-eq (reverse []) nil)
    (assert-eq (reverse [1 2 3]) [3 2 1])
    (assert-eq (reverse {:a 1}) [[:a 1]]))

  (testing "conj"
    (assert-eq (conj) [])
    (assert-eq (conj [1] nil) [1])
    (assert-eq (conj [] 1 2 3) [1 2 3])
    (assert-eq (conj [0] 1 2 3) [0 1 2 3])
    (assert-eq (conj {:a 1} [:b 2]) {:a 1 :b 2})
    (assert-eq (conj {:a 1}) {:a 1})
    (assert-eq (conj [1] 2 3 4 5 6 7) [1 2 3 4 5 6 7]))

  (testing "cons"
    (assert-eq (cons nil [1]) [1])
    (assert-eq (cons 1 []) [1])
    (assert-eq (cons 1 [0]) [1 0]))

  (testing "first"
    (assert-eq (first [1 2 3]) 1)
    (assert-eq (first {:a 1}) [:a 1])
    (assert-eq (first []) nil))

  (testing "last"
    (assert-eq (last [1 2 3]) 3)
    (assert-eq (last []) nil)
    (assert-eq (last nil) nil)
    (assert-eq (last {:a 1}) [:a 1]))

  (testing "rest"
    (assert-eq (rest [1 2 3]) [2 3])
    (assert-eq (rest {:a 1}) [])
    (assert-eq (rest []) [])
    (assert-eq (rest nil) []))

  (testing "butlast"
    (assert-eq (butlast [1 2 3]) [1 2])
    (assert-eq (butlast {:a 1}) nil)
    (assert-eq (butlast []) nil)
    (assert-eq (butlast nil) nil))

  (testing "reduce-kv"
    (assert-eq (reduce-kv #(+ $1 $3) 0 {:a 1 :b 2 :c 3}) 6)
    (assert* (not (pcall reduce-kv #(+ $1 $3) 0)))
    (assert* (not (pcall reduce-kv #(+ $1 $3))))
    (assert* (not (pcall reduce-kv))))

  (testing "assoc"
    (assert* (not (pcall assoc)))
    (assert* (not (pcall assoc {})))
    (assert-eq (assoc {} :a 1) {:a 1})
    (assert-eq (assoc {} :a 1 :b 2 :c 3 :d 4) {:a 1 :b 2 :c 3 :d 4})))

(deftest function-manipulation
  (testing "constantly"
    (let [always-nil (constantly nil)]
      (assert-eq (always-nil) nil)
      (assert-eq (always-nil 1) nil)
      (assert-eq (always-nil 1 2 3 4 "5") nil))

    (let [always-true (constantly true)]
      (assert* (always-true))
      (assert* (always-true false))))

  (testing "complement"
    (assert* ((complement #(do false))))
    (assert* ((complement nil?) 10))
    (assert* ((complement every?) double? [1 2 3 4]))
    (assert* ((complement #(= $1 $2 $3)) 1 1 2 1))
    (assert* ((complement #(= $1 $2)) 1 2)))

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
    (assert* (not (pcall apply)))
    (assert* (not (pcall apply add))))

  (testing "comp"
    (assert-eq ((comp) 10) 10)
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
    (assert-eq (identity 1) 1)
    (assert-eq (identity {:a 1 :b 2}) {:a 1 :b 2})
    (assert-eq (identity [1 2 3]) [1 2 3])
    (assert-eq (identity "abc") "abc")
    (assert-eq (identity f) f)))

(deftest sequence-predicates
  (testing "some"
    (assert* (not (pcall some)))
    (assert* (not (pcall some pos-int?)))
    (assert* (some pos-int? [-1 1.1 2.3 -5.5 42 10 -27]))
    (assert* (not (some pos-int? {:a 1})))
    (assert* (some pos-int? [{:a 1} "1" -1 1])))

  (testing "not-any?"
    (assert* (not (pcall not-any?)))
    (assert* (not (pcall not-any? pos-int?)))
    (assert* (not-any? pos-int? [-1 1.1 2.3 -5.5 -42 -10 -27]))
    (assert* (not-any? pos-int? {:a 1}))
    (assert* (not (not-any? pos-int? [1 2 3 4 5]))))

  (testing "every?"
    (assert* (not (pcall every?)))
    (assert* (not (pcall every? pos-int?)))
    (assert* (not (every? pos-int? [-1 1.1 2.3 -5.5 42 10 -27])))
    (assert* (not (every? pos-int? {:a 1})))
    (assert* (every? pos-int? [1 2 3 4 5])))

  (testing "empty?"
    (assert* (not (pcall empty?)))
    (assert* (empty? []))
    (assert* (empty? {}))
    (assert* (empty? ""))
    (assert* (not (empty? "1")))
    (assert* (not (empty? [1])))
    (assert* (not (empty? {:a 1})))
    (assert* (not (pcall empty? 10))))

  (testing "not-empty"
    (assert* (not (pcall not-empty)))
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
    (assert* (not (pcall inc)))
    (assert* (not (pcall inc nil))))

  (testing "dec"
    (assert-eq (dec 1) 0)
    (assert-eq (dec -1) -2)
    (assert* (not (pcall dec)))
    (assert* (not (pcall dec nil)))))

(deftest table-access
  (testing "get"
    (assert-eq (get {:key1 10 :key2 20} :key1) 10)
    (assert-eq (get {:key1 10 :key2 20} :key1 false) 10)
    (assert-eq (get {:key1 10 :key2 20} :key3 false) false)
    (assert-eq (get {:key1 10 :key2 20} :key3) nil)
    (assert* (not (pcall get)))
    (assert* (not (pcall get {}))))

  (testing "get-in"
    (local t {:a {:b {:c 10}}})
    (assert-eq (get-in t [:a]) {:b {:c 10}})
    (assert-eq (get-in t [:a :b]) {:c 10})
    (assert-eq (get-in t [:a :b :c]) 10)
    (assert-eq (get-in t [:a :b :c] false) 10)
    (assert-eq (get-in t [:a :b :d] false) false)
    (assert-eq (get-in t [:a :b :d]) nil)
    (assert-eq (get-in t []) t)
    (assert* (not (pcall get-in)))
    (assert* (not (pcall get-in {})))))

(deftest methods
  (testing "methods"
    (defmulti f identity)
    (defmethod f :a [_] :a)
    (defmethod f :b [_] :b)
    (defmethod f :c [x] (* x x))
    (assert-eq (methods f) (. (getmetatable f) :multimethods))
    (assert* (not (pcall methods)))
    (assert* (not (pcall methods f f))))

  (testing "get-method"
    (defmulti f identity)
    (defmethod f :a [_] :a)
    (defmethod f :b [_] :b)
    (defmethod f :c [x] (* x x))
    (assert-eq ((get-method f :a) 10) :a)
    (assert-eq ((get-method f :b) 20) :b)
    (assert-eq ((get-method f :c) 4) 16)
    (assert* (not (pcall get-method)))
    (assert* (not (pcall get-method f)))
    (assert* (not (pcall get-method f :a :b))))

  (testing "remove-method"
    (defmulti f identity)
    (defmethod f :a [_] :a)
    (defmethod f :b [_] :b)
    (remove-method f :a)
    (assert-eq (get-method f :a) nil)
    (defmethod f :default [_] :default)
    (assert-eq (get-method f :a) (get-method f :default))
    (assert* (not (pcall remove-method)))
    (assert* (not (pcall remove-method f)))
    (assert* (not (pcall remove-method f :a :b))))

  (testing "remove-all-methods"
    (defmulti f identity)
    (defmethod f :a [_] :a)
    (defmethod f :b [_] :b)
    (defmethod f :default [_] :default)
    (remove-all-methods f)
    (assert-eq (methods f) {})
    (assert* (not (pcall remove-all-methods)))
    (assert* (not (pcall remove-all-methods f f)))))

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
    (assert* (not (pcall div)))
    (assert-eq (div 1) 1)
    (assert-eq (div -1) -1)
    (assert-eq (div 1 2) (/ 1 2))
    (assert-eq (div 1 2 3) (/ 1 2 3))
    (assert-eq (div 1 2 3 4) (/ 1 2 3 4))
    (assert-eq (div 1 2 3 4 5) (/ 1 2 3 4 5))))

(deftest comparison-functions
  (testing "le"
    (assert* (not (pcall le)))
    (assert* (le 1))
    (assert* (le 1 2))
    (assert* (le 1 2 2))
    (assert* (le 1 2 3 4))
    (assert* (not (le 2 1)))
    (assert* (not (le 2 1 3)))
    (assert* (not (le 1 2 4 3))))

  (testing "lt"
    (assert* (not (pcall lt)))
    (assert* (lt 1))
    (assert* (lt 1 2))
    (assert* (lt 1 2 3))
    (assert* (lt 1 2 3 4))
    (assert* (not (lt 2 1)))
    (assert* (not (lt 2 1 3)))
    (assert* (not (lt 1 2 4 4))))

  (testing "ge"
    (assert* (not (pcall ge)))
    (assert* (ge 2))
    (assert* (ge 2 1))
    (assert* (ge 3 3 2))
    (assert* (ge 4 3 2 -1))
    (assert* (not (ge 1 2)))
    (assert* (not (ge 2 1 3)))
    (assert* (not (ge 1 2 4 4))))

  (testing "gt"
    (assert* (not (pcall gt)))
    (assert* (gt 2))
    (assert* (gt 2 1))
    (assert* (gt 3 2 1))
    (assert* (gt 4 3 2 -1))
    (assert* (not (gt 1 2)))
    (assert* (not (gt 2 1 3)))
    (assert* (not (gt 1 2 4 4)))))

(deftest vec
  (testing "vec"
    (assert-eq (vector) [])
    (assert-eq (vector 1) [1])
    (assert-eq (vector 1 2 3) [1 2 3])
    (assert-eq (getmetatable (vector 1 2 3)) {:cljlib/table-type :seq})))

(deftest hash-map
  (testing "hash-map"
    (assert* (not (pcall hash-map :a)))
    (assert-eq (hash-map) {})
    (assert-eq (hash-map :a 1) {:a 1})
    (assert-eq (hash-map :a 1 :b 2 :c 3) {:a 1 :b 2 :c 3})
    (assert-eq (getmetatable (hash-map)) {:cljlib/table-type :table})))
