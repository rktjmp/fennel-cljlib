(require-macros :fennel-test.test)
(require-macros :init-macros)

(deftest into
  (testing "into"
    (assert-eq (into [] nil) [])
    (assert-eq (into nil nil) nil)
    (assert-eq (into nil [1 2 3]) [1 2 3])

    (assert-eq (into [] []) [])
    (assert-eq (into [1 2 3] []) [1 2 3])
    (assert-eq (into [1 2 3] [4 5 6]) [1 2 3 4 5 6])

    (assert-eq (into {} {}) {})
    (assert-eq (into {:a 1} {}) {:a 1})
    (assert-eq (into {:a 1} {:b 2}) {:a 1 :b 2})

    ;; different bodies are being used at compile time so worth testing
    (assert-eq (into [] {}) [])
    (assert-eq (into {} []) [])
    (assert-eq (. (getmetatable (into [] {})) :cljlib/type) :seq)
    (assert-eq (. (getmetatable (into {} [])) :cljlib/type) :table)
    (let [a []] (assert-eq (. (getmetatable (into a a)) :cljlib/type) :seq))

    ;; can't transform table with more than one key-value pair, as order
    ;; is undeterminitive
    (assert-eq (into [] {:a 1}) [[:a 1]])
    (assert-eq (into [[:b 2]] {:a 1}) [[:b 2] [:a 1]])
    (assert-eq (into [[:c 3]] {}) [[:c 3]])

    (assert-eq (into {} [[:c 3] [:a 1] [:b 2]]) {:a 1 :b 2 :c 3})
    (assert-eq (into {:d 4} [[:c 3] [:a 1] [:b 2]]) {:a 1 :b 2 :c 3 :d 4})
    (assert-eq (into {:a 0 :b 0 :c 0} [[:c 3] [:a 1] [:b 2]]) {:a 1 :b 2 :c 3})

    (let [a (fn [] {:a 1})
          b (fn [] [[:b 2]])]
      (assert-eq (into (a) (b)) {:a 1 :b 2})
      (assert-eq (into (b) (a)) [[:b 2] [:a 1]])
      (let [c []]
        (assert-eq (into c (b)) [[:b 2]]))
      (let [c []]
        (assert-eq (into c (a)) [[:a 1]]))
      (let [c []]
        (assert-eq (into (b) c) (b))
        (assert-eq (into (a) c) (a))))

    (let [a {}
          b []]
      (assert-eq (into a [1 2 3]) [1 2 3])
      (assert-eq (into b [1 2 3]) [1 2 3]))
    (let [a {}
          b []]
      (assert-eq (into b {:a 1}) [[:a 1]]))

    (let [a {}
          b []]
      (assert-eq (into a "vaiv") ["v" "a" "i" "v"])
      (when _G.utf8 (assert-eq (into b "ваыв") ["в" "а" "ы" "в"])))
    (assert-eq (into [] "vaiv") ["v" "a" "i" "v"])
    (when _G.utf8 (assert-eq (into [] "ваыв") ["в" "а" "ы" "в"]))))

(deftest let-variants
  (testing "when-let"
    (assert-eq (when-let [a 4] a) 4)
    (assert-not (when-let [a false] a) "(not (when-let [a false] a))")
    (assert-not (when-let [a nil] a) "(not (when-let [a nil] a))"))

  (testing "when-some"
    (assert-eq (when-some [a [1 2 3]] a) [1 2 3])
    (assert-eq (when-some [a false] a) false)
    (assert-not (when-some [a nil] a) "(when-some [a nil] a)"))

  (testing "if-let"
    (assert-eq (if-let [a 4] a 10) 4)
    (assert-eq (if-let [a false] a 10) 10)
    (assert-eq (if-let [a nil] a 10) 10))

  (testing "if-some"
    (assert-eq (if-some [a [1 2 3]] a :nothing) [1 2 3])
    (assert-eq (if-some [a false] a :nothing) false)
    (assert-eq (if-some [a nil] a :nothing) :nothing)))

(deftest multimethods
  (testing "defmulti"
    (defmulti x (fn [x] x))
    (assert-eq (defmulti x (fn [x] (+ x 1))) nil))

  (testing "defmulti defalut"
    (defmulti fac (fn [x] x))
    (defmethod fac 0 [_] 1)
    (defmethod fac :default [x] (* x (fac (- x 1))))
    (assert-eq (fac 4) 24))

  (testing "defmulti keys"
    (defmulti send-data (fn [protocol data] protocol))
    (defmethod send-data :http [protocol data] (.. data " will be sent over HTTP"))
    (defmethod send-data :icap [protocol data] (.. data " will be sent over ICAP"))
    (assert-eq (send-data :http 42) "42 will be sent over HTTP")
    (assert-eq (send-data :icap 42) "42 will be sent over ICAP")

    (defmulti send-message (fn [message] (. message :protocol)))
    (defmethod send-message :http [message] (.. "sending " (. message :message) " over HTTP"))
    (defmethod send-message :icap [message] (.. "sending " (. message :message) " over ICAP"))
    (assert-eq (send-message {:protocol :http :message "ваыв"})
               "sending ваыв over HTTP")
    (assert-eq (send-message {:protocol :icap :message 42})
               "sending 42 over ICAP"))

  (testing "defmulti with dispatch on tables"
    (defmulti encounter (fn [x y] [(. x :species) (. y :species)]))
    (defmethod encounter [:bunny :lion] [_ _] :run)
    (defmethod encounter [:lion :bunny] [_ _] :eat)
    (defmethod encounter [:lion :lion] [_ _] :fight)
    (defmethod encounter [:bunny :bunny] [_ _] :mate)

    (let [l {:species :lion}
          b {:species :bunny}]
      (assert-eq (encounter b b) :mate)
      (assert-eq (encounter l l) :fight)
      (assert-eq (encounter b l) :run)
      (assert-eq (encounter l b) :eat)))

  (testing "defmulti default name"
    (defmulti f (fn [x] x) :default :my-default)
    (defmethod f :my-default [_] 42)
    (assert-eq (f 10) 42))


  (testing "defmulti docstring"
    (defmulti f "documentation" (fn [x] x))
    (assert-eq (meta f) {:fnl/docstring "documentation"})
    (defmulti g "documentation" (fn [x] x) :default 0)
    (assert-eq (meta g) {:fnl/docstring "documentation"}))

  (testing "defmulti with multiple arity"
    (defmulti f (fn* ([x] x) ([x y] [x y])))
    (defmethod f :default ([_] :def) ([_ _] :def2))
    (defmethod f :4 ([x] (.. x :2)))
    (defmethod f [:4 :2] ([x y] 42))

    (assert-eq (f 0) :def)
    (assert-eq (f 0 1) :def2)
    (assert-eq (f :4) :42)
    (assert-eq (f :4 :2) 42)))

(deftest def-macros
  (testing "def"
    (def {:mutable true} a 10)
    (assert-eq a 10)
    (set a 20)
    (assert-eq a 20)
    (def a {})
    (assert-eq a {})
    (def a.b 10)
    (assert-eq a.b 10)
    (assert-eq b 10)
    (def :mutable c 10)
    (set c 15)
    (assert-eq c 15))

  (testing "defonce"
    (defonce {:mutable true} a 10)
    (assert-eq a 10)
    (defonce a {})
    (assert-eq a 10)
    (defonce b {})
    (defonce b.a 10)
    (assert-eq b.a 10)
    (assert-eq a 10)))

(deftest meta
  (testing "with-meta"
    (assert-eq (meta (with-meta :a {:k :v})) {:k :v}))

  (testing "def meta"
    (def {:doc "x"} x 10)
    (assert-eq (meta x) {:fnl/docstring "x"})
    (def {:doc "x" :mutable true} x 10)
    (assert-eq (meta x) {:fnl/docstring "x"}))

  (testing "defonce meta table"
    (defonce {:doc "x"} x 10)
    (assert-eq (meta x) {:fnl/docstring "x"})
    (defonce {:doc "y"} x 20)
    (assert-eq (meta x) {:fnl/docstring "x"})
    (defonce {:doc "y" :mutable true} y 20)
    (assert-eq (meta y) {:fnl/docstring "y"})))

(deftest empty
  (testing "empty map"
    (assert-eq (empty {}) {})
    (assert-eq (getmetatable (empty {})) {:cljlib/type :table})
    (let [a {:a 1 :b 2}]
      (assert-eq (empty a) {})
      (assert-eq (getmetatable (empty a)) {:cljlib/type :table}))
    (let [a {}]
      (assert-eq (empty a) [])
      (assert-eq (getmetatable (empty a)) {:cljlib/type :empty})))

  (testing "empty seq"
    (assert-eq (empty []) {})
    (assert-eq (getmetatable (empty [])) {:cljlib/type :seq})
    (let [a [:a 1 :b 2]]
      (assert-eq (empty a) [])
      (assert-eq (getmetatable (empty a)) {:cljlib/type :seq}))))

(deftest try
  (testing "try"
    (assert-eq (try (+ 1 2 3)) 6)
    (assert-eq (try (+ 1 2 3) (catch _ 0) (finally 10)) 6)
    (assert-eq (try (+ 1 2 3 nil) (catch _ 0) (finally 10)) 0)
    (assert-eq (try (+ 1 2 3 nil) (catch _) (finally 10)) nil))

  (testing "catch-all"
    (assert-eq (try
                 (error "10")
                 (catch _ :pass))
               :pass)
    (assert-eq (try
                 (error [10])
                 (catch err err))
               [10]))

  (testing "finally"
    (let [tbl []]
      (try
        (try
          (finally (table.insert tbl 1)))
        (try
          (error 10)
          (catch _ (table.insert tbl 2))
          (finally (table.insert tbl 3)))
        (try
          (error 20)
          (finally (table.insert tbl 4)))
        (catch _ (table.insert tbl 5))
        (catch 20 (table.insert tbl 6))
        (finally (table.insert tbl 7)))
      (assert-eq tbl [1 2 3 4 5 7])))

  (testing "runtime error"
    (assert-eq 0 (try
                   (/ 1 nil)
                   (catch _ 0))))

  (testing "multi-value results"
    (assert-eq 3 (select :# (try (values 1 2 3))))
    (assert-eq [1 2 3] [(try (values 1 2 3))])
    (assert-eq 6 (select :# (try (values 1 nil 3 nil nil nil))))
    (assert-eq 6 (select :# (try (error 10) (catch _ (values 1 nil 3 nil nil nil)))))))

(deftest loop
  (testing "loop macro"
    (assert-eq
     (loop [[first & rest] [1 2 3 4 5]
            acc 0]
       (if (= nil first)
           acc
           (recur rest (+ acc first))))
     15)

    (assert-eq
     (loop [a 2
            b (+ a 3)]
       (if (= b 5)
           (recur a (+ 1 b))
           b))
     6)))
