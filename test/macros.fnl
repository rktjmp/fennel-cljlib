(require-macros :test.test)
(local {: eq : identity} (require :core)) ;; required for testing

(require-macros :macros.core)

(deftest into
  (testing into
    (assert-eq (into [] []) [])
    (assert-eq (into [1 2 3] []) [1 2 3])
    (assert-eq (into [1 2 3] [4 5 6]) [1 2 3 4 5 6])

    (assert-eq (into {} {}) {})
    (assert-eq (into {:a 1} {}) {:a 1})
    (assert-eq (into {:a 1} {:b 2}) {:a 1 :b 2})

    ;; different bodies are being used so worth testing
    (assert-eq (into [] {}) [])
    (assert-eq (into {} []) [])

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
      (assert-eq (into b {:a 1}) [[:a 1]]))))

(deftest let-variants
  (testing when-let
    (assert-eq (when-let [a 4] a) 4)
    (assert* (not (when-let [a false] a)) "(not (when-let [a false] a))")
    (assert* (not (when-let [a nil] a)) "(not (when-let [a nil] a))"))

  (testing when-some
    (assert-eq (when-some [a [1 2 3]] a) [1 2 3])
    (assert-eq (when-some [a false] a) false)
    (assert* (not (when-some [a nil] a)) "(when-some [a nil] a)"))

  (testing if-let
    (assert-eq (if-let [a 4] a 10) 4)
    (assert-eq (if-let [a false] a 10) 10)
    (assert-eq (if-let [a nil] a 10) 10))

  (testing if-some
    (assert-eq (if-some [a [1 2 3]] a :nothing) [1 2 3])
    (assert-eq (if-some [a false] a :nothing) false)
    (assert-eq (if-some [a nil] a :nothing) :nothing)))

(deftest multimethods
  (testing defmulti
    (defmulti x (fn [x] x))
    (assert-eq (defmulti x (fn [x] (+ x 1))) nil))

  (testing defmethod
    (defmulti fac identity)
    (defmethod fac 0 [_] 1)
    (defmethod fac :default [x] (* x (fac (- x 1))))
    (assert-eq (fac 42) 7538058755741581312)

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
               "sending 42 over ICAP")))

(deftest def-macros
  (testing def
    (def {:dynamic true} a 10)
    (assert-eq a 10)
    (set a 20)
    (assert-eq a 20)
    (def a {})
    (assert-eq a {})
    (def a.b 10)
    (assert-eq a.b 10)
    (assert-eq b 10)
    (def :dynamic c 10)
    (set c 15)
    (assert-eq c 15))

  (testing defonce
    (defonce {:dynamic true} a 10)
    (assert-eq a 10)
    (defonce a {})
    (assert-eq a 10)
    (defonce b {})
    (defonce b.a 10)
    (assert-eq b.a 10)
    (assert-eq a 10)))

(deftest meta
  (testing with-meta
    (assert-eq (meta (with-meta :a {:k :v})) (when-meta {:k :v})))

  (testing def-meta
    (def {:doc "x"} x 10)
    (assert-eq (meta x) (when-meta {:fnl/docstring "x"}))
    (def {:doc "x" :dynamic true} x 10)
    (assert-eq (meta x) (when-meta {:fnl/docstring "x"})))

  (testing defonce-meta
    (defonce {:doc "x"} x 10)
    (assert-eq (meta x) (when-meta {:fnl/docstring "x"}))
    (defonce {:doc "y"} x 20)
    (assert-eq (meta x) (when-meta {:fnl/docstring "x"}))
    (defonce {:doc "y" :dynamic true} y 20)
    (assert-eq (meta y) (when-meta {:fnl/docstring "y"}))))

(deftest empty
  (testing empty
    (assert-eq (empty {}) {})
    (assert-eq (empty []) {})
    (assert-eq (getmetatable (empty [])) {:cljlib/table-type :seq})
    (assert-eq (getmetatable (empty {})) {:cljlib/table-type :table})))
