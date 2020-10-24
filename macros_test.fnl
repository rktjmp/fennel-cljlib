(import-macros {: if-let : when-let : if-some : when-some : into} :macros.core)
(import-macros {: assert-eq : assert-ne : assert* : test} :test)
(local {: eq?} (require :core)) ;; required for testing


(test into
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
    (assert-eq (into (b) (a)) [[:b 2] [:a 1]]))

  (let [a {}
        b []]
    (assert-eq (into a [1 2 3]) [1 2 3])
    (assert-eq (into b [1 2 3]) [1 2 3]))
  (let [a {}
        b []]
    (assert-eq (into b {:a 1}) [[:a 1]])))

(test let-variants
  (assert-eq (when-let [a 4] a) 4)
  (assert* (not (when-let [a false] a)) "(not (when-let [a false] a))")
  (assert* (not (when-let [a nil] a)) "(not (when-let [a nil] a))")

  (assert-eq (when-some [a [1 2 3]] a) [1 2 3])
  (assert-eq (when-some [a false] a) false)
  (assert* (not (when-some [a nil] a)) "(when-some [a nil] a)")

  (assert-eq (if-let [a 4] a 10) 4)
  (assert-eq (if-let [a false] a 10) 10)
  (assert-eq (if-let [a nil] a 10) 10)

  (assert-eq (if-some [a [1 2 3]] a :nothing) [1 2 3])
  (assert-eq (if-some [a false] a :nothing) false)
  (assert-eq (if-some [a nil] a :nothing) :nothing))
