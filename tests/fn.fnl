(require-macros :fennel-test.test)
(require-macros :macros)

(deftest fn*
  (testing "fn* meta"
    (fn* f
      "docstring"
      [x] x)
    (assert-eq (meta f)
               (when-meta {:fnl/docstring "docstring"
                           :fnl/arglist ["([x])"]}))
    (fn* f
      "docstring"
      [])
    (assert-eq (meta f)
               (when-meta {:fnl/docstring "docstring"
                           :fnl/arglist ["([])"]}))
    (fn* f
      "docstring"
      ([x] x))
    (assert-eq (meta f)
               (when-meta {:fnl/docstring "docstring"
                           :fnl/arglist ["([x])"]}))
    (fn* f
      "docstring"
      ([x] x)
      ([x y] (+ x y)))
    (assert-eq (meta f)
               (when-meta {:fnl/docstring "docstring"
                           :fnl/arglist ["([x])"
                                         "([x y])"]}))
    (fn* f
      "docstring"
      ([])
      ([x y] (+ x y)))
    (assert-eq (meta f)
               (when-meta {:fnl/docstring "docstring"
                           :fnl/arglist ["([])"
                                         "([x y])"]}))
    (fn* f
      "docstring"
      ([x] x)
      ([x y] (+ x y))
      ([x y & z] (+ x y)))
    (assert-eq (meta f)
               (when-meta {:fnl/docstring "docstring"
                           :fnl/arglist ["([x])"
                                         "([x y])"
                                         "([x y & z])"]})))

  (testing "fn* doc destructuring"
    (fn* f [[a b c]])
    (assert-eq (meta f)
               (when-meta {:fnl/arglist ["([[a b c]])"]}))
    (fn* f ([[a b c]]) ([{: a}]) ([[{:a [a b c]}]]))
    (assert-eq (meta f)
               (when-meta {:fnl/arglist ["([[a b c]])"
                                         "([{:a a}])"
                                         "([[{:a [a b c]}]])"]})))

  (testing "fn* methods"
    (local ns {:a 1 :b 2})

    (fn* ns:foo []
      (+ self.a self.b))
    (assert-eq (ns:foo) 3)
    (assert-not (pcall #(ns:foo 1)))
    (assert-not (pcall #(ns:foo 1 2)))

    (fn* ns:bar
      ([x] (+ self.a x))
      ([x y] (+ self.b x y)))
    (assert-eq (ns:bar -1) 0)
    (assert-eq (ns:bar 10 20) 32)
    (assert-not (pcall #(ns:bar)))
    (assert-not (pcall #(ns:bar 1 2 3))))

  (testing "fn* anonymous calls"
    (assert-eq ((fn* [])) (values))
    (assert-eq ((fn* [] nil)) nil)
    (assert-eq ((fn* [x] x) 5) 5)
    (assert-eq ((fn* [a b c d e] [e d c b a]) 1 2 3 4 5) [5 4 3 2 1])
    (assert-eq ((fn* ([x] x) ([x y] [y x])) 10) 10)
    (assert-eq ((fn* ([x] x) ([x y] [y x])) 10 20) [20 10])))
