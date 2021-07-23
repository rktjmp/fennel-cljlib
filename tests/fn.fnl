(require-macros :fennel-test.test)
(require-macros :init-macros)

(deftest fn*
  (testing "fn* meta"
    (fn* f
      "single arity"
      [x] x)
    (assert-eq (meta f)
               {:fnl/docstring "single arity"
                :fnl/arglist ["[x]"]})
    (fn* f
      "single empty arity"
      [])
    (assert-eq (meta f)
               {:fnl/docstring "single empty arity"
                :fnl/arglist ["[]"]})
    (fn* f
      "multiarity with single entry"
      ([x] x))
    (assert-eq (meta f)
               {:fnl/docstring "multiarity with single entry"
                :fnl/arglist ["[x]"]})
    (fn* f
      "multiarity"
      ([x] x)
      ([x y] (+ x y)))
    (assert-eq (meta f)
               {:fnl/docstring "multiarity"
                :fnl/arglist ["([x])"
                              "([x y])"]})
    (fn* f
      "multiarity with one empty arity"
      ([])
      ([x y] (+ x y)))
    (assert-eq (meta f)
               {:fnl/docstring "multiarity with one empty arity"
                :fnl/arglist ["([])"
                              "([x y])"]})
    (fn* f
      "multiarity with two or more arity"
      ([x] x)
      ([x y] (+ x y))
      ([x y & z] (+ x y)))
    (assert-eq (meta f)
               {:fnl/docstring "multiarity with two or more arity"
                :fnl/arglist ["([x])"
                              "([x y])"
                              "([x y & z])"]}))

  (testing "fn* doc destructuring"
    (fn* f [[a b c]])
    (assert-eq (meta f)
               {:fnl/arglist ["[[a b c]]"]})
    (fn* f ([[a b c]]))
    (assert-eq (meta f)
               {:fnl/arglist ["[[a b c]]"]})
    (fn* f ([[a b c]]) ([{: a}]) ([[{:a [a b c]}]]))
    (assert-eq (meta f)
               {:fnl/arglist ["([[a b c]])"
                              "([{:a a}])"
                              "([[{:a [a b c]}]])"]}))

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
