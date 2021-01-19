(require-macros :tests.test)
(require-macros :macros)

(deftest fn*
  (testing "fn* meta"
    (fn* f
      "docstring"
      [x] x)
    (assert-eq (meta f) (when-meta {:fnl/docstring "docstring"
                                    :fnl/arglist ["([x])"]}))
    (fn* f
      "docstring"
      [])
    (assert-eq (meta f) (when-meta {:fnl/docstring "docstring"
                                    :fnl/arglist ["([])"]}))

    (fn* f
      "docstring"
      ([x] x))
    (assert-eq (meta f) (when-meta {:fnl/docstring "docstring"
                                    :fnl/arglist ["([x])"]}))

    (fn* f
      "docstring"
      ([x] x)
      ([x y] (+ x y)))
    (assert-eq (meta f) (when-meta {:fnl/docstring "docstring"
                                    :fnl/arglist ["([x])"
                                                  "([x y])"]}))

    (fn* f
      "docstring"
      ([])
      ([x y] (+ x y)))
    (assert-eq (meta f) (when-meta {:fnl/docstring "docstring"
                                    :fnl/arglist ["([])"
                                                  "([x y])"]}))

    (fn* f
      "docstring"
      ([x] x)
      ([x y] (+ x y))
      ([x y & z] (+ x y)))
    (assert-eq (meta f) (when-meta {:fnl/docstring "docstring"
                                    :fnl/arglist ["([x])"
                                                  "([x y])"
                                                  "([x y & z])"]}))))
