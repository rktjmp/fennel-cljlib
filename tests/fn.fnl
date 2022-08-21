(require-macros :fennel-test)
(require-macros :init-macros)
(local (meta? fennel) (pcall require :fennel))

(fn meta [x]
  {:fnl/docstring (fennel.metadata:get x :fnl/docstring)
   :fnl/arglist (fennel.metadata:get x :fnl/arglist)})

(deftest test-fn*
  (when meta?
    (testing "fn* meta"
      (defn f
        "single arity"
        [x] x)
      (assert-eq {:fnl/docstring "single arity"
                  :fnl/arglist ["[x]"]}
                 (meta f))
      (defn f
        "single empty arity"
        [])
      (assert-eq {:fnl/docstring "single empty arity"
                  :fnl/arglist ["[]"]}
                 (meta f))
      (defn f
        "multiarity with single entry"
        ([x] x))
      (assert-eq {:fnl/docstring "multiarity with single entry"
                  :fnl/arglist ["([x])"]}
                 (meta f))
      (defn f
        "multiarity"
        ([x] x)
        ([x y] (+ x y)))
      (assert-eq {:fnl/docstring "multiarity"
                  :fnl/arglist ["([x])"
                                "([x y])"]}
                 (meta f))
      (defn f
        "multiarity with one empty arity"
        ([])
        ([x y] (+ x y)))
      (assert-eq {:fnl/docstring "multiarity with one empty arity"
                  :fnl/arglist ["([])"
                                "([x y])"]}
                 (meta f))
      (defn f
        "multiarity with two or more arity"
        ([x] x)
        ([x y] (+ x y))
        ([x y & z] (+ x y)))
      (assert-eq {:fnl/docstring "multiarity with two or more arity"
                  :fnl/arglist ["([x])"
                                "([x y])"
                                "([x y & z])"]}
                 (meta f))))

  (testing "defn doc destructuring"
    (defn f [[a b c]])
    (assert-eq {:fnl/arglist ["[[a b c]]"]}
               (meta f))
    (defn f ([[a b c]]))
    (assert-eq {:fnl/arglist ["([[a b c]])"]}
               (meta f))
    (defn f ([[a b c]]) ([{: a} b]) ([[{:a [a b c]}] d e]))
    (assert-eq {:fnl/arglist ["([[a b c]])"
                              "([{:a a} b])"
                              "([[{:a [a b c]}] d e])"]}
               (meta f)))

  (testing "defn anonymous calls"
    (assert-eq ((fn* [])) (values))
    (assert-eq ((fn* [] nil)) nil)
    (assert-eq ((fn* [x] x) 5) 5)
    (assert-eq ((fn* [a b c d e] [e d c b a]) 1 2 3 4 5) [5 4 3 2 1])
    (assert-eq ((fn* ([x] x) ([x y] [y x])) 10) 10)
    (assert-eq ((fn* ([x] x) ([x y] [y x])) 10 20) [20 10])))
