(import-macros {: assert-eq : assert-ne : assert* : testing : deftest} :test.test)
(import-macros {: meta} :macros.core)
(local {: eq} (require :core)) ;; required for testing

(import-macros {: fn* : fn&} :macros.fn)

(deftest fn*
  (testing fn*-meta
    (fn* f
      "docstring"
      [x] x)
    (assert-eq (meta f) {:fnl/docstring "docstring"
                         :fnl/arglist ["x"]})

    (fn* f
      "docstring"
      ([x] x))
    (assert-eq (meta f) {:fnl/docstring "docstring"
                         :fnl/arglist ["x"]})

    (fn* f
      "docstring"
      ([x] x)
      ([x y] (+ x y)))
    (assert-eq (meta f) {:fnl/docstring "docstring"
                         :fnl/arglist ["\n  [x]"
                                       "\n  [x y]"]})

    (fn* f
      "docstring"
      ([x] x)
      ([x y] (+ x y))
      ([x y & z] (+ x y)))
    (assert-eq (meta f) {:fnl/docstring "docstring"
                         :fnl/arglist ["\n  [x]"
                                       "\n  [x y]"
                                       "\n  [x y & z]"]})))

(deftest fn&
  (testing fn&-meta
    (fn& f "docstring" [x] x)
    (assert-eq (meta f) {:fnl/docstring "docstring"
                         :fnl/arglist ["x"]})

    (fn& f "docstring" [...] [...])
    (assert-eq (meta f) {:fnl/docstring "docstring"
                         :fnl/arglist ["..."]})))
