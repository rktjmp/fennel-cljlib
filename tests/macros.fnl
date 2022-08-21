(require-macros :fennel-test)
(require-macros :init-macros)
(local (meta? fennel) (pcall require :fennel))

(fn meta [x]
  {:fnl/docstring (fennel.metadata:get x :fnl/docstring)
   :fnl/arglist (fennel.metadata:get x :fnl/arglist)})

(deftest test-let-variants
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

(deftest test-multimethods
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

  (testing "defmulti with multiple arity"
    (defmulti f (fn* ([x] x) ([x y] [x y])))
    (defmethod f :default ([_] :def) ([_ _] :def2))
    (defmethod f :4 ([x] (.. x :2)))
    (defmethod f [:4 :2] ([x y] 42))

    (assert-eq (f 0) :def)
    (assert-eq (f 0 1) :def2)
    (assert-eq (f :4) :42)
    (assert-eq (f :4 :2) 42)))

(deftest test-try
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

(deftest test-loop
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
