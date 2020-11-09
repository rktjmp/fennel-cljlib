(require-macros :macros.fn)

(fn eq-fn []
  `(fn eq# [a# b#]
     (if (and (= (type a#) :table) (= (type b#) :table))
         (do (var [res# count-a# count-b#] [true 0 0])
             (each [k# v# (pairs a#)]
               (set res# (eq# v# (. b# k#)))
               (set count-a# (+ count-a# 1))
               (when (not res#) (lua :break)))
             (when res#
               (each [_# _# (pairs b#)]
                 (set count-b# (+ count-b# 1)))
               (set res# (and res# (= count-a# count-b#))))
             res#)
         (= a# b#))))

(fn* assert-eq
  ([expr1 expr2]
   (assert-eq expr1 expr2 nil))
  ([expr1 expr2 msg]
   `(let [left# ,expr1
          right# ,expr2
          (res# view#) (pcall require :fennelview)
          eq# ,(eq-fn)]
      (assert (eq# left# right#) (or ,msg (.. "equality assertion failed
  Left: " ((if res# view# tostring) left#) "
  Right: " ((if res# view# tostring) right#) "\n"))))))

(fn* assert-ne
  ([expr1 expr2]
   (assert-ne expr1 expr2 nil))
  ([expr1 expr2 msg]
   `(let [left# ,expr1
          right# ,expr2
          (res# view#) (pcall require :fennelview)
          eq# ,(eq-fn)]
      (assert (not (eq# left# right#)) (or ,msg (.. "unequality assertion failed
  Left: " ((if res# view# tostring) left#) "
  Right: " ((if res# view# tostring) right#) "\n"))))))

(fn* assert*
  ([expr]
   (assert* expr nil))
  ([expr msg]
   `(assert ,expr (.. "assertion failed for " (or ,msg ,(tostring expr))))))

(fn* deftest [name docstring & tests]
  "Simple way of grouping tests"
  `(do
     ,docstring
     ,((or table.unpack _G.unpack) tests)))

(fn* testing
  "Define test function, print its name and run it."
  [description & body]
  (let [test-name (gensym)]
    `(do (fn ,test-name []
           ,((or table.unpack _G.unpack) body))
         (io.stderr:write (.. "testing: " ,description "\n"))
         (,test-name))))

{: assert-eq
 : assert-ne
 : assert*
 : deftest
 : testing}
