(local test {})

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
               (set res# (= count-a# count-b#)))
             res#)
         (= a# b#))))

(fn test.assert-eq
  [expr1 expr2 msg]
  `(let [left# ,expr1
         right# ,expr2
         (res# view#) (pcall require :fennelview)
         eq# ,(eq-fn)
         tostr# (if res# view# tostring)]
     (assert (eq# left# right#)
             (or ,msg (.. "equality assertion failed
  Left: " (tostr# left#) "
  Right: " (tostr# right#) "\n")))))

(fn test.assert-ne
  [expr1 expr2 msg]
  `(let [left# ,expr1
         right# ,expr2
         (res# view#) (pcall require :fennelview)
         eq# ,(eq-fn)
         tostr# (if res# view# tostring)]
     (assert (not (eq# left# right#))
             (or ,msg (.. "unequality assertion failed
  Left: " (tostr# left#) "
  Right: " (tostr# right#) "\n")))))

(fn test.assert*
  [expr msg]
  `(assert ,expr (.. "assertion failed for "
                     (or ,msg ,(tostring expr)))))
(fn test.assert-not
  [expr msg]
  `(assert (not ,expr) (.. "assertion failed for "
                           (or ,msg ,(tostring expr)))))

(fn test.deftest
  [name ...]
  "Simple way of grouping tests"
  `(do ,...))

(fn test.testing
  [description ...]
  "Print test description and run it."
  `(do (io.stderr:write (.. "testing: " ,description "\n"))
       ,...))

test
