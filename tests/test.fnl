(local test {})

(fn eq-fn []
  "Returns recursive equality function.

This function is able to compare tables of any depth, even if one of
the tables uses tables as keys."
  `(fn eq# [left# right#]
     (if (and (= (type left#) :table) (= (type right#) :table))
         (let [oldmeta# (getmetatable right#)]
           ;; In case if we'll get something like
           ;; (eq {[1 2 3] {:a [1 2 3]}} {[1 2 3] {:a [1 2 3]}})
           ;; we have to do even deeper search
           (setmetatable right# {:__index (fn [tbl# key#]
                                        (var res# nil)
                                        (each [k# v# (pairs tbl#)]
                                          (when (eq# k# key#)
                                            (set res# v#)
                                            (lua :break)))
                                        res#)})
           (var [res# count-a# count-b#] [true 0 0])
           (each [k# v# (pairs left#)]
             (set res# (eq# v# (. right# k#)))
             (set count-a# (+ count-a# 1))
             (when (not res#) (lua :break)))
           (when res#
             (each [_# _# (pairs right#)]
               (set count-b# (+ count-b# 1)))
             (set res# (= count-a# count-b#)))
           (setmetatable right# oldmeta#)
           res#)
         (= left# right#))))

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
