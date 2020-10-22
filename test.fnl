;; requires `eq?' from core.fnl to be available at runtime

(fn assert-eq [expr1 expr2 msg]
  `(let [left# ,expr1
         right# ,expr2
         view# (require :fennelview)]
     (assert (eq? left# right#) (or ,msg (.. "equality assertion failed
  Left: " (view# ,expr1) "
  Right: " (view# ,expr2) "\n")))))

(fn assert-ne [expr1 expr2 msg]
  `(let [left# ,expr1
         right# ,expr2
         view# (require :fennelview)]
     (assert (not (eq? left# right#)) (or ,msg (.. "unequality assertion failed
  Left: " (view# ,expr1) "
  Right: " (view# ,expr2) "\n")))))

(fn assert* [expr msg]
  `(assert ,expr (.. "assertion failed for " ,(or msg (tostring expr)))))

{: assert-eq
 : assert-ne
 : assert*}
