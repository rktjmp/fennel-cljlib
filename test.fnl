(import-macros {: fn*} :macros.fn)

;; requires `eq?' from core.fnl to be available at runtime

(fn* assert-eq
  ([expr1 expr2]
   (assert-eq expr1 expr2 'nil))
  ([expr1 expr2 msg]
  `(let [left# ,expr1
         right# ,expr2
         view# (require :fennelview)]
     (assert (eq? left# right#) (or ,msg (.. "equality assertion failed
  Left: " (view# left#) "
  Right: " (view# right#) "\n"))))))

(fn* assert-ne
  ([expr1 expr2]
   (assert-ne expr1 expr2 'nil))
  ([expr1 expr2 msg]
  `(let [left# ,expr1
         right# ,expr2
         view# (require :fennelview)]
     (assert (not (eq? left# right#)) (or ,msg (.. "unequality assertion failed
  Left: " (view# left#) "
  Right: " (view# right#) "\n"))))))

(fn* assert*
  ([expr]
   (assert* expr 'nil))
  ([expr msg]
  `(assert ,expr (.. "assertion failed for " (or ,msg ,(tostring expr))))))

(fn* test
  ;"define test function, print its name and run it."
  [name docstring & body]
  `(do (fn ,name []
         ,(or docstring nil)
         ((or table.unpack unpack) ,body))
       (io.stderr:write (.. "running: " ,(tostring name) "\n"))
       (,name)))

{: assert-eq
 : assert-ne
 : assert*
 : test}