(import-macros {: fn*} :macros.fn)
;; requires `eq' from core.fnl to be available at runtime

(fn* assert-eq
  ([expr1 expr2]
   (assert-eq expr1 expr2 nil))
  ([expr1 expr2 msg]
   `(let [left# ,expr1
          right# ,expr2
          (res# view#) (pcall require :fennelview)]
      (assert (eq left# right#) (or ,msg (.. "equality assertion failed
  Left: " ((if res# view# tostring) left#) "
  Right: " ((if res# view# tostring) right#) "\n"))))))

(fn* assert-ne
  ([expr1 expr2]
   (assert-ne expr1 expr2 nil))
  ([expr1 expr2 msg]
   `(let [left# ,expr1
          right# ,expr2
          (res# view#) (pcall require :fennelview)]
      (assert (not (eq left# right#)) (or ,msg (.. "unequality assertion failed
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
  [name & body]
  (let [test-name (sym (.. (tostring name) "-test"))]
    `(do (fn ,test-name []
           ,((or table.unpack _G.unpack) body))
         (io.stderr:write (.. "running: " ,(tostring test-name) "\n"))
         (,test-name))))

{: assert-eq
 : assert-ne
 : assert*
 : deftest
 : testing}
