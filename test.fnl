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

(fn walk-tree [root f custom-iterator]
  "Walks a tree (like the AST), invoking f(node, idx, parent) on each node.
When f returns a truthy value, recursively walks the children."
  (fn walk [iterfn parent idx node]
    (when (f idx node parent)
      (each [k v (iterfn node)]
        (walk iterfn node k v))))
  (walk (or custom-iterator pairs) nil nil root)
  root)

(fn* assert*
  ([expr]
   (assert* expr 'nil))
  ([expr msg]
   `(assert ,expr (.. "assertion failed for " (or ,msg ,(tostring expr))))))

(fn* test
  ;"define test function, print its name and run it."
  [name docstring & body]
  (let [test-name (sym (.. (tostring name) "-test"))]
    `(do (fn ,test-name []
           ,(or docstring nil)
           ((or table.unpack unpack) ,body))
         (io.stderr:write (.. "running: " ,(tostring test-name) "\n"))
         (,test-name))))

{: assert-eq
 : assert-ne
 : assert*
 : test}
