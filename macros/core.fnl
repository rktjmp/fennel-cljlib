(import-macros {: fn*} :macros.fn)
(local _unpack (or table.unpack unpack))

(fn check-bindings [bindings]
  (assert-compile (sequence? bindings) "expected binding table

* Try placing a table here in square brackets containing identifiers to bind." bindings)
  (assert-compile (= (length bindings) 2) "expected exactly two forms in binding vector." bindings))

(fn* if-let
  ([bindings then]
   (if-let bindings then 'nil))
  ([bindings then else]
   (check-bindings bindings)
   (let [[form test] bindings]
     `(let [tmp# ,test]
        (if tmp#
            (let [,form tmp#]
              ,then)
            ,else)))))

(fn* when-let
  [bindings & body]
  (check-bindings bindings)
  (let [[form test] bindings]
    `(let [tmp# ,test]
       (if tmp#
           (let [,form tmp#]
             ,(_unpack body))))))

(fn* if-some
  ([bindings then]
   (if-some bindings then 'nil))
  ([bindings then else]
   (check-bindings bindings)
   (let [[form test] bindings]
     `(let [tmp# ,test]
        (if (= tmp# nil)
            ,else
            (let [,form tmp#]
              ,then))))))

(fn* when-some
  [bindings & body]
  (check-bindings bindings)
  (let [[form test] bindings]
    `(let [tmp# ,test]
       (if (= tmp# nil)
           nil
           (let [,form tmp#]
             ,(_unpack body))))))

{: if-let
 : when-let
 : if-some
 : when-some}
