(import-macros {: fn*} :macros.fn)
(local _unpack (or table.unpack unpack))
(local insert table.insert)

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


;; based on `seq' from `core.fnl'
(fn into [to from]
  (if (sequence? to)
      `(let [to# ,to
             from# ,from
             insert# table.insert
             unpack# (or table.unpack unpack)
             res# []]
         (var assoc# false)
         (each [k# v# (pairs from#)]
           (if (and (not assoc#)
                    (not (= (type k#) "number")))
               (set assoc# true))
           (insert# res# [k# v#]))
         (let [res# (if assoc# res# from#)]
           (if (~= (next to#) nil)
               (do (when (~= (next res#) nil)
                     (each [_# v# (ipairs res#)]
                       (insert# to# v#)))
                   to#)
               res#)))
      ;; to support (into {} {}) we first need transform `from' into a
      ;; sequential table.  Unfortunately it seems impossible to do
      ;; this with `(into [] ,from)' call, as it results in infinity
      ;; compilation loop.  Because of that the body of previous
      ;; branch is repeated here almost entirely, although without
      ;; some extra checks, as these aren't necessary in this case.
      (table? to)
      `(let [to# ,to
             from# (let [from# ,from
                         insert# table.insert
                         unpack# (or table.unpack unpack)
                         res# []]
                     (var assoc# false)
                     (each [k# v# (pairs from#)]
                       (if (and (not assoc#)
                                (not (= (type k#) "number")))
                           (set assoc# true))
                       (insert# res# [k# v#]))
                     (if assoc# res# from#))]
         (each [_# [k# v#] (ipairs from#)]
           (tset to# k# v#))
         to#)
      `(error "expected table as the first argument" 2)))

{: if-let
 : when-let
 : if-some
 : when-some
 : into}
