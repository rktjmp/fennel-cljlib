(import-macros {: fn*} :macros.fn)
(local unpack (or table.unpack _G.unpack))
(local insert table.insert)

(fn check-bindings [bindings]
  (and (assert-compile (sequence? bindings) "expected binding table" [])
       (assert-compile (= (length bindings) 2) "expected exactly two forms in binding vector." bindings)))

(fn* if-let
  ([bindings then]
   (if-let bindings then nil))
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
             ,(unpack body))))))

(fn* if-some
  ([bindings then]
   (if-some bindings then nil))
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
             ,(unpack body))))))


(fn table-type [tbl]
  (if (sequence? tbl) :seq
      (table? tbl) :table
      :else))

;; based on `seq' from `core.fnl'
(fn into [to from]
  (local to-type (table-type to))
  (local from-type (table-type from))
  `(let [to# ,to
         from# ,from
         insert# table.insert
         table-type# (fn [tbl#]
                       (let [t# (type tbl#)]
                         (if (= t# :table)
                             (let [(k# _#) (next tbl#)]
                               (if (and (= (type k#) :number) (= k# 1)) :seq
                                   (= k# nil) :empty
                                   :table))
                             :else)))
         seq# (fn [tbl#]
                (var assoc# false)
                (let [res# []]
                  (each [k# v# (pairs tbl#)]
                    (if (and (not assoc#)
                             (not (= (type k#) :number)))
                        (set assoc# true))
                    (insert# res# [k# v#]))
                  (if assoc# res# tbl#)))
         to-type# ,to-type
         to-type# (if (= to-type# :else)
                      (table-type# to#)
                      to-type#)
         from-type# ,from-type
         from-type# (if (= from-type# :else)
                        (table-type# from#)
                        from-type#)]
     (match to-type#
       :seq (do (each [_# v# (ipairs (seq# from#))]
                  (insert# to# v#)))
       :table (match from-type#
                :seq (each [_# [k# v#] (ipairs from#)]
                       (tset to# k# v#))
                :table (each [k# v# (pairs from#)]
                         (tset to# k# v#))
                :empty to#
                :else (error "expected table as second argument"))
       ;; If we could not deduce type, it means that
       ;; we've got empty table.  We use will default
       ;; to sequential table, because it will never
       ;; break when converting into
       :empty (do (each [_# v# (ipairs (seq# from#))]
                    (insert# to# v#)))
       :else (error "expected table as first argument"))
     to#))


{: if-let
 : when-let
 : if-some
 : when-some
 : into}
