(import-macros {: fn* : fn&} :macros.fn)
(local core {})
(local unpack (or table.unpack _G.unpack))
(local insert table.insert)

(fn multisym->sym [s]
  (if (multi-sym? s)
      (values (sym (string.gsub (tostring s) ".*[.]" "")) true)
      (values s false)))

(fn check-bindings [bindings]
  (and (assert-compile (sequence? bindings) "expected binding table" [])
       (assert-compile (= (length bindings) 2) "expected exactly two forms in binding vector." bindings)))

(fn* core.if-let
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

(fn* core.when-let
  [bindings & body]
  (check-bindings bindings)
  (let [[form test] bindings]
    `(let [tmp# ,test]
       (if tmp#
           (let [,form tmp#]
             ,(unpack body))))))

(fn* core.if-some
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

(fn* core.when-some
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

(fn table-type-fn []
  `(fn [tbl#]
     (let [t# (type tbl#)]
       (if (= t# :table)
           (let [(k# _#) (next tbl#)]
             (if (and (= (type k#) :number) (= k# 1)) :seq
                 (= k# nil) :empty
                 :table))
           :else))))

(fn seq-fn []
  `(fn [tbl#]
     (var assoc# false)
     (let [res# []
           insert# table.insert]
       (each [k# v# (pairs tbl#)]
         (if (and (not assoc#)
                  (not (= (type k#) :number)))
             (set assoc# true))
         (insert# res# [k# v#]))
       (if assoc# res# tbl#))))

(fn& core.into [to from]
  (let [to-type (table-type to)
        from-type (table-type from)]
    (if (and (= to-type :seq) (= from-type :seq))
        `(let [to# ,to
               insert# table.insert]
           (each [_# v# (ipairs ,from)]
             (insert# to# v#))
           to#)
        (= to-type :seq)
        `(let [to# ,to
               seq# ,(seq-fn)
               insert# table.insert]
           (each [_# v# (ipairs (seq# ,from))]
             (insert# to# v#))
           to#)
        (and (= to-type :table) (= from-type :seq))
        `(let [to# ,to]
           (each [_# [k# v#] (ipairs ,from)]
             (tset to# k# v#))
           to#)
        (and (= to-type :table) (= from-type :table))
        `(let [to# ,to
               from# ,from]
           (each [k# v# (pairs from#)]
             (tset to# k# v#))
           to#)
        (= to-type :table)
        `(let [to# ,to
               from# ,from]
           (match (,(table-type-fn) from#)
             :seq (each [_# [k# v#] (ipairs from#)]
                    (tset to# k# v#))
             :table (each [k# v# (pairs from#)]
                      (tset to# k# v#))
             :else (error "expected table as second argument"))
           to#)
        `(let [to# ,to
               from# ,from
               insert# table.insert
               table-type# ,(table-type-fn)
               seq# ,(seq-fn)]
           (match (table-type# to#)
             :seq (each [_# v# (ipairs (seq# from#))]
                    (insert# to# v#))
             :table (match (table-type# from#)
                      :seq (each [_# [k# v#] (ipairs from#)]
                             (tset to# k# v#))
                      :table (each [k# v# (pairs from#)]
                               (tset to# k# v#))
                      :else (error "expected table as second argument"))
             ;; If we could not deduce type, it means that
             ;; we've got empty table.  We use will default
             ;; to sequential table, because it will never
             ;; break when converting into
             :empty (each [_# v# (ipairs (seq# from#))]
                      (insert# to# v#))
             :else (error "expected table as first argument"))
           to#))))

(fn first [tbl]
  (. tbl 1))

(fn rest [tbl]
  [(unpack tbl 2)])

(fn string? [x]
  (= (type x) :string))

(fn* core.defmulti
  [name & opts]
  (let [docstring (if (string? (first opts)) (first opts))
        opts (if docstring (rest opts) opts)
        dispatch-fn (first opts)]
    (if (in-scope? name)
        nil
        `(local ,name
                (let [multimethods# {}]
                  (setmetatable
                   {}
                   {:__call
                    (fn [_# ...]
                      ,docstring
                      (let [dispatch-value# (,dispatch-fn ...)]
                        ((or (. multimethods# dispatch-value#)
                             (. multimethods# :default)
                             (error (.. "No method in multimethod '"
                                        ,(tostring name)
                                        "' for dispatch value: "
                                        dispatch-value#) 2)) ...)))
                    :multimethods multimethods#}))))))

(fn* core.defmethod
  [multifn dispatch-val & fn-tail]
  `(let [multifn# ,multifn]
     (tset (. (getmetatable multifn#) :multimethods)
           ,dispatch-val
           (fn ,(unpack fn-tail)))
     multifn#))

(fn* core.def
  ([name expr] (def {} name expr))
  ([attr-map name expr]
   (if (not (or (table? attr-map)
                (string? attr-map)))
       (error "def: expected keyword or literal table as first argument" 2))
   (let [(s multi) (multisym->sym name)
         f (if (if (table? attr-map)
                   (. attr-map :dynamic)
                   (if (= attr-map :dynamic)
                       true
                       (error (.. "unsupported attribute keyword: :" attr-map) 2)))
               'var 'local)]
     (if multi
         `(,f ,s (do (,f ,s ,expr) (set ,name ,s) ,s))
         `(,f ,name ,expr)))))

(fn* core.defonce
  ([name expr]
   (defonce {} name expr))
  ([attr-map name expr]
   (if (in-scope? (if (table? attr-map) name attr-map))
       nil
       (def attr-map name expr))))

core
