(require-macros :macros.fn)
(local core {})
(local unpack (or table.unpack _G.unpack))
(local insert table.insert)
(local meta-enabled (pcall _SCOPE.specials.doc (list (sym :doc) (sym :doc)) _SCOPE _CHUNK))

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
           (let [meta# (getmetatable tbl#)
                 table-type# (and meta# (. meta# :cljlib/table-type))]
             (if table-type# table-type#
                 (let [(k# _#) (next tbl#)]
                   (if (and (= (type k#) :number) (= k# 1)) :seq
                       (= k# nil) :empty
                       :table))))
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

(fn& core.empty [tbl]
  (let [table-type (table-type tbl)]
    (if (= table-type :seq) `(setmetatable {} {:cljlib/table-type :seq})
        (= table-type :table) `(setmetatable {} {:cljlib/table-type :table})
        `(setmetatable {} {:cljlib/table-type (,(table-type-fn) ,tbl)}))))

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

(fn& core.when-meta [...]
  (when meta-enabled `(do ,...)))

(fn* core.with-meta [val meta]
  (if (not meta-enabled) val
      `(let [val# ,val
             (res# fennel#) (pcall require :fennel)]
         (if res#
             (each [k# v# (pairs ,meta)]
               (fennel#.metadata:set val# k# v#)))
         val#)))

(fn* core.meta [v]
  (when-meta
    `(let [(res# fennel#) (pcall require :fennel)]
       (if res# (. fennel#.metadata ,v)))))

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
                   ,(with-meta {} {:fnl/docstring docstring})
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
   (let [attr-map (if (table? attr-map) attr-map
                      (string? attr-map) {attr-map true}
                      (error "def: expected keyword or literal table as first argument" 2))
         (s multi) (multisym->sym name)
         docstring (or (. attr-map :doc)
                       (. attr-map :fnl/docstring))
         f (if (. attr-map :dynamic) 'var 'local)]
     (if multi
         `(,f ,s (do (,f ,s ,expr)
                     (set ,name ,s)
                     ,(with-meta s {:fnl/docstring docstring})))
         `(,f ,name ,(with-meta expr {:fnl/docstring docstring}))))))

(fn* core.defonce
  ([name expr]
   (defonce {} name expr))
  ([attr-map name expr]
   (if (in-scope? name)
       nil
       (def attr-map name expr))))

core
