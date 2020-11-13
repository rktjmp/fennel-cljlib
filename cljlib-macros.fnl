(local meta-enabled (pcall _SCOPE.specials.doc (list (sym :doc) (sym :doc)) _SCOPE _CHUNK))

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

(fn seq-fn []
  "Returns function that transforms tables and strings into sequences.

Sequential tables `[1 2 3 4]' are shallowly copied.

Assocative tables `{:a 1 :b 2}' are transformed into `[[:a 1] [:b 2]]'
with nondeterministic order.

Strings are transformed into a sequence of letters."
  `(fn [col#]
     (let [type# (type col#)
           res# (setmetatable {} {:cljlib/table-type :seq})
           insert# table.insert]
       (if (= type# :table)
           (do (var assoc?# false)
               (let [assoc-res# (setmetatable {} {:cljlib/table-type :seq})]
                 (each [k# v# (pairs col#)]
                   (if (and (not assoc?#)
                            (not (= (type k#) :number)))
                       (set assoc?# true))
                   (insert# res# v#)
                   (insert# assoc-res# [k# v#]))
                 (if assoc?# assoc-res# res#)))
           (= type# :string)
           (let [char# utf8.char]
             (each [_# b# (utf8.codes col#)]
               (insert# res# (char# b#)))
             res#)
           (= type# :nil) nil
           (error "expected table, string or nil" 2)))))

(fn with-meta [val meta]
  (if (not meta-enabled) val
      `(let [val# ,val
             (res# fennel#) (pcall require :fennel)]
         (if res#
             (each [k# v# (pairs ,meta)]
               (fennel#.metadata:set val# k# v#)))
         val#)))

(fn gen-arglist-doc [args]
  (if (list? (. args 1))
      (let [arglist []
            open (if (> (length args) 1) "\n  [" "")
            close (if (= open "") "" "]")]
        (each [i v (ipairs args)]
          (table.insert
           arglist
           (.. open (table.concat (gen-arglist-doc v) " ") close)))
        arglist)

      (sequence? (. args 1))
      (let [arglist []]
        (each [_ v (ipairs (. args 1))]
          (table.insert arglist (tostring v)))
        arglist)))

(fn multisym->sym [s]
  (if (multi-sym? s)
      (values (sym (string.gsub (tostring s) ".*[.]" "")) true)
      (values s false)))

(fn string? [x]
  (= (type x) "string"))

(fn has-amp? [args]
  ;; Check if arglist has `&' and return its position of `false'.
  ;; Performs additional checks for `&' and `...' usage in arglist.
  (var res false)
  (each [i s (ipairs args)]
    (if (= (tostring s) "&")
        (if res (assert-compile false "only one `&' can be specified in arglist." args)
            (set res i))
        (= (tostring s) "...")
        (assert-compile false "use of `...' in `defn' is not permitted. Use `&' if you want a vararg." args)
        (and res (> i (+ res 1)))
        (assert-compile false "only one `more' argument can be supplied after `&' in arglist." args)))
  res)

(fn gen-arity [[args & body]]
  ;; Forms three values, representing data needed to create dispatcher:
  ;;
  ;; - the length of arglist;
  ;; - the body of the function we generate;
  ;; - position of `&' in the arglist if any.
  (assert-compile (sequence? args) "defn: expected parameters table.

* Try adding function parameters as a list of identifiers in brackets." args)
  (values (length args)
          (list 'let [args ['...]] (list 'do ((or table.unpack _G.unpack) body)))
          (has-amp? args)))

(fn contains? [tbl x]
  (var res false)
  (each [i v (ipairs tbl)]
    (if (= v x)
        (do (set res i)
            (lua :break))))
  res)

(fn grows-by-one-or-equal? [tbl]
  (let [t []]
    (each [_ v (ipairs tbl)] (table.insert t v))
    (table.sort t)
    (var prev nil)
    (each [_ cur (ipairs t)]
      (if prev
          (when (and (not= (+ prev 1) cur)
                     (not= prev cur))
            (lua "return false")))
      (set prev cur))
    prev))

(fn arity-dispatcher [len fixed body& name]
  ;; Forms an `if' expression with all fixed arities first, then `&'
  ;; arity, if present, and default error message as last arity.
  ;;
  ;; `len' is a symbol, that represents the length of the current argument
  ;; list, and is computed at runtime.
  ;;
  ;; `fixed' is a table of arities with fixed amount of arguments.
  ;; These are put in this `if' as: `(= len fixed-len)', where
  ;; `fixed-len' is the length of current arity arglist, computed with
  ;; `gen-arity'.
  ;;
  ;; `body&' stores size of fixed part of arglist, that is, everything
  ;; up until `&', and the body itself.  When `body&' provided, the
  ;; `(>= len more-len)' is added to the resulting `if' expression.
  ;;
  ;; Lastly the catchall branch is added to `if' expression, which
  ;; ensures that only valid amount of arguments were passed to
  ;; function, which are defined by previous branches.
  (let [bodies '(if)
        lengths []]
    (var max nil)
    (each [fixed-len body (pairs (doto fixed))]
      (when (or (not max) (> fixed-len max))
        (set max fixed-len))
      (table.insert lengths fixed-len)
      (table.insert bodies (list '= len fixed-len))
      (table.insert bodies body))
    (when body&
      (let [[more-len body arity] body&]
        (assert-compile (not (and max (<= more-len max))) "defn: arity with `&' must have more arguments than maximum arity without `&'.

* Try adding more arguments before `&'" arity)
        (table.insert lengths (- more-len 1))
        (table.insert bodies (list '>= len (- more-len 1)))
        (table.insert bodies body)))
    (if (not (and (grows-by-one-or-equal? lengths)
                  (contains? lengths 0)))
        (table.insert bodies (list 'error
                             (.. "wrong argument amount"
                                 (if name (.. " for "  name) "")) 2)))
    bodies))

(fn single-arity-body [args fname]
  ;; Produces arglist and body for single-arity function.
  ;; For more info check `gen-arity' documentation.
  (let [[args & body] args
        (arity body amp) (gen-arity [args ((or table.unpack _G.unpack) body)])]
    `(let [len# (select :# ...)]
       ,(arity-dispatcher
         'len#
         (if amp {} {arity body})
         (if amp [amp body])
         fname))))

(fn multi-arity-body [args fname]
  ;; Produces arglist and all body forms for multi-arity function.
  ;; For more info check `gen-arity' documentation.
  (let [bodies {}   ;; bodies of fixed arity
        bodies& []] ;; bodies where arglist contains `&'
    (each [_ arity (ipairs args)]
      (let [(n body amp) (gen-arity arity)]
        (if amp
            (table.insert bodies& [amp body arity])
            (tset bodies n body))))
    (assert-compile (<= (length bodies&) 1)
                    "defn must have only one arity with `&':"
                    (. bodies& (length bodies&)))
    `(let [len# (select :# ...)]
       ,(arity-dispatcher
         'len#
         bodies
         (if (not= (next bodies&) nil)
             (. bodies& 1))
         fname))))

(fn defn [name doc? ...]
  "Create (anonymous) function of fixed arity.
Supports multiple arities by defining bodies as lists:

Named function of fixed arity 2:
(defn f [a b] (+ a b))

Function of fixed arities 1 and 2:
(defn ([x] x)
    ([x y] (+ x y)))

Named function of 2 arities, one of which accepts 0 arguments, and the
other one or more arguments:
(defn f
  ([] nil)
  ([x & xs]
   (print x)
   (f (unpack xs))))

Note, that this function is recursive, and calls itself with less and
less amount of arguments until there's no arguments, and the
zero-arity body is called.

Named functions accept additional documentation string before the
argument list:

(defn cube
     \"raise `x' to power of 3\"
     [x]
     (^ x 3))

(defn greet
     \"greet a `person', optionally specifying default `greeting'.\"
     ([person] (print (.. \"Hello, \" person \"!\")))
     ([greeting person] (print (.. greeting \", \" person \"!\"))))

Argument lists follow the same destruction rules as in `let'.
Variadic arguments with `...' are not supported.

If function name contains namespace part, defines local variable
without namespace part, then creates function with this name, sets
this function to the namespace, and returns it.  This roughly means,
that instead of writing this:

(local namespace {})
(fn f [x]
  (if (> x 0) (f (- x 1))))
(set namespace.f f)
(fn g [x] (f (* x 100)))
(set namespace.g g)

It is possible to write:

(local namespace {})
(defn namespace.f [x]
  (if (> x 0) (f (- x 1))))
(defn namespace.g [x] (f (* x 100)))

Note that it is still possible to call `f' and `g' in current scope
without namespace part.  `Namespace' will hold both functions as `f'
and `g' respectively."
  (assert-compile (not (string? name)) "defn expects symbol, vector, or list as first argument" name)
  (let [docstring (if (string? doc?) doc? nil)
        (name-wo-namespace namespaced?) (multisym->sym name)
        fname (if (sym? name-wo-namespace) (tostring name-wo-namespace))
        args (if (sym? name-wo-namespace)
                 (if (string? doc?) [...] [doc? ...])
                 [name-wo-namespace doc? ...])
        arglist-doc (gen-arglist-doc args)
        [x] args

        body (if (sequence? x) (single-arity-body args fname)
                 (list? x) (multi-arity-body args fname)
                 (assert-compile false "defn: expected parameters table.

* Try adding function parameters as a list of identifiers in brackets." x))]
    (if (sym? name-wo-namespace)
        (if namespaced?
            `(local ,name-wo-namespace
                    (do
                      (fn ,name-wo-namespace [...] ,docstring ,body)
                      (set ,name ,name-wo-namespace)
                      ,(with-meta name-wo-namespace `{:fnl/arglist ,arglist-doc :fnl/docstring ,docstring})))
            `(local ,name ,(with-meta `(fn ,name [...] ,docstring ,body) `{:fnl/arglist ,arglist-doc :fnl/docstring ,docstring})))
        (with-meta `(fn [...] ,docstring ,body) `{:fnl/arglist ,arglist-doc :fnl/docstring ,docstring}))))

(fn fn+ [name doc? args ...]
  "Create (anonymous) function.
Works the same as plain `fn' except supports automatic declaration of
namespaced functions.  See `defn' for more info."
  (assert-compile (not (string? name)) "defn expects symbol, vector, or list as first argument" name)
  (let [docstring (if (string? doc?) doc? nil)
        (name-wo-namespace namespaced?) (multisym->sym name)
        arg-list (if (sym? name-wo-namespace)
                     (if (string? doc?) args doc?)
                     name-wo-namespace)
        arglist-doc (gen-arglist-doc arg-list)
        body (if (sym? name)
                 (if (string? doc?)
                     [doc? ...]
                     [args ...])
                 [doc? args ...])]
    (if (sym? name-wo-namespace)
        (if namespaced?
            `(local ,name-wo-namespace
                    (do
                      (fn ,name-wo-namespace ,arg-list ,((or table.unpack _G.unpack) body))
                      (set ,name ,name-wo-namespace)
                      ,(with-meta name-wo-namespace `{:fnl/arglist ,arglist-doc :fnl/docstring ,docstring})))
            `(local ,name ,(with-meta `(fn ,name ,arg-list ,((or table.unpack _G.unpack) body)) `{:fnl/arglist ,arglist-doc :fnl/docstring ,docstring})))
        (with-meta `(fn ,arg-list ,((or table.unpack _G.unpack) body)) `{:fnl/arglist ,arglist-doc :fnl/docstring ,docstring}))))

(fn check-bindings [bindings]
  (and (assert-compile (sequence? bindings) "expected binding table" [])
       (assert-compile (= (length bindings) 2) "expected exactly two forms in binding vector." bindings)))

(fn if-let [...]
  (let [[bindings then else] (match (select :# ...)
                               2 [...]
                               3 [...]
                               _ (error "wrong argument amount for if-some" 2))]
    (check-bindings bindings)
    (let [[form test] bindings]
      `(let [tmp# ,test]
         (if tmp#
             (let [,form tmp#]
               ,then)
             ,else)))))

(fn when-let [...]
  (let [[bindings & body] (if (> (select :# ...) 0) [...]
                              (error "wrong argument amount for when-let" 2))]
    (check-bindings bindings)
    (let [[form test] bindings]
      `(let [tmp# ,test]
         (if tmp#
             (let [,form tmp#]
               ,((or table.unpack _G.unpack) body)))))))

(fn if-some [...]
  (let [[bindings then else] (match (select :# ...)
                               2 [...]
                               3 [...]
                               _ (error "wrong argument amount for if-some" 2))]
    (check-bindings bindings)
    (let [[form test] bindings]
      `(let [tmp# ,test]
         (if (= tmp# nil)
             ,else
             (let [,form tmp#]
               ,then))))))

(fn when-some [...]
  (let [[bindings & body] (if (> (select :# ...) 0) [...]
                              (error "wrong argument amount for when-some" 2))]
    (check-bindings bindings)
    (let [[form test] bindings]
      `(let [tmp# ,test]
         (if (= tmp# nil)
             nil
             (let [,form tmp#]
               ,((or table.unpack _G.unpack) body)))))))


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
           (= t# :nil) :nil
           (= t# :string) :string
           :else))))

(fn empty [tbl]
  (let [table-type (table-type tbl)]
    (if (= table-type :seq) `(setmetatable {} {:cljlib/table-type :seq})
        (= table-type :table) `(setmetatable {} {:cljlib/table-type :table})
        `(setmetatable {} {:cljlib/table-type (,(table-type-fn) ,tbl)}))))

(fn into [to from]
  (let [to-type (table-type to)
        from-type (table-type from)]
    (if (and (= to-type :seq) (= from-type :seq))
        `(let [to# (or ,to [])
               insert# table.insert]
           (each [_# v# (ipairs (or ,from []))]
             (insert# to# v#))
           (setmetatable to# {:cljlib/table-type :seq}))
        (= to-type :seq)
        `(let [to# (or ,to [])
               seq# ,(seq-fn)
               insert# table.insert]
           (each [_# v# (ipairs (seq# (or ,from [])))]
             (insert# to# v#))
           (setmetatable to# {:cljlib/table-type :seq}))
        (and (= to-type :table) (= from-type :seq))
        `(let [to# (or ,to [])]
           (each [_# [k# v#] (ipairs (or ,from []))]
             (tset to# k# v#))
           (setmetatable to# {:cljlib/table-type :table}))
        (and (= to-type :table) (= from-type :table))
        `(let [to# (or ,to [])
               from# (or ,from [])]
           (each [k# v# (pairs from#)]
             (tset to# k# v#))
           (setmetatable to# {:cljlib/table-type :table}))
        (= to-type :table)
        `(let [to# (or ,to [])
               from# (or ,from [])]
           (match (,(table-type-fn) from#)
             :seq (each [_# [k# v#] (ipairs from#)]
                    (tset to# k# v#))
             :table (each [k# v# (pairs from#)]
                      (tset to# k# v#))
             :else (error "expected table as second argument" 2))
           (setmetatable to# {:cljlib/table-type :table}))
        ;; runtime branch
        `(let [to# ,to
               from# ,from
               insert# table.insert
               table-type# ,(table-type-fn)
               seq# ,(seq-fn)
               to-type# (table-type# to#)
               to# (or to# []) ;; secure nil
               res# (match to-type#
                      :seq (do (each [_# v# (ipairs (seq# from#))]
                                 (insert# to# v#))
                               to#)
                      :table (match (table-type# from#)
                               :seq (do (each [_# [k# v#] (ipairs (or from# []))]
                                          (tset to# k# v#))
                                        to#)
                               :string (do (each [_# v# (ipairs (seq# from#))]
                                             (insert# to# v#))
                                           to#)
                               :table (do (each [k# v# (pairs (or from# []))]
                                            (tset to# k# v#))
                                          to#)
                               :empty to#
                               :else (error "expected table as second argument" 2))
                      ;; If we could not deduce type, it means that
                      ;; we've got empty table.  We use will default
                      ;; to sequential table, because it will never
                      ;; break when converting into
                      :empty (do (each [_# v# (ipairs (seq# from#))]
                                   (insert# to# v#))
                                 to#)
                      :nil (match (table-type# from#)
                             :nil nil
                             :empty to#
                             :seq (do (each [k# v# (pairs (or from# []))]
                                        (tset to# k# v#))
                                      to#)
                             :table (do (each [k# v# (pairs (or from# []))]
                                          (tset to# k# v#))
                                        to#)
                             :else (error "expected table as second argument" 2))
                      :else (error "expected table as first argument" 2))]
           (if res#
               (setmetatable res# {:cljlib/table-type (match to-type#
                                                        :seq :seq
                                                        :empty :seq
                                                        :table :table)}))))))

(fn first [tbl]
  (. tbl 1))

(fn rest [tbl]
  [((or table.unpack _G.unpack) tbl 2)])

(fn string? [x]
  (= (type x) :string))

(fn when-meta [...]
  (when meta-enabled
    `(do ,...)))

(fn meta [v]
  (when-meta
    `(let [(res# fennel#) (pcall require :fennel)]
       (if res# (. fennel#.metadata ,v)))))

(fn seq->table [seq]
  (let [tbl {}]
    (var v nil)
    (var (i k) (next seq))
    (while i
      (set (i v) (next seq i))
      (tset tbl k v)
      (set (i k) (next seq i)))
    tbl))

(fn defmulti [...]
  (let [[name & options] (if (> (select :# ...) 0) [...]
                          (error "wrong argument amount for defmulti"))
        docstring (if (string? (first options)) (first options))
        options (if docstring (rest options) options)
        dispatch-fn (first options)
        options (rest options)]
    (assert (= (% (length options) 2) 0) "wrong argument amount for defmulti")
    (let [options (seq->table options)]
      (if (in-scope? name)
          nil
          `(local ,name
                  (let [multimethods# {}]
                    (setmetatable
                     ,(with-meta {} {:fnl/docstring docstring})
                     {:__call
                      (fn [_# ...]
                        ,docstring
                        (let [dispatch-value# (,dispatch-fn ...)
                              (res# view#) (pcall require :fennelview)
                              tostr# (if res# view# tostring)]
                          ((or (. multimethods# dispatch-value#)
                               (. multimethods# (or (. ,options :default) :default))
                               (error (.. "No method in multimethod '"
                                          ,(tostring name)
                                          "' for dispatch value: "
                                          (tostr# dispatch-value#))
                                      2)) ...)))
                      :multimethods (setmetatable multimethods#
                                                  {:__index
                                                   (fn [tbl# key#]
                                                     (let [eq# ,(eq-fn)]
                                                       (var res# nil)
                                                       (each [k# v# (pairs tbl#)]
                                                         (when (eq# k# key#)
                                                           (set res# v#)
                                                           (lua :break)))
                                                       res#))})})))))))

(fn defmethod [multifn dispatch-val ...]
  (when (= (select :# ...) 0) (error "wrong argument amount for defmethod"))
  `(let [multifn# ,multifn]
     (tset (. (getmetatable multifn#) :multimethods)
           ,dispatch-val
           (do (defn f# ,...)
               f#))
     multifn#))

(fn def [...]
  (let [[attr-map name expr] (match (select :# ...)
                               2 [{} ...]
                               3 [...]
                               _ (error "wrong argument amount for def" 2))
        attr-map (if (table? attr-map) attr-map
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
        `(,f ,name ,(with-meta expr {:fnl/docstring docstring})))))

(fn defonce [...]
  (let [[attr-map name expr] (match (select :# ...)
                               2 [{} ...]
                               3 [...]
                               _ (error "wrong argument amount for def" 2))]
    (if (in-scope? name)
        nil
        (def attr-map name expr))))

{: defn
 : fn+
 : if-let
 : when-let
 : if-some
 : when-some
 : empty
 : into
 : when-meta
 : with-meta
 : meta
 : defmulti
 : defmethod
 : def
 : defonce}

;; LocalWords:  arglist fn runtime arities arity multi destructuring
;; LocalWords:  docstring Variadic LocalWords
