(local unpack (or table.unpack _G.unpack))
(local insert table.insert)

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
        (assert-compile false "use of `...' in `fn*' is not permitted." args)
        (and res (> i (+ res 1)))
        (assert-compile false "only one `more' argument can be supplied after `&' in arglist." args)))
  res)

(fn gen-arity [[args & body]]
  ;; Forms three values, representing data needed to create dispatcher:
  ;;
  ;; - the length of arglist;
  ;; - the body of the function we generate;
  ;; - position of `&' in the arglist if any.
  (assert-compile (sequence? args) "fn*: expected parameters table.

* Try adding function parameters as a list of identifiers in brackets." args)
  (values (length args)
          (list 'let [args ['...]] (list 'do (unpack body)))
          (has-amp? args)))

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
  (let [bodies '(if)]
    (var max nil)
    (each [fixed-len body (pairs (doto fixed))]
      (when (or (not max) (> fixed-len max))
        (set max fixed-len))
      (insert bodies (list '= len fixed-len))
      (insert bodies body))
    (when body&
      (let [[more-len body arity] body&]
        (assert-compile (not (and max (<= more-len max))) "fn*: arity with `& more' must have more arguments than maximum arity without `& more'.

* Try adding more arguments before `&'" arity)
        (insert bodies (list '>= len (- more-len 1)))
        (insert bodies body)))
    (insert bodies (list 'error
                         (.. "wrong argument amount"
                             (if name (.. " for "  name) "")) 2))
    bodies))

(fn single-arity-body [args fname]
  ;; Produces arglist and body for single-arity function.
  ;; For more info check `gen-arity' documentation.
  (let [[args & body] args
        (arity body amp) (gen-arity [args (unpack body)])]
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
            (insert bodies& [amp body arity])
            (tset bodies n body))))
    (assert-compile (<= (length bodies&) 1)
                    "fn* must have only one arity with `&':"
                    (. bodies& (length bodies&)))
    `(let [len# (select :# ...)]
       ,(arity-dispatcher
         'len#
         bodies
         (if (~= (next bodies&) nil)
             (. bodies& 1))
         fname))))

(fn fn* [name doc? ...]
  "Create (anonymous) function of fixed arity.
Supports multiple arities by defining bodies as lists:

Named function of fixed arity 2:
(fn* f [a b] (+ a b))

Function of fixed arities 1 and 2:
(fn* ([x] x)
    ([x y] (+ x y)))

Named function of 2 arities, one of which accepts 0 arguments, and the
other one or more arguments:
(fn* f
  ([] nil)
  ([x & xs]
   (print x)
   (f (unpack xs))))

Note, that this function is recursive, and calls itself with less and
less amount of arguments until there's no arguments, and the
zero-arity body is called.

Named functions accept additional documentation string before the
argument list:

(fn* cube
     \"raise `x' to power of 3\"
     [x]
     (^ x 3))

(fn* greet
     \"greet a `person', optionally specifying default `greeting'.\"
     ([person] (print (.. \"Hello, \" person \"!\")))
     ([greeting person] (print (.. greeting \", \" person \"!\"))))

Note that functions created with `fn*' when inspected with `doc'
command will always show its arguments as `...', because the
resulting function actually accepts variable amount of arguments, but
we check the amount and doing destructuring in runtime.

(doc greet)

(greet ...)
  greet a `person', optionally specifying default `greeting'.

When defining multi-arity functions it is handy to include accepted
arities in the docstring.

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
(fn* namespace.f [x]
  (if (> x 0) (f (- x 1))))
(fn* namespace.g [x] (f (* x 100)))

Note that it is still possible to call `f' and `g' in current scope
without namespace part.  `Namespace' will hold both functions as `f'
and `g' respectively."
  (assert-compile (not (string? name)) "fn* expects symbol, vector, or list as first argument" name)
  (let [docstring (if (string? doc?) doc? nil)
        (name-wo-namespace namespaced?) (multisym->sym name)
        fname (if (sym? name-wo-namespace) (tostring name-wo-namespace))
        args (if (sym? name-wo-namespace)
                 (if (string? doc?) [...] [doc? ...])
                 [name-wo-namespace doc? ...])
        [x] args
        body (if (sequence? x) (single-arity-body args fname)
                 (list? x) (multi-arity-body args fname)
                 (assert-compile false "fn*: expected parameters table.

* Try adding function parameters as a list of identifiers in brackets." x))]
    (if (sym? name-wo-namespace)
        (if namespaced?
            `(local ,name-wo-namespace
                    (do
                      (fn ,name-wo-namespace [...] ,docstring ,body)
                      (set ,name ,name-wo-namespace)
                      ,name-wo-namespace))
            `(fn ,name [...] ,docstring ,body))
        `(fn [...] ,docstring ,body))))

(fn fn& [name doc? args ...]
  "Create (anonymous) function.
Works the same as plain `fn' except supports automatic declaration of
namespaced functions.  See `fn*' for more info."
  (assert-compile (not (string? name)) "fn* expects symbol, vector, or list as first argument" name)
  (let [docstring (if (string? doc?) doc? nil)
        (name-wo-namespace namespaced?) (multisym->sym name)
        arg-list (if (sym? name-wo-namespace)
                 (if (string? doc?) args doc?)
                 name-wo-namespace)
        body (if (sym? name)
                 (if (string? doc?)
                     [doc? ...]
                     [args ...])
                 [doc? args ...])]
    (if (sym? name-wo-namespace)
        (if namespaced?
            `(local ,name-wo-namespace
                    (do
                      (fn ,name-wo-namespace ,arg-list ,(unpack body))
                      (set ,name ,name-wo-namespace)
                      ,name-wo-namespace))
            `(fn ,name ,arg-list ,(unpack body)))
        `(fn ,arg-list ,(unpack body)))))

{: fn* : fn&}

;; LocalWords:  arglist fn runtime arities arity multi destructuring
;; LocalWords:  docstring Variadic LocalWords
