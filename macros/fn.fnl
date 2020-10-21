(local _unpack (or table.unpack unpack))
(local insert table.insert)

(fn string? [x]
  (= (type x) "string"))

(fn has-amp? [args]
  "Check if arglist has `&' and return its position of `false'.
Performs additional checks for `&' usage in arglist."

  (var res false)
  (each [i s (ipairs args)]
    (if (= (tostring s) "&")
        (if res (assert-compile false "only one `&' can be specified in arglist." args)
            (set res i))
        (and res (> i (+ res 1)))
        (assert-compile false "only one `more' arg can be supplied after `&' in arglist." args)))
  res)

(fn gen-arity [[args & body]]
  "Forms three values, representing data needed to create dispatcher:

- the lengs of arglist;
- the body of the function we generate;
- position of `&' in the arglist. "
  (assert-compile (sequence? args) "fn*: expected parameters table.
* Try adding function parameters as a list of identifiers in brackets." args)
  (values (length args)
          (list 'let [args ['...]] (_unpack body))
          (has-amp? args)))

(fn arity-dispatcher [len fixed amp-body name]
  "Forms an `if' expression with all fixed arities first, then `&'
arity, if present, and default error message as last arity.

`len' is a symbol, that represens the length of the current argumen
list, and is computed at runtime.

`fixed' is a table of arities with fixed amount of arguments. These are put in this `if' as:
`(= len fixed-len)', where `fixed-len' is the length of current arity arglist, computed with `gen-arity'.

`amp-body' stores size of fixed part of arglist, that is, everything up until `&'"
  (let [bodies '(if)]
    (each [i body (pairs (doto fixed))]
      (insert bodies (list '= len i))
      (insert bodies body))
    (when amp-body
      (let [[i body] amp-body]
        (insert bodies (list '>= len (- i 1)))
        (insert bodies body)))
    (insert bodies (list 'error
                         (.. "wrong argument amount"
                             (if name (.. " for "  name) "")) 3))
    bodies))

(fn single-arity-body [args fname]
  (let [[args & body] args
        (arity body amp) (gen-arity [args (_unpack body)])]
    `(let [len# (length [...])]
       ,(arity-dispatcher
         'len#
         (if amp {} {arity body})
         (if amp [amp body])
         fname))))

(fn multi-arity-body [args fname]
  (let [bodies {}
        amp-bodies {}]
    (each [_ arity (ipairs args)]
      (let [(n body amp) (gen-arity arity)]
        (if amp
            (do (insert amp-bodies amp)
                (insert amp-bodies body)
                (insert amp-bodies arity))
            (tset bodies n body))))
    (assert-compile (<= (length amp-bodies) 3)
                    "fn* must have only one arity with &:"
                    (. amp-bodies (length amp-bodies)))
    `(let [len# (length [...])]
       ,(arity-dispatcher
         'len#
         bodies
         (if (~= (next amp-bodies) nil)
             amp-bodies)
         fname))))

(fn fn* [name doc? ...]
  (assert-compile (not (string? name)) "fn* expects symbol as function name" name)
  (let [docstring (if (string? doc?) doc? nil)
        fname (if (sym? name) (tostring name))
        args (if (sym? name)
                 (if (string? doc?) [...] [doc? ...])
                 [name doc? ...])
        [x] args
        body (if (sequence? x) (single-arity-body args fname)
                 (list? x) (multi-arity-body args fname)
                 (assert-compile false "fn* expects vector as its arguments" x))]
    (if (sym? name)
        `(fn ,name [...] ,docstring ,body)
        `(fn [...] ,docstring ,body))))

{: fn*}

;; (import-macros {: fn*} :fn)
;; (fn* f ([a] a) ([a b] (+ a b)))
