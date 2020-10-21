(local _unpack (or table.unpack unpack))

(fn string? [x]
  (= (type x) "string"))

(fn has-amp? [args]
  (var res false)
  (each [i s (ipairs args)]
    (when (= (tostring s) "&")
      (set res i)))
  res)

(fn gen-arity [[args & body]]
  (assert-compile (sequence? args) "fn* expects vector as it's parameter list.
Try wrapping arguments in square brackets." args)
  (list (length args)
        (list 'let [args ['...]] (_unpack body))
        (has-amp? args)))

(fn arity-dispatcher [size fixed amp-body name]
  (let [bodies []]
    (each [i body (pairs (doto fixed))]
      (table.insert bodies (list '= size i))
      (table.insert bodies body))
    (when amp-body
      (let [[i body] amp-body]
        (table.insert bodies (list '>= size (- i 1)))
        (table.insert bodies body)))
    (table.insert
     bodies
     (list 'error
           (.. "wrong argument amount"
               (if name (.. " for "  name) "")) 3))
    (list 'if (_unpack bodies))))


(fn fn* [name doc? ...]
  (assert-compile (not (string? name)) "fn* expects symbol as function name" name)
  (let [docstring (if (string? doc?) doc? nil)
        fname (if (sym? name) (tostring name))
        args (if (sym? name)
                 (if (string? doc?) [...] [doc? ...])
                 [name doc? ...])
        [x] args
        body (if (sequence? x)
                 ;; Single-arity function
                 (let [[args & body] args
                       [arity body amp] (gen-arity [args (_unpack body)])]
                   `(let [len# (length [...])]
                      ,(arity-dispatcher
                        'len#
                        (if amp {} {arity body})
                        (if amp [amp body])
                        fname)))
                 ;; Multi-arity function
                 (list? x)
                 (let [bodies {}
                       amp-bodies {}]
                   (each [_ arity (ipairs args)]
                     (let [[n body amp] (gen-arity arity)]
                       (if amp
                           (do (table.insert amp-bodies amp)
                               (table.insert amp-bodies body)
                               (table.insert amp-bodies arity))
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
                        fname)))
                 (assert-compile false "fn* expects vector as its arguments" x))]
    (if (sym? name)
        `(fn ,name [...] ,docstring ,body)
        `(fn [...] ,docstring ,body))))

{: fn*}

;; (import-macros {: fn*} :fn)
;; (fn* f ([a] a) ([a b] (+ a b)))
