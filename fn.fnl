(fn string? [x]
  (= (type x) "string"))

(fn has-amp? [args]
  (var res false)
  (each [_ s (ipairs args)]
    (when (= (tostring s) "&")
      (set res true)))
  res)

(fn gen-arity [[args & body]]
  (assert-compile (sequence? args) "fn* expects vector as it's parameter list.
Try wrapping arguments in square brackets." args)
  (let [arg-length (if (has-amp? args) (sym "_") (length args))
        body (list 'let [args [(sym "...")]] (unpack body))]
    (list arg-length body)))

(fn fn* [name doc? ...]
  (assert-compile (not (string? name)) "fn* expects symbol as function name" name)
  (let [docstring (if (string? doc?) doc? nil)
        args (if (sym? name)
                 (if (string? doc?) [...] [doc? ...])
                 [name doc? ...])
        [x & xs] args]
    (if (sequence? x)
        ;; Ordinary function
        (let [[args & body] args]
          (if (sym? name)
              `(fn ,name ,args ,docstring ,(unpack body))
              `(fn ,args ,docstring ,(unpack body))))
        ;; Multi-arity function
        (list? x)
        (let [bodies []]
          (each [_ arity (ipairs args)]
            (let [[arity body] (gen-arity arity)]
              (table.insert bodies arity)
              (table.insert bodies body)))
          `(fn ,name [...] ,docstring (match (length [...]) ,(unpack bodies)))))))

{: fn*}

;; (import-macros {: fn*} :fn) (macrodebug (fn* f ([a] a)))
