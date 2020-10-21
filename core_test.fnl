(import-macros {: fn*} :macros.fn)

(local {: seq
        : mapv
        : mapkv
        : reduce
        : reduce-kv
        : conj
        : cons
        : consj
        : first
        : rest
        : eq?
        : identity
        : comp
        : every?} (require :core))

;; Test equality function should be done first and with a lot of care,
;; because we rely on deep comparison in other tests.

(assert (eq? 1 1))
(assert (not (eq? 1 2)))
(assert (eq? 1 1 1 1 1))
(assert (eq? "1" "1" "1" "1" "1"))
(assert (eq? [1 2] [1 2]))
(assert (not (eq? [1] [1 2])))
(assert (not (eq? [1 2] [1])))
(assert (eq? [1 [2]] [1 [2]] [1 [2]]))
(assert (eq? [1 [2]] [1 [2]] [1 [2]]))
(assert (not (eq? [1 [2]] [1 [2]] [1 [2 [3]]])))

(fn* range
  ([upper] (range 0 upper 1))
  ([lower upper] (range lower upper 1))
  ([lower upper step]
   (let [res []]
     (for [i lower (- upper step) step]
       (table.insert res i))
     res)))

(assert (eq? (range 10) [0 1 2 3 4 5 6 7 8 9]))
(assert (eq? (range -5 5) [-5 -4 -3 -2 -1 0 1 2 3 4]))
;; (assert (eq? (range 0 1 0.2) [0 0.2 0.4 0.6 0.8])) ;; TODO: fails, unsure why.
