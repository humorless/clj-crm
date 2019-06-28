(ns clj-crm.parallel
  (:gen-class))

(defmacro plet [bindings & forms]
  (let [pairs (partition 2 bindings)
        names (map first pairs)
        vals (map second pairs)]
    `(let [[~@names] (pvalues ~@vals)] ~@forms)))
