(ns clj-crm.fjr.time
  (:import (java.time LocalDate)
           (java.time.temporal IsoFields)))

(defn- year []
  (.getYear (LocalDate/now)))

(defn- quarter []
  (.get (LocalDate/now) IsoFields/QUARTER_OF_YEAR))

(defn quarter-month-str-set []
  (let [y (year)
        q (dec (quarter))]
    (let [m1 (+ 1 (* q 3))
          m2 (+ 2 (* q 3))
          m3 (+ 3 (* q 3))
          months  [m1 m2 m3]]
      (set
       (->> (map #(format "%02d" %) months)
            (map #(str y "-"  %)))))))
