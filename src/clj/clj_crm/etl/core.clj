(ns clj-crm.etl.core
  (:require [clj-time.core :as t]
            [clj-time.periodic :refer [periodic-seq]]
            [chime :as chime]
            [clj-crm.etl.lamp :as lamp]))

(defn init-etl
  "Trigger timer to run ETL periodically.
   Run once a day -> interval = 86400"
  [interval]
  (let [events-seq (rest (periodic-seq (t/now)
                                       (t/seconds interval)))]
    (lamp/sync-data)
    (chime/chime-at events-seq
                    (fn [ts]
                      (prn "at " ts "sync data")
                      (lamp/sync-data))
                    {:on-finished (fn []
                                    (println "Schedule finished."))})))
