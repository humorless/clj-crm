(ns clj-crm.etl.core
  (:require [clj-time.core :as t]
            [clj-time.periodic :refer [periodic-seq]]
            [chime :as chime]
            [clj-crm.etl.lamp :as lamp]
            [clj-crm.etl.user :as user]
            [clj-crm.etl.raw :as raw]
            [clj-crm.etl.allocation :as allocation])
  (:import [org.joda.time DateTimeZone]))

(defn init-etl
  "Trigger timer to run ETL periodically.
   Run once a day at 13:00 "
  []
  (let [events-seq (rest (periodic-seq (.. (t/now)
                                           (withZone (DateTimeZone/forID "Asia/Taipei"))
                                           (withTime 13 0 0 0))
                                       (t/days 1)))]
    ;; (lamp/sync-data)
    (chime/chime-at events-seq
                    (fn [ts]
                      (prn "at " ts "sync data")
                      (lamp/sync-data))
                    {:on-finished (fn []
                                    (println "Schedule finished."))})))

(defn sync-data
  "Switch on cmd to decide which `sync-data` function to use"
  [cmd filename]
  (case cmd
    "customer" (lamp/sync-data filename)
    "user" (user/sync-data lamp/url filename)
    "raw" (raw/sync-data lamp/url filename)
    "allocation" (allocation/sync-data lamp/url filename)))
