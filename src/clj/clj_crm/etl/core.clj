(ns clj-crm.etl.core
  (:require [clj-time.core :as t]
            [clj-time.periodic :refer [periodic-seq]]
            [chime :as chime]
            [clj-crm.etl.customer :as customer]
            [clj-crm.etl.user :as user]
            [clj-crm.etl.allocation :as allocation]
            [clj-crm.etl.raw :as raw]
            [clj-crm.etl.lap :as lap]
            [clj-crm.config :refer [env]]
            [mount.core :as mount])
  (:import [org.joda.time DateTimeZone]))

(mount/defstate url
  :start (:etl-url env)
  :stop "")

(defn init-etl
  "Trigger timer to run ETL periodically.
   Run once a day at 13:00 "
  []
  (let [events-seq (rest (periodic-seq (.. (t/now)
                                           (withZone (DateTimeZone/forID "Asia/Taipei"))
                                           (withTime 13 0 0 0))
                                       (t/days 1)))]
    (chime/chime-at events-seq
                    (fn [ts]
                      (prn "at " ts "sync data")
                      (customer/sync-data))
                    {:on-finished (fn []
                                    (println "Schedule finished."))})))

(defn sync-data
  "Switch on cmd to decide which `sync-data` function to use"
  [cmd filename]
  (case cmd
    "customer" (customer/sync-data url filename)
    "user" (user/sync-data url filename)
    "raw" (raw/sync-data url filename)
    "lap" (lap/sync-data url filename)
    "allocation" (allocation/sync-data url filename)))
