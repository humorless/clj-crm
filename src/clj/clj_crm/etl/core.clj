(ns clj-crm.etl.core
  (:require [clj-time.core :as t]
            [clj-time.periodic :refer [periodic-seq]]
            [chime :as chime]
            [clj-crm.etl.customer :as customer]
            [clj-crm.etl.user :as user]
            [clj-crm.etl.allocation :as allocation]
            [clj-crm.etl.raw :as raw]
            [clj-crm.etl.lap :as lap]
            [clj-crm.etl.agp :as agp]
            [clj-crm.etl.rev-allo :as rev-allo]
            [clj-crm.config :refer [env]]
            [mount.core :as mount])
  (:import [org.joda.time DateTimeZone]))

(mount/defstate url
  :start (:etl-url env)
  :stop "")

(defn sync-data
  "Switch on cmd to decide which `sync-data` function to use"
  [cmd filename]
  (case cmd
    "customer" (customer/sync-data url filename)
    "user" (user/sync-data url filename)
    "raw" (raw/sync-data url filename)
    "lap" (lap/sync-data url filename)
    "agp" (agp/sync-data url filename)
    "rev-allo" (rev-allo/sync-data url filename)
    "allocation" (allocation/sync-data url filename)))
