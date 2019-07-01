(ns clj-crm.etl.core
  (:require [clj-time.core :as t]
            [clj-time.periodic :refer [periodic-seq]]
            [chime :as chime]
            [clj-crm.etl.customer :as customer]
            [clj-crm.etl.user :as user]
            [clj-crm.etl.allocation :as allocation]
            [clj-crm.etl.rev-allo :as rev-allo]
            [clj-crm.etl.lamp :as lamp]
            [clj-crm.etl.gui :as gui]
            [clj-crm.etl.lap :as lap]
            [clj-crm.etl.agp :as agp]
            [clj-crm.etl.target :as target]
            [clj-crm.config :refer [env]]
            [mount.core :as mount])
  (:import [org.joda.time DateTimeZone]))

(mount/defstate url
  :start (:etl-url env)
  :stop "")

(defn sync-data
  "Switch on cmd to decide which `sync-data` function to use"
  [cmd filename]
  (let [fn-sym (symbol cmd "sync-data")
        sync-data (resolve fn-sym)]
    (sync-data url filename)))
