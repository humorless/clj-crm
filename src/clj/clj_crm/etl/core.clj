(ns clj-crm.etl.core
  (:require
   [clj-crm.etl.customer]
   [clj-crm.etl.user]
   [clj-crm.etl.allocation]
   [clj-crm.etl.rev-allo]
   [clj-crm.etl.lamp]
   [clj-crm.etl.gui]
   [clj-crm.etl.lap]
   [clj-crm.etl.agp]
   [clj-crm.etl.target]
   [clj-crm.etl.pipeline]
   [clojure.tools.logging :as log]
   [clj-crm.config :refer [env]]
   [mount.core :as mount]))

(mount/defstate url
  :start (:etl-url env)
  :stop "")

(defn sync-data
  "Switch on cmd to decide which `sync-data` function to use"
  [cmd filename]
  (log/info "cmd as " cmd ", filename as " filename)
  (let [fn-sym (symbol (str "clj-crm.etl." cmd) "sync-data")
        etl (resolve fn-sym)]
    (log/info "fn-sym as " fn-sym ", etl function as " etl)
    (etl url filename)))
