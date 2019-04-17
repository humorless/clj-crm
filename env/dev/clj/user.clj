(ns user
  (:require [clj-crm.config :refer [env]]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.tools.logging :as log]
            [clj-crm.db.core :refer [setup-app-db setup-db-fn]]
            [clj-crm.etl.core :refer [init-etl]]
            [mount.core :as mount]
            [clj-crm.figwheel :refer [start-fw stop-fw cljs]]
            [clj-crm.core :refer [start-app]]))

(alter-var-root #'s/*explain-out* (constantly expound/printer))

(def cancel-etl-f (atom (constantly "cancel function init state")))

(defn start []
  (mount/start-without #'clj-crm.core/repl-server)
  (log/info "prepare to install db schema")
  (setup-app-db "schema.edn")
  (setup-app-db "dev-preload-data.edn")
  (setup-db-fn)
  (let [f (init-etl)]
    (reset! cancel-etl-f f)))

(defn stop []
  (mount/stop-except #'clj-crm.core/repl-server)
  (@cancel-etl-f)) ;; call canceling function on ETL scheduler

(defn restart []
  (stop)
  (start))
