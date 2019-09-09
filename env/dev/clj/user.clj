(ns user
  (:require [clj-crm.config :refer [env]]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.tools.logging :as log]
            [clj-crm.db.core :as dcore :refer [setup-app-db setup-db-fn]]
            [mount.core :as mount]
            [clj-crm.figwheel :refer [start-fw stop-fw cljs]]
            [clj-crm.core]))

(alter-var-root #'s/*explain-out* (constantly expound/printer))

(defn start []
  (mount/start-without #'clj-crm.core/repl-server)
  (log/info "prepare to install db schema")
  (setup-app-db "schema.edn")
  (setup-app-db "schema2.edn")
  (setup-app-db "dev-preload-data.edn")
  (setup-app-db "dev-preload-data2.edn")
  (dcore/setup-app-db* dcore/auxi-conn "schema3.edn")
  (setup-db-fn))

(defn stop []
  (mount/stop-except #'clj-crm.core/repl-server))

(defn restart []
  (stop)
  (start))
