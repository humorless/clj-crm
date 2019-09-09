(ns clj-crm.core
  (:require [clj-crm.handler :as handler]
            [clj-crm.nrepl :as nrepl]
            [luminus.http-server :as http]
            [clj-crm.config :refer [env]]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.tools.logging :as log]
            [clj-crm.db.core :as dcore  :refer [setup-app-db setup-db-fn]]
            [mount.core :as mount])
  (:gen-class))

(def cli-options
  [["-p" "--port PORT" "Port number"
    :parse-fn #(Integer/parseInt %)]])

(mount/defstate ^{:on-reload :noop} http-server
  :start
  (http/start
   (-> env
       (assoc  :handler #'handler/app)
       (update :io-threads #(or % (* 2 (.availableProcessors (Runtime/getRuntime)))))
       (update :port #(or (-> env :options :port) %))))
  :stop
  (http/stop http-server))

(mount/defstate ^{:on-reload :noop} repl-server
  :start
  (when (env :nrepl-port)
    (nrepl/start {:bind (env :nrepl-bind)
                  :port (env :nrepl-port)}))
  :stop
  (when repl-server
    (nrepl/stop repl-server)))

(defn stop-app []
  (doseq [component (:stopped (mount/stop))]
    (log/info component "stopped"))
  (shutdown-agents))

(defn start-app [args]
  (System/setProperty "datomic.objectCacheMax" "1g")
  (doseq [component (-> args
                        (parse-opts cli-options)
                        mount/start-with-args
                        :started)]
    (log/info component "started"))
  (log/info "install db schema")
  (setup-app-db "schema.edn") ;; setup app schema, idempotent operation
  (setup-app-db "schema2.edn") ;; setup app schema, idempotent operation
  (setup-app-db "preload-data.edn")
  (setup-db-fn)
  (dcore/setup-app-db* dcore/auxi-conn "schema3.edn")
  (.addShutdownHook (Runtime/getRuntime) (Thread. stop-app)))

(defn -main [& args]
  (start-app args))
