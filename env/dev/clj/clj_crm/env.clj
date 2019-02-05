(ns clj-crm.env
  (:require [selmer.parser :as parser]
            [clojure.tools.logging :as log]
            [clj-crm.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[clj-crm started successfully using the development profile]=-"))
   :stop
   (fn []
     (log/info "\n-=[clj-crm has shut down successfully]=-"))
   :middleware wrap-dev})
