(ns clj-crm.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[clj-crm started successfully]=-"))
   :stop
   (fn []
     (log/info "\n-=[clj-crm has shut down successfully]=-"))
   :middleware identity})
