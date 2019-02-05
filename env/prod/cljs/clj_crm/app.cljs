(ns clj-crm.app
  (:require [clj-crm.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
