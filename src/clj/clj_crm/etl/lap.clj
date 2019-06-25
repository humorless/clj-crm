(ns clj-crm.etl.lap
  (:require
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [clj-crm.etl.utility :as utility]
   [clojure.spec.alpha :as spec]))

(spec/def ::year-month double?)
(spec/def ::adaccount-corporate-name string?)
(spec/def ::adaccount-id string?)
(spec/def ::billing-tax-id string?)
(spec/def ::revenue double?)

(spec/def ::rev-stream
  (spec/keys :req-un
             [::year-month ::adaccount-corporate-name ::adaccount-id
              ::billing-tax-id ::revenue]))

(def ^:private columns-map
  {:A :year-month
   :B :adaccount-corporate-name
   :C :adaccount-id
   :E :billing-tax-id
   :F :revenue})

(defn- data->data-txes
  [data]
  data)

(def ^:private check-raw
  (utility/check-raw-fn ::rev-stream))

(def ^:private get-raw-from-excel
  (utility/get-raw-from-excel-fn columns-map))

(def sync-data
  (utility/sync-data-fn get-raw-from-excel check-raw data->data-txes))
