(ns clj-crm.etl.agp
  (:require
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [clj-crm.etl.utility :as utility]
   [clojure.spec.alpha :as spec]))

(spec/def ::year-month double?)
(spec/def ::neon-product-id double?)
(spec/def ::invoice-details string?)
(spec/def ::basic-id string?)
(spec/def ::customer-name string?)
(spec/def ::deptor-code string?)
(spec/def ::revenue double?)

(spec/def ::rev-stream
  (spec/keys :req-un
             [::year-month ::neon-product-id ::invoice-details ::basic-id
              ::customer-name ::deptor-code ::revenue]))

(def ^:private columns-map
  {:B :year-month
   :D :neon-product-id
   :G :invoice-details
   :K :basic-id
   :L :customer-name
   :O :deptor-code
   :AA :revenue})

(defn- data->data-txes
  [data]
  data)

(def ^:private check-raw
  (utility/check-raw-fn ::rev-stream))

(def ^:private get-raw-from-excel
  (utility/get-raw-from-excel-fn columns-map))

(def sync-data
  (utility/sync-data-fn get-raw-from-excel check-raw data->data-txes))
