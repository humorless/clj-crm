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
  (spec/*
   (spec/keys :req-un
              [::year-month ::adaccount-corporate-name ::adaccount-id
               ::billing-tax-id ::revenue])))

(def ^:private columns-map
  {:A :year-month
   :B :adaccount-corporate-name
   :C :adaccount-id
   :E :billing-tax-id
   :F :revenue})

(defn- basic-mapping
  "handle the mapping that does not need to lookup any tables in database"
  [{y-m :year-month a-c-n :adaccount-corporate-name
    a-i :adaccount-id r :revenue}]
  (let [y-m-str (str (int y-m))]
    {:rev-stream/stream-unique-id a-i
     :rev-stream/campaign-name a-c-n
     :rev-stream/customer-id a-i
     :rev-stream/service-category-enum :product.type/timeline
     :rev-stream/writing-time (utility/y-m->dt y-m-str)
     :rev-stream/revenue (long r)
     :rev-stream/source :etl.source/lap}))

(defn- chan-mapping
  [table {debtor-key :billing-tax-id}]
  (let [chan-eid  (get table debtor-key)]
    (if chan-eid   ;; possibly nil
      {:rev-stream/channel chan-eid}
      {})))

(defn- data->data-txes
  [data]
  (let [db (d/db conn)
        table (utility/tax-id->c-eid db)]
    (let [basic-v (map basic-mapping data)
          chan-v  (map #(chan-mapping table %) data)]
      (->> (map merge basic-v chan-v)
           (mapv #(vector :fn/upsert-rev-stream %))))))

(def ^:private check-raw
  (utility/check-raw-fn ::rev-stream))

(def ^:private get-raw-from-excel
  (utility/get-raw-from-excel-fn columns-map))

(def sync-data
  (utility/sync-data-fn get-raw-from-excel check-raw data->data-txes))
