(ns clj-crm.etl.rev-allo
  (:require
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [clj-crm.etl.utility :as utility]
   [clojure.spec.alpha :as spec]))

(spec/def ::apply-time inst?)
(spec/def ::customer-id string?)
(spec/def ::lamp-customer-id string?)
(spec/def ::sales-name string?)
(spec/def ::source #{"agp" "lap"})

(spec/def ::mapping
  (spec/*
   (spec/keys :req-un
              [::apply-time ::customer-id ::lamp-customer-id ::sales-name ::source])))

(def ^:private columns-map
  {:A :apply-time
   :B :source
   :C :customer-id
   :D :lamp-customer-id
   :F :sales-name})

(defn- sales-name->eid
  [db]
  (->> (d/q '[:find ?n ?e
              :where [?e :user/name ?n]] db)
       (into {})))

(defn- lamp-customer-id->eid
  [db]
  (->> (d/q '[:find ?n ?e
              :where [?e :customer/id ?n]] db)
       (into {})))

(defn- raw->tx
  [s-table c-table raw-m]
  (let [{sn :sales-name at :apply-time ci :customer-id
         lci :lamp-customer-id s :source} raw-m
        s-eid (get s-table sn)
        c-eid (get c-table lci)
        s-keyword (keyword "etl.source" s)]
    (when (nil? c-eid)
      (log/error :lamp-customer-id lci  "has no mapping in database")
      (throw (ex-info "raw data error" {:causes raw-m
                                        :desc "lookup :rev-allo/customer failed"})))
    (when (nil? s-eid)
      (log/error :sales-name sn  "has no mapping in database")
      (throw (ex-info "raw data error" {:causes raw-m
                                        :desc "lookup :rev-allo/sales failed"})))
    {:rev-allo/sales s-eid
     :rev-allo/customer c-eid
     :rev-allo/customer-id ci
     :rev-allo/time (utility/dt->dt-tz at)
     :rev-allo/source s-keyword}))

(defn- data->data-txes
  "The transformation:

  Lookup `sales ref` by sales-name field
  Lookup `customer ref` by lamp-customer-id field
  Store the `customer-id` using db.type/string
  Store the `time` using db.type/inst
  Store the `source` using db.type/keyword"
  [data]
  (let [db (d/db conn)
        s-table (sales-name->eid db)
        c-table (lamp-customer-id->eid db)]
    (->> data
         (map #(raw->tx s-table c-table %)))))

(def ^:private check-raw
  (utility/check-raw-fn ::mapping))

(def ^:private get-raw-from-excel
  (utility/get-raw-from-excel-fn columns-map))

(def sync-data
  (utility/sync-data-fn get-raw-from-excel check-raw data->data-txes))

(comment (def raw
           (get-raw-from-excel "/home/vagrant/clo/clj-crm/etl_test/" "dev_rev_allo.xlsx")))
