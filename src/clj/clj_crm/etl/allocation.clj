(ns clj-crm.etl.allocation
  (:require
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [clojure.string :as string]
   [clj-crm.etl.utility :as utility]
   [clojure.spec.alpha :as spec]))

(spec/def ::customer-id string?)
(spec/def ::product string?)
(spec/def ::sales string?)
(spec/def ::time inst?)

(spec/def ::allocation
  (spec/*
   (spec/keys :req-un
              [::customer-id ::product ::sales ::time])))

(def ^:private columns-map
  {:A :customer-id
   :B :product
   :C :sales
   :D :time})

(defn- service-category->enum
  "create a mapping table that can lookup enum from service category name."
  [db]
  (merge {"all" :product.cat/all}
         (utility/service-category->enum db)))

(defn- customer-id->eid
  "create a mapping table that can lookup eid from customer-id."
  [db]
  (into {}  (d/q '[:find ?id ?e
                   :where
                   [?e :customer/id ?id]]
                 db)))

(defn- chan-mapping
  "m is of the form:
   { :customer-id \"Customer ID\"
     :sales       \"Sales Name\"
     :product     \"LINE NOW\"
     :time        #inst         }"
  [c-table u-table p-table
   {c :customer-id u :sales p :product}]
  (let [c-eid (get c-table c)
        u-eid (get u-table u)
        p-enum (get p-table p)]
    (if (nil? c-eid) (throw (ex-info "c-eid is nil" {:causes c :desc "customer-id not matched"})))
    (if (nil? u-eid) (throw (ex-info "u-eid is nil" {:causes u :desc "sales not matched"})))
    (if (nil? p-enum) (throw (ex-info "p-enum is nil" {:causes p :desc "product not matched"})))
    {:allo/customer c-eid
     :allo/sales u-eid
     :allo/product p-enum}))

(defn- basic-mapping
  "handle the mapping that does not need to lookup any tables in database"
  [{t :time}]
  {:allo/time t})

(defn- data->data-txes
  [data]
  (let [db (d/db conn)
        c-table (customer-id->eid db)
        u-table (utility/user-name->u-eid db)
        p-table  (service-category->enum db)]
    (let [basic-xs (map basic-mapping data)
          chan-xs  (map #(chan-mapping c-table u-table p-table %) data)]
      (mapv merge basic-xs chan-xs))))

(def ^:private check-raw
  (utility/check-raw-fn ::allocation))

(def ^:private get-raw-from-excel
  (utility/get-raw-from-excel-fn columns-map))

(def sync-data
  (utility/sync-data-fn get-raw-from-excel check-raw data->data-txes))

(comment
  (def raw (get-raw-from-excel "http://10.20.30.40:5001/" "allocation.xlsx")))
