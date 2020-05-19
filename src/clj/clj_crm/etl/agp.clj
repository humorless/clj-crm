(ns clj-crm.etl.agp
  (:require
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [clojure.string :as string]
   [clj-crm.etl.utility :as utility]
   [clojure.spec.alpha :as spec]))

(spec/def ::service-category string?)
(spec/def ::year-month double?)
(spec/def ::invoice-details-id string?)
(spec/def ::invoice-details string?)
(spec/def ::basic-id string?)
(spec/def ::customer-name string?)
(spec/def ::debtor-code string?)
(spec/def ::revenue double?)
(spec/def ::ad-unit string?)
(spec/def ::billing-account-id string?)

(spec/def ::rev-stream
  (spec/*
   (spec/keys :req-un
              [::service-category ::year-month ::invoice-details-id
               ::invoice-details ::basic-id
               ::customer-name ::debtor-code ::revenue
               ::ad-unit ::billing-account-id])))

(def ^:private columns-map
  {:A :service-category
   :B :ad-unit
   :D :year-month
   :H :invoice-details-id
   :I :invoice-details
   :M :basic-id
   :N :customer-name
   :Q :debtor-code
   :R :billing-account-id
   :AF :revenue})

(defn- basic-mapping
  "handle the mapping that does not need to lookup any tables in database"
  [{y-m :year-month ivo-i :invoice-details-id ivo :invoice-details
    b-i :basic-id c-n :customer-name b-a-i :billing-account-id r :revenue
    ad-unit :ad-unit}]
  (let [y-m-str (str (int y-m))]
    {:rev-stream/stream-unique-id (str b-i "_" ivo-i)
     :rev-stream/campaign-name c-n
     :rev-stream/customer-id b-i
     :rev-stream/writing-time (utility/y-m->dt y-m-str)
     :rev-stream/accounting-time (utility/yearmonth->year-month y-m-str)
     :rev-stream/revenue (long r)
     :rev-stream/product-name ivo
     :rev-stream/ad-unit ad-unit
     :rev-stream/billing-account-id b-a-i
     :rev-stream/source :etl.source/agp}))

(defn- chan-mapping
  [table {d-c :debtor-code}]
  (let [trim-d-c (string/trim d-c)
        str-tuple (string/split trim-d-c #"-")
        debtor-key (first str-tuple)
        chan-eid  (get table debtor-key)]
    (when (nil? chan-eid)
      (log/info "chan-eid is nil, debtor-code is: " debtor-key))
    (if chan-eid   ;; possibly nil
      {:rev-stream/channel chan-eid}
      {})))

(defn- sc-mapping
  [table {sc :service-category}]
  (let [p-enum (get table sc)]
    (when (nil? p-enum)
      (throw (ex-info "p-enum is nil" {:causes sc :desc "product not matched"})))
    {:rev-stream/service-category-enum p-enum}))

(defn- data->data-txes
  [data]
  (let [db (d/db conn)
        table (utility/neon-code->c-eid db)
        p-table (utility/service-category->enum db)]
    (let [basic-xs (map basic-mapping data)
          chan-xs  (map #(chan-mapping table %) data)
          sc-xs (map #(sc-mapping p-table %) data)]
      (->> (map merge basic-xs chan-xs sc-xs)
           (mapv #(vector :fn/upsert-rev-stream %))))))

(def ^:private check-raw
  (utility/check-raw-fn ::rev-stream))

(def ^:private get-raw-from-excel
  (utility/get-raw-from-excel-fn columns-map))

(def sync-data
  (utility/sync-data-fn get-raw-from-excel check-raw data->data-txes))

(comment
  (def raw (get-raw-from-excel "http://10.20.30.40:5001/" "dev_agp.xlsx")))
