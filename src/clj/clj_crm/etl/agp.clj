(ns clj-crm.etl.agp
  (:require
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [clojure.string :as string]
   [clj-crm.etl.utility :as utility]
   [clojure.spec.alpha :as spec]))

(spec/def ::year-month double?)
(spec/def ::invoice-details-id string?)
(spec/def ::invoice-details string?)
(spec/def ::basic-id string?)
(spec/def ::customer-name string?)
(spec/def ::debtor-code string?)
(spec/def ::revenue double?)

(spec/def ::rev-stream
  (spec/*
   (spec/keys :req-un
              [::year-month ::invoice-details-id ::invoice-details ::basic-id
               ::customer-name ::debtor-code ::revenue])))

(def ^:private columns-map
  {:B :year-month
   :F :invoice-details-id
   :G :invoice-details
   :K :basic-id
   :L :customer-name
   :O :debtor-code
   :AA :revenue})

(defn- basic-mapping
  "handle the mapping that does not need to lookup any tables in database"
  [{y-m :year-month ivo-i :invoice-details-id ivo :invoice-details
    b-i :basic-id c-n :customer-name  r :revenue}]
  (let [y-m-str (str (int y-m))]
    {:rev-stream/stream-unique-id (str b-i "_" ivo-i)
     :rev-stream/campaign-name (str c-n "_" ivo)
     :rev-stream/customer-id b-i
     :rev-stream/service-category-enum :product.type/OA
     :rev-stream/writing-time (utility/y-m->dt y-m-str)
     :rev-stream/revenue (long r)
     :rev-stream/source :etl.source/agp}))

(defn- chan-mapping
  [table {d-c :debtor-code}]
  (let [str-tuple (string/split d-c #"-")
        debtor-key (first str-tuple)
        chan-eid  (get table debtor-key)]
    (if chan-eid   ;; possibly nil
      {:rev-stream/channel chan-eid}
      {})))

(defn- data->data-txes
  [data]
  (let [db (d/db conn)
        table (utility/neon-code->c-eid db)]
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
