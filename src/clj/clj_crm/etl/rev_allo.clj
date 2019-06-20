(ns clj-crm.etl.rev-allo
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [dk.ative.docjure.spreadsheet :as spreadsheet]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as spec]))

(spec/def ::apply-time inst?)
(spec/def ::customer-id string?)
(spec/def ::lamp-customer-id string?)
(spec/def ::sales-name string?)
(spec/def ::source string?)

(spec/def ::mapping
  (spec/keys :req-un
             [::apply-time ::customer-id ::lamp-customer-id ::sales-name ::source]))

(defn- check-mappings [state]
  (if (every? #(spec/valid? ::mapping %) (:mappings state))
    state
    (throw (ex-info "schema error of mapping" {:causes (:mappings state)
                                               :desc "mapping schema error"}))))

;; (get-raw-orders-from-excel "http://127.0.0.1:5001/" "allocation.xlsx")
(defn- get-mappings-from-excel
  "Read the excel file, retrieve the orders data,
   and then transform the data into db-transaction-form"
  [addr filename]
  (with-open [stream (io/input-stream (str addr filename))]
    (->> (spreadsheet/load-workbook stream)
         (spreadsheet/select-sheet "Sheet0")
         (spreadsheet/select-columns {:A :apply-time
                                      :B :source
                                      :C :customer-id
                                      :D :lamp-customer-id
                                      :F :sales-name})
         rest)))

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
    (when (and (not= s-keyword :etl.source/lap) (not= s-keyword :etl.source/agp))
      (log/error :source s "source str error")
      (throw (ex-info "raw data error" {:causes raw-m
                                        :desc "convert source string to valid keyword failed"})))
    {:rev-allo/sales s-eid
     :rev-allo/customer c-eid
     :rev-allo/customer-id ci
     :rev-allo/time at
     :rev-allo/source s-keyword}))

(defn- rev-allo-raw->rev-allo-txes
  "The transformation:

  Lookup `sales ref` by sales-name field
  Lookup `customer ref` by lamp-customer-id field
  Store the `customer-id` using db.type/string
  Store the `time` using db.type/inst
  Store the `source` using db.type/keyword"
  [db state]
  (let [s-table (sales-name->eid db)
        c-table (lamp-customer-id->eid db)]
    (->> (:mappings state)
         (map #(raw->tx s-table c-table %)))))

;; Action with exception throwing


(defn- get-excel-and-validate [addr filename]
  (-> {}
      (assoc :mappings (get-mappings-from-excel addr filename))
      (check-mappings)))

;; Assembly
(defn sync-data
  "read excel, database, and sync
   Assembly function"
  [url filename]
  (log/info "sync-data triggered!")
  (let [state (get-excel-and-validate url filename)
        db (d/db conn)
        tx-data (rev-allo-raw->rev-allo-txes db state)]
    (do (log/info "tx-data write into db, length: " (count tx-data))
        (log/info "first item of tx-data" (first tx-data))
        (when (seq tx-data)
          @(d/transact conn tx-data)))))
