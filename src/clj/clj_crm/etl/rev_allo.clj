(ns clj-crm.etl.rev-allo
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [dk.ative.docjure.spreadsheet :as spreadsheet]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as spec]))

(spec/def ::writing-time inst?)
(spec/def ::customer-id string?)
(spec/def ::lamp-customer-id string?)
(spec/def ::sales-name string?)

(spec/def ::mapping
  (spec/keys :req-un
             [::writing-time ::customer-id ::lamp-customer-id ::sales-name]))

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
         (spreadsheet/select-columns {:A :writing-time
                                      :B :customer-id
                                      :C :lamp-customer-id
                                      :E :sales-name})
         rest)))

(defn- rev-allo-raw->rev-allo-txes
  "The transformation:

  Lookup `sales ref` by sales-name field
  Lookup `customer ref` by lamp-customer-id field
  Store the `customer-id` using db.type/string
  Store the `time` using db.type/inst
  Store the `source` using db.type/keyword"
  [db raw-map src])

;; Action with exception throwing
(defn- get-excel [addr filename]
  (-> {}
      (assoc :mappings (get-mappings-from-excel addr filename))
      (check-mappings)))

;; Assembly
(defn sync-data
  "read excel, database, and sync
   Assembly function"
  [url filename src]
  (log/info "sync-data triggered!")
  (let [raw-data-map (get-excel url filename)
        db (d/db conn)
        tx-retract-data (rev-allo-retract-txes db src)
        tx-data (rev-allo-raw->rev-allo-txes db raw-data-map src)]
    (do (log/info "tx-data write into db, length: " (count tx-data))
        (log/info "first item of tx-data" (first tx-data))
        (when (seq tx-data)
          @(d/transact conn tx-data)))))
