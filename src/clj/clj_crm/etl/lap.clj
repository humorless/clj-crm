(ns clj-crm.etl.lap
  (:require
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [dk.ative.docjure.spreadsheet :as spreadsheet]
   [clojure.java.io :as io]
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

(defn- check-streams [data]
  (if (spec/valid? (spec/* ::rev-stream) data)
    data
    (throw (ex-info "schema error of rev-stream" {:causes data
                                                  :desc "lap schema validation error"}))))

(defn- get-rev-streams-from-excel
  "Read the excel file, retrieve the orders data,
 and then transform the data into db-transaction-form

 (get-rev-streams-from-excel \"http://127.0.0.1:5001/\" \"lap.xlsx\")"
  [addr filename]
  (with-open [stream (io/input-stream (str addr filename))]
    (let [title+orders (->> (spreadsheet/load-workbook stream)
                            (spreadsheet/select-sheet "Sheet0")
                            (spreadsheet/select-columns {:A :year-month
                                                         :B :adaccount-corporate-name
                                                         :C :adaccount-id
                                                         :E :billing-tax-id
                                                         :F :revenue}))]
      (rest title+orders))))

(defn- raw-streams->stream-txes
  [db streams]
  streams)

(defn sync-data
  "read excel, database, and sync
   Assembly function"
  [url filename]
  (log/info "sync-data triggered!")
  (let [db (d/db conn)
        rev-streams-raw (get-rev-streams-from-excel url filename)
        rev-streams (check-streams rev-streams-raw) ;; schema validation
        tx-data (raw-streams->stream-txes db rev-streams)]
    (do (log/info "tx-data write into db, length: " (count tx-data))
        (log/info "first item of tx-data" (first tx-data))
        (when (seq tx-data)
          @(d/transact conn tx-data)))))
