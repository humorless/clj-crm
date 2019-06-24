(ns clj-crm.etl.agp
  (:require
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [dk.ative.docjure.spreadsheet :as spreadsheet]
   [clojure.java.io :as io]
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

(defn- check-streams [data]
  (if (spec/valid? (spec/* ::rev-stream) data)
    data
    (throw (ex-info "schema error of rev-stream" {:causes data
                                                  :desc "agp schema validation error"}))))

(defn- get-rev-streams-from-excel
  "Read the excel file, retrieve the orders data,
 and then transform the data into db-transaction-form

 (get-rev-streams-from-excel \"http://127.0.0.1:5001/\" \"agp.xlsx\")"
  [addr filename]
  (with-open [stream (io/input-stream (str addr filename))]
    (let [title+orders (->> (spreadsheet/load-workbook stream)
                            (spreadsheet/select-sheet "Sheet0")
                            (spreadsheet/select-columns {:B :year-month
                                                         :D :neon-product-id
                                                         :G :invoice-details
                                                         :K :basic-id
                                                         :L :customer-name
                                                         :O :deptor-code
                                                         :AA :revenue}))]
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
