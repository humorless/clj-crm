(ns clj-crm.etl.utility
  (:require
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [dk.ative.docjure.spreadsheet :as spreadsheet]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as spec]))

(defn check-raw-fn
  "assemble schema and validate fn"
  [schema]
  (fn check-raw
    [data]
    (if (spec/valid? (spec/* schema) data)
      data
      (throw (ex-info "schema error of excel" {:causes data
                                               :desc (spec/explain-str (spec/* schema) data)})))))

(defn get-raw-from-excel-fn
  "assemble columns-map and get-raw-from-excel fn"
  [columns-map]
  (fn get-raw-from-excel
    [addr filename]
    (with-open [stream (io/input-stream (str addr filename))]
      (let [title+orders (->> (spreadsheet/load-workbook stream)
                              (spreadsheet/select-sheet "Sheet0")
                              (spreadsheet/select-columns columns-map))]
        (rest title+orders)))))

(defn sync-data-fn
  "assemble get, validate, transform, push to db"
  [get-raw-from-excel check-raw data->tx-data]
  (fn sync-data
    [url filename]
    (log/info "sync-data triggered!")
    (let [raw (get-raw-from-excel url filename)
          data (check-raw raw) ;; schema validation
          tx-data (data->tx-data data)]
      (do (log/info "tx-data write into db, length: " (count tx-data))
          (log/info "first item of tx-data" (first tx-data))
          (when (seq tx-data)
            @(d/transact conn tx-data))))))
