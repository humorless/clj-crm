(ns clj-crm.etl.raw
  (:require [clojure.set :as cs]
            [clojure.string :as s]
            [clojure.tools.logging :as log]
            [clj-crm.db.core :as dcore :refer [conn]]
            [datomic.api :as d]
            [dk.ative.docjure.spreadsheet :as spreadsheet]
            [clojure.java.io :as io])
  (:import [java.io StringWriter]))

(defn- service-category->enum
  "create a mapping table that can lookup enum from service category name."
  [db]
  (into {}  (d/q '[:find ?name ?enum
                   :where
                   [?e :product/service-category ?name]
                   [?e :product/type ?enum]]
                 db)))

(defn- get-raw-excel
  [uri]
  (with-open [stream (io/input-stream uri)]
    (->> (spreadsheet/load-workbook stream)
         (spreadsheet/select-sheet "Sheet0")
         (spreadsheet/select-columns {:E :io-writing-time
                                      :O :product-unique-id
                                      :V :service-category
                                      :AI :1
                                      :AJ :2
                                      :AK :3
                                      :AL :4
                                      :AM :5
                                      :AN :6
                                      :AO :7
                                      :AP :8
                                      :AQ :9
                                      :AR :10
                                      :AS :11
                                      :AT :12}))))

;; [:1 :2 :3 :4 ... :12]
(def month-fields (mapv #(keyword (str %)) (range 1 13)))

(defn- expand-orders
  "make a single order expand to 12 orders

  m is of the {HashMap} form that just reading the data from excel.
  Note that :1 :2 :3 ... :12 are nilable
  m is of the form:
  { :io-writing-time   XXX
    :product-unique-id XXX
    :service-category  XXX
    :1 XXX
    :2 XXX
    ... }

  Output value is: [
  { :io-writing-time   XXX
    :product-unique-id XXX
    :service-category  XXX
    :accounting-data      [\"2019-03\", 45]  }
  ...
  ]"
  [months m]
  (let [part-m (select-keys m [:io-writing-time :product-unique-id :service-category])
        gen-order (fn gen-order [k month-str]
                    (prn "debug gen-order" k month-str)
                    (assoc part-m :accounting-data [month-str (get m k)]))]
    (mapv gen-order month-fields months)))

;; (get-orders-from-excel (d/db conn) "http://127.0.0.1:5001/" "raw.xlsx")
(defn- get-orders-from-excel
  "Read the excel file, retrieve the orders data,
   and then transform the data into db-transaction-form

  Implementation details:
  rest - remove the title row
  set  - remove duplicated rows"
  [db addr filename]
  (let [p-table (service-category->enum db)
        sheet-data (get-raw-excel (str addr filename))
        titles (first sheet-data)
        title-months (mapv #(get titles %) month-fields)]
    (->> sheet-data
         rest
         (map #(expand-orders title-months)))))

(defn- order-eids [db]
  (d/q '[:find [?e ...] :where [?e :order/product-unique-id]] db))

(defn- eid->order [db eid]
  (d/pull db '[*] eid))

(defn- get-orders-from-db [db]
  (let [eids (order-eids db)
        tx-data (map #(eid->order db %) eids)]
    (set tx-data)))

(defn sync-data
  "A <= order table
   From Excel file, get the current $A
   From DB, get the $A inside DB.
   Calculate the difference. Find out the new $A in Excel but not in DB.
   Write into database"
  [url filename]
  (log/info "sync-data triggered!")
  (let [db (d/db conn)
        e-rel (get-orders-from-excel db url filename)
        d-rel (get-orders-from-db db)
        tx-data (vec (cs/difference e-rel d-rel))]
    (do (log/info "tx-data write into db, length: " (count tx-data))
        (log/info "first item of tx-data" (first tx-data))
        (when (seq tx-data)
          @(d/transact conn tx-data)))))
