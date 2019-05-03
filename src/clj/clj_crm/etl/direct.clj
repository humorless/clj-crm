(ns clj-crm.etl.direct
  (:require [clojure.set :as cs]
            [clojure.string :as s]
            [clojure.tools.logging :as log]
            [clj-crm.db.core :as dcore :refer [conn]]
            [datomic.api :as d]
            [dk.ative.docjure.spreadsheet :as spreadsheet]
            [clojure.java.io :as io])
  (:import [java.io StringWriter]))

(defn- expand-sales
  "m is of the {HashMap} form that just reading the data from excel.
   Note that :display and :account are nilable
   m is of the form:
  { :customer-id \"Customer ID\"
    :display     \"Sales Name\"
    :account     \"Sales Name\"
    :time       #inst            }"
  [m]
  (let [acc (:account m)
        dis (:display m)
        part-m (dissoc m :display :account)
        acc-m (assoc part-m :sales acc :product :product.cat/account)
        dis-m (assoc part-m :sales dis :product :product.cat/display)]
    [acc-m dis-m]))

(defn- username->eid
  "create a mapping table that can lookup eid from user name."
  [db]
  (into {}  (d/q '[:find ?n ?e
                   :where
                   [?e :user/name ?n]]
                 db)))

(defn- customer-id->eid
  "create a mapping table that can lookup eid from customer-id."
  [db]
  (into {}  (d/q '[:find ?id ?e
                   :where
                   [?e :customer/id ?id]]
                 db)))

(defn- allo-m->allo-tx-m
  "m is of the form:
   { :customer-id \"Customer ID\"
     :sales       \"Sales Name\"
     :product     product-enumeration
     :time        #inst         }"
  [db c-table u-table  m]
  (let [cid (:customer-id m)
        s-name  (:sales m)
        c-eid (get c-table cid)
        u-eid (get u-table s-name)
        tx-m (assoc {} :allo/customer c-eid
                    :allo/sales u-eid
                    :allo/time (:time m)
                    :allo/product (:product m))]
    (if (nil? c-eid) (do (prn "c-eid is nil" m) (log/info "c-eid is nil " m)))
    (if (nil? u-eid) (do (prn "u-eid is nil " m) (log/info "u-eid is nil " m)))
    tx-m))

(defn- sales-not-nil [m]
  (not (nil? (:sales m))))

(defn- correct-data-cell [m]
  (let [cid-str (:customer-id m)
        sales-str (:sales m)]
    (and (string? cid-str)
         (string? sales-str))))

;; (get-allos-from-excel (d/db conn) "http://127.0.0.1:5001/" "direct.xlsx")
(defn- get-allos-from-excel
  "Read the excel file, retrieve the allo data,
   and then transform the data into db-transaction-form

  Implementation details:
  rest - remove the title row
  set  - remove duplicated rows"
  [db addr filename]
  (let [c-table (customer-id->eid db)
        u-table (username->eid db)]
    (with-open [stream (io/input-stream (str addr filename))]
      (->> (spreadsheet/load-workbook stream)
           (spreadsheet/select-sheet "Sheet0")
           (spreadsheet/select-columns {:A :customer-id
                                        :I :account
                                        :J :display
                                        :K :time})
           rest
           ;; (map prn) ;; enable when debugging
           (mapcat expand-sales)
           (filter sales-not-nil)
           (filter correct-data-cell)  ;; remove error excel cell value
           (map #(allo-m->allo-tx-m db c-table u-table %))
           set))))

(defn sync-data
  "A <= allocations table
   From Excel file, get the current $A
   Write into database"
  [url filename]
  (log/info "sync-data triggered!")
  (let [db (d/db conn)
        e-rel (get-allos-from-excel db url filename)
        tx-data (vec e-rel)]
    (do (log/info "tx-data write into db, length: " (count tx-data))
        (log/info "first item of tx-data" (first tx-data))
        (when (seq tx-data)
          @(d/transact conn tx-data)))))
