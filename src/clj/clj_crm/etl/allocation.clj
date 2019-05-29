(ns clj-crm.etl.allocation
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
  (into {"all" :product.cat/all}
        (d/q '[:find ?name ?enum
               :where
               [?e :product/type-id ?name]
               [?e :product/type ?t]
               [?t :db/ident ?enum]]
             db)))

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
     :product     \"LINE NOW\"
     :time        #inst         }"
  [db c-table u-table p-table m]
  (let [cid (:customer-id m)
        s-name  (:sales m)
        p-name  (:product m)
        c-eid (get c-table cid)
        u-eid (get u-table s-name)
        p-enum (get p-table p-name)
        tx-m (assoc {} :allo/customer c-eid
                    :allo/product p-enum
                    :allo/sales u-eid
                    :allo/time (:time m))]
    (if (nil? c-eid) (do
                       (log/info "c-eid is nil " m)
                       (throw (ex-info "c-eid is nil" {:causes m :desc "customer not matched"}))))
    (if (nil? u-eid) (do (prn "u-eid is nil " m) (log/info "u-eid is nil " m)))
    (if (nil? p-enum) (do (prn "p-enum is nil " m) (log/info "p-enum is nil " m)))
    tx-m))

;; (get-allos-from-excel (d/db conn) "http://127.0.0.1:5001/" "allocation.xlsx")
(defn- get-allos-from-excel
  "Read the excel file, retrieve the allo data,
   and then transform the data into db-transaction-form

  Implementation details:
  rest - remove the title row
  set  - remove duplicated rows"
  [db addr filename]
  (let [c-table (customer-id->eid db)
        u-table (username->eid db)
        p-table (service-category->enum db)]
    (with-open [stream (io/input-stream (str addr filename))]
      (->> (spreadsheet/load-workbook stream)
           (spreadsheet/select-sheet "Sheet0")
           (spreadsheet/select-columns {:A :customer-id
                                        :B :product
                                        :C :sales
                                        :D :time})
           rest
           ;; (map prn) ;; enable when debugging
           (map #(allo-m->allo-tx-m db c-table u-table p-table %))
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
