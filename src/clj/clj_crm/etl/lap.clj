(ns clj-crm.etl.lap
  (:require [clojure.set :as cs]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [clj-crm.db.core :as dcore :refer [conn]]
            [datomic.api :as d]
            [dk.ative.docjure.spreadsheet :as spreadsheet]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clj-time.format :as time.format]
            [clj-time.core :as time.core])
  (:import [java.io StringWriter]))

(spec/def ::adaccount-id string?)
(spec/def ::billing-tax-id string?)
(spec/def ::revenue double?)
(spec/def ::advertiser-tax-id string?)

(spec/def ::order
  (spec/keys :req-un
             [::adaccount-id ::billing-tax-id ::revenue]))

(spec/def ::mapping
  (spec/keys :req-un
             [::adaccount-id ::advertiser-tax-id]))
;; (get-raw-orders-from-excel "http://127.0.0.1:5001/" "allocation.xlsx")
(defn- get-raw-orders-from-excel
  "Read the excel file, retrieve the orders data,
 and then transform the data into db-transaction-form"
  [addr filename]
  (with-open [stream (io/input-stream (str addr filename))]
    (let [title+orders (->> (spreadsheet/load-workbook stream)
                            (spreadsheet/select-sheet "Sheet0")
                            (spreadsheet/select-columns {:B :adaccount-id
                                                         :F :billing-tax-id
                                                         :K :revenue}))]
      {:order-title (first title+orders)
       :orders (rest title+orders)})))

(defn- get-mappings-from-excel
  "Read the excel file, retrieve the orders data,
   and then transform the data into db-transaction-form"
  [addr filename]
  (with-open [stream (io/input-stream (str addr filename))]
    (->> (spreadsheet/load-workbook stream)
         (spreadsheet/select-sheet "Sheet1")
         (spreadsheet/select-columns {:D :adaccount-id
                                      :G :advertiser-tax-id})
         rest)))

(defn- check-mappings [state]
  (if (every? #(spec/valid? ::mapping %) (:mappings state))
    state
    (throw (Exception. "schema error of mapping"))))

(defn- check-orders [state]
  (if (every? #(spec/valid? ::order %) (:orders state))
    state
    (throw (Exception. "schema error of order"))))

(defn- tax-id->c-eid
  "create a mapping table that can lookup customer-eid from customer tax-id."
  [db]
  (into {}  (d/q '[:find ?tax-id ?e
                   :where
                   [?e :customer/tax-id ?tax-id]]
                 db)))

(def td-fmt-y-m (time.format/formatter "yyyy-MM"))

(defn- date-str->dt [y-m-str]
  (time.format/parse td-fmt-y-m y-m-str))

(defn- order->tx [mapping-table c-table y-m-str order]
  (let [{:keys [adaccount-id billing-tax-id revenue]} order
        c-tax-id (get mapping-table billing-tax-id)
        c-eid (get c-table c-tax-id)
        chan-eid (get c-table billing-tax-id)
        dt (date-str->dt y-m-str)
        m-last-day (time.core/plus- (time.core/last-day-of-the-month- dt) (time.core/hours 23))]
    {:order/product-unique-id adaccount-id
     :order/customer c-eid
     :order/channel chan-eid
     :order/service-category-enum :product.type/timeline
     :order/io-writing-time m-last-day
     :order/accouning-data {:accounting/month y-m-str
                            :accounting/revenue revenue}}))

(defn- raw-orders->order-txes
  [state db]
  (let [{:keys [mappings orders order-title]} state
        y-m-str (:revenue order-title)
        mapping-vals (map #(vec (vals %)) mappings)
        mapping-table (into {} mapping-vals)
        c-table (tax-id->c-eid db)]
    (->> orders
         (map #(order->tx mapping-table c-table y-m-str %)))))

(defn- process-excel [db addr filename]
  (-> {}
      (assoc :mappings (get-mappings-from-excel addr filename))
      (check-mappings)
      (merge (get-raw-orders-from-excel addr filename))
      (check-orders)
      (raw-orders->order-txes db)))

(defn sync-data
  "A <= orders table
   From Excel file, get the current $A
   Write into database"
  [url filename]
  (log/info "sync-data triggered!")
  (let [db (d/db conn)
        tx-data (process-excel db url filename)]
    (do (log/info "tx-data write into db, length: " (count tx-data))
        (log/info "first item of tx-data" (first tx-data))
        (when (seq tx-data)
          @(d/transact conn tx-data)))))
