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
            [clj-time.coerce :as time.coerce]
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
    (throw (ex-info "schema error of mapping" {:causes (:mappings state)
                                               :desc "mapping schema error"}))))

(defn- check-orders [state]
  (if (every? #(spec/valid? ::order %) (:orders state))
    state
    (throw (ex-info "schema error of order" {:causes (:orders state)
                                             :desc "orders schema error"}))))

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
     :order/io-writing-time (time.coerce/to-date m-last-day)
     :order/accounting-data {:accounting/month y-m-str
                             :accounting/revenue (long revenue)}}))

(defn- not-nil-entry [[k v]]
  (if (some? v)
    true
    false))

(defn- remove-nil-entry
  ":order/customer and :order/channel may be nil"
  [order-tx]
  (->> order-tx
       (filter not-nil-entry)
       (into {})))

(defn- find-order-accounting-data-by-pui-date
  "Return `[e r]` when find order successfully.
   Return `nil` when find no orders."
  [db pui date]
  (d/q '[:find [?e ?r]
         :in $ ?pui ?y-m-str
         :where [?e :order/product-unique-id ?pui]
         [?e :order/accounting-data ?r]
         [?r :accounting/month ?y-m-str]] db pui date))

(defn- update-tx-for-accounting-data
  "checking accounting-data in existing db"
  [db order-tx-m]
  (let [pui (:order/product-unique-id order-tx-m)
        y-m-str (get-in order-tx-m [:order/accounting-data :accounting/month])]
    (if-some [[e r] (find-order-accounting-data-by-pui-date db pui y-m-str)]
      [[:db/retractEntity r] [:db/retract e :order/accounting-data r] order-tx-m]
      [order-tx-m])))

(defn- raw-orders->order-txes
  [state db]
  (let [{:keys [mappings orders order-title]} state
        y-m-str (:revenue order-title)
        mapping-vals (map #(vec (vals %)) mappings)
        mapping-table (into {} mapping-vals)
        c-table (tax-id->c-eid db)]
    (->> orders
         (map #(order->tx mapping-table c-table y-m-str %))
         (map #(remove-nil-entry %))
         (mapcat #(update-tx-for-accounting-data db %)))))

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
