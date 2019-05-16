(ns clj-crm.etl.raw
  (:require [clojure.set :as cs]
            [clojure.string :as string]
            [clojure.instant :as instant]
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
                   [?e :product/type-id ?name]
                   [?e :product/type ?t]
                   [?t :db/ident ?enum]]
                 db)))

(defn- tax-id->c-eid
  "create a mapping table that can lookup customer-eid from customer tax-id."
  [db]
  (into {}  (d/q '[:find ?tax-id ?e
                   :where
                   [?e :customer/tax-id ?tax-id]]
                 db)))

(defn- get-raw-excel
  [uri]
  (with-open [stream (io/input-stream uri)]
    (->> (spreadsheet/load-workbook stream)
         (spreadsheet/select-sheet "Sheet0")
         (spreadsheet/select-columns {:E :io-writing-time
                                      :J :tax-id
                                      :N :debtor-tax-id
                                      :O :product-unique-id
                                      :V :service-category
                                      :AC :terms-start-date
                                      :AD :terms-end-date
                                      :AG :product-net-price
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

(def property-fields [:io-writing-time :tax-id :debtor-tax-id :product-unique-id
                      :service-category :terms-start-date :terms-end-date :product-net-price])

(def order-property-fields
  (mapv #(keyword (str "order") (name %))
        [:product-unique-id :customer :channel :service-category-enum
         :io-writing-time :product-net-price :terms-start-date :terms-end-date]))

(defn- expand-order
  "make a single order expand to 12 orders

  m is of the {HashMap} form that just reading the data from excel.
  Note that :1 :2 :3 ... :12 are nilable
  m is of the form:
  { :io-writing-time   XXX
    :tax-id            XXX
    :product-unique-id XXX
    :service-category  XXX
    :1 XXX
    :2 XXX
    ... }

  Output value is: [
  { :io-writing-time   XXX
    :tax-id            XXX
    :product-unique-id XXX
    :service-category  XXX
    :accounting-data   [\"2019-03\", 45]  }
  ...
  ]"
  [title-table m]
  (let [part-m (select-keys m property-fields)
        gen-order (fn gen-order [k]
                    (assoc part-m :accounting-data [(k title-table) (k m)]))]
    (mapv gen-order month-fields)))

(defn- revenue-number? [m]
  (number? (second (:accounting-data m))))

(defn- order-m->revenue-tx
  "Use suitable data type for each fields:
   #inst         -> io-writing-time
   db.type/ref   -> customer
   db.type/ref   -> service-category-enum"
  [c-table p-table m]
  (let [[date clock-time time-zone] (string/split (:io-writing-time m) #" ")
        t (instant/read-instant-date (str date "T" clock-time "+08:00"))  ;; Default: Use Taipei TimeZone
        c-eid (get c-table (:tax-id m))
        chan-eid (get c-table (:debtor-tax-id m))
        p-enum (get p-table (:service-category m))
        [month revenue-double] (:accounting-data m)
        revenue-long (long revenue-double)
        np-double (:product-net-price m)
        np-long (long np-double)]
    (when (nil? c-eid) (log/info :tax-id (:tax-id m) "has no mapping in c-table"))
    (when (nil? chan-eid) (log/info :debtor-tax-id (:debtor-tax-id m) "has no mapping in c-table"))
    (when (nil? p-enum) (log/info :service-category (:service-category m) "has no mapping in p-table"))
    (assoc {} :order/product-unique-id (:product-unique-id m)
           :order/io-writing-time t
           :order/customer c-eid
           :order/channel chan-eid
           :order/service-category-enum p-enum
           :order/terms-start-date (:terms-start-date m)
           :order/terms-end-date (:terms-end-date m)
           :order/product-net-price np-long
           :order/accounting-data {:accounting/month month :accounting/revenue revenue-long})))

(defn- revenue-merger
  " Test Case
    (apply merge-with revenue-merger [{:a 5 :b {:c 1}} {:a 6 :b {:c 2} } {:a 7 :b {:c 3}} {:a 8 :b {:c 4}}])
    => {:a 5, :b [{:c 1} {:c 2} {:c 3} {:c 4}]}
    (apply merge-with revenue-merger [{:a 5 :b {:c 1}}])
    => {:a 5, :b {:c 1}} "
  [left right]
  (if (map? right)
    (if (vector? left)
      (conj left right)
      (conj [] left right))
    left))

(defn- revenue-txes->order-tx
  "Merge the revenue into revenue-item-or-items.

   k -> \"product-unique-id\"
   v -> revenue-txes, a vector of {HashMap}, which each represents an item of revenue"
  [coll k v]
  (let [m (apply merge-with revenue-merger v)
        part-m (select-keys m order-property-fields)
        revenue-item-or-items  (:order/accounting-data m)
        revenue-items    (flatten (list revenue-item-or-items))
        order-tx-m       (assoc part-m :order/accounting-data revenue-items)]
    (conj coll order-tx-m)))
;; (get-orders-from-excel (d/db conn) "http://127.0.0.1:5001/" "raw.xlsx")
(defn- get-orders-from-excel
  "Read the excel file, retrieve the orders data,
   and then transform the data into db-transaction-form

  Implementation details:
  rest - remove the title row
  set  - remove duplicated rows"
  [db addr filename]
  (let [c-table (tax-id->c-eid db)
        p-table (service-category->enum db)
        sheet-data (get-raw-excel (str addr filename))
        title-table (first sheet-data)]
    (->> sheet-data
         rest
         (mapcat #(expand-order title-table %))
         (filter revenue-number?)
         (map #(order-m->revenue-tx c-table p-table %))
         (filter :order/service-category-enum)            ;; remove service-category "TV"
         (group-by :order/product-unique-id)
         (reduce-kv revenue-txes->order-tx [])
         set)))

(defn- order-eids [db]
  (d/q '[:find [?e ...] :where [?e :order/product-unique-id]] db))

(defn- eid->order [db eid]
  (d/pull db '[:order/product-unique-id] eid))

(defn- get-orders-from-db [db]
  (let [eids (order-eids db)
        tx-order (map #(eid->order db %) eids)]
    (set tx-order)))

;; [:fn/update-accounting-data [:order/product-unique-id 5722-1] [[2019-01 127379] [2019-02 115052]]]
(comment
  (d/q '[:find ?m ?r
         :in $ ?e
         :where [?e :order/accounting-data ?d]
         [?d :accounting/month ?m]
         [?d :accounting/revenue ?r]]
       (d/db conn) [:order/product-unique-id "5722-1"]))

(defn- accounting-data-not=?
  [db m]
  (let [u-i (:order/product-unique-id m)
        new-ad (set (map #(vector (:accounting/month %)
                                  (:accounting/revenue %)) (:order/accounting-data m)))
        old-ad (set (d/q '[:find ?m ?r
                           :in $ ?e
                           :where [?e :order/accounting-data ?d]
                           [?d :accounting/month ?m]
                           [?d :accounting/revenue ?r]]
                         db [:order/product-unique-id u-i]))]
    (not= new-ad old-ad)))

(defn- date-revenue-not=?
  [db m]
  (let [u-i (:order/product-unique-id m)
        key-tuple [:order/terms-start-date :order/terms-end-date :order/product-net-price]
        new-d-r (select-keys m key-tuple)
        old-d-r-tuple (d/q '[:find [?sd ?ed ?np]
                             :in $ ?o
                             :where [?o :order/terms-start-date ?sd]
                             [?o :order/terms-end-date ?ed]
                             [?o :order/product-net-price ?np]]
                           db [:order/product-unique-id u-i])
        old-d-r (zipmap key-tuple old-d-r-tuple)]
    (not= new-d-r old-d-r)))

(defn- retract-and-add [db m]
  (let [u-i (:order/product-unique-id m)
        eid [:order/product-unique-id u-i]
        ad-refs (d/q '[:find [?d ...]
                       :in $ ?e
                       :where [?e :order/accounting-data ?d]]
                     db eid)
        retracts (mapcat (fn [r] [[:db/retractEntity r]
                                  [:db/retract eid :order/accounting-data r]]) ad-refs)]
    (conj (vec retracts) m)))

(defn- ->update-tx [db txes]
  (mapcat #(retract-and-add db %) txes))

(defn sync-data
  "From Excel file, get the current order table.
   From DB, get the order inside DB.

   Treat create/update as different cases,
   Write into database"
  [url filename]
  (log/info "sync-data triggered!")
  (let [db (d/db conn)
        e-rel (get-orders-from-excel db url filename)
        d-rel (get-orders-from-db db)
        tx-group (group-by #(contains? d-rel (select-keys % [:order/product-unique-id])) e-rel)
        tx-create-data (get tx-group false)
        tx-to-sync-data (get tx-group true)
        tx-not=? (some-fn #(accounting-data-not=? db %) #(date-revenue-not=? db %))
        tx-update-data (->update-tx db (filter tx-not=? tx-to-sync-data))]
    (log/info "tx-create-data length:" (count tx-create-data))
    (log/info "tx-update-data length:" (count tx-update-data))
    (when (seq tx-create-data)
      (log/info "tx-create-data write into db, length: " (count tx-create-data))
      (log/info "first item of tx-create-data" (first tx-create-data))
      @(d/transact conn tx-create-data))
    (when (seq tx-update-data)
      (log/info "tx-update-data write into db, length: " (count tx-update-data))
      (log/info "first item of tx-update-data" (first tx-update-data))
      @(d/transact conn tx-update-data))
    (when (or (seq tx-create-data) (seq tx-update-data))
      :insert-data)))

(comment
  (def temp-datum #:order{:product-unique-id "5722-1"
                          :io-writing-time #inst "2018-08-20T10:46:00.000-00:00"
                          :customer 17592186046462
                          :channel  17592186046462
                          :service-category-enum :product.type/OA
                          :terms-start-date "2019-05-14"
                          :terms-end-date "2020-06-15"
                          :product-net-price 19500
                          :accounting-data (list {:accounting/month "2019-04" :accounting/revenue -3}
                                                 {:accounting/month "2019-05" :accounting/revenue -2})}))
