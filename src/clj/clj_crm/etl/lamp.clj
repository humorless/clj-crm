(ns clj-crm.etl.lamp
  (:require [clj-http.client :as hc]
            [cheshire.core :as che]
            [clojure.set :as cs]
            [clojure.string :as s]
            [clojure.tools.logging :as log]
            [mount.core :refer [defstate]]
            [clj-crm.db.core :as dcore :refer [conn]]
            [datomic.api :as d]
            [clj-crm.config :refer [env]]))

(defn- lamp-path [base]
  (str base "/nfi/CustomerInterfaceBO/getCustomerList?EMPLOYEE_NO=ANONYMOUS&DATA=[{compNm:LINETWLTD,sourceSystem:LAMP}]"))

(defstate url
  :start (-> env :lamp-url lamp-path)
  :stop "")

(defn- get-lamp-data [addr]
  (:body (hc/get addr)))

(def b-type-table {"Arts / Movie / Entertainment" :customer.bus/arts
                   "Automotive / Transportation / Energy" :customer.bus/automotive
                   "Computers / Electronics" :customer.bus/computer
                   "Construction / Real Estate" :customer.bus/construction
                   "Cosmetics / Consumer Goods" :customer.bus/cosmetics
                   "Celebrity / Artist" :customer.bus/celebrity
                   "Education / Publication" :customer.bus/education
                   "Fashion" :customer.bus/fashion
                   "Finance / Insurance" :customer.bus/finance
                   "Food / Dining" :customer.bus/food
                   "Games / Internet" :customer.bus/games
                   "Government Services / Public Services" :customer.bus/government
                   "Health / Medicine" :customer.bus/health
                   "Marketing / Agency" :customer.bus/marketing
                   "Shopping / E-Commerce" :customer.bus/e-commerce
                   "Telecommunications / Broadcasting / Media" :customer.bus/media
                   "Travel / Leisure" :customer.bus/travel
                   "Wholesale / Distribution / Retail" :customer.bus/retail
                   "Miscellaneous" :customer.bus/miscellaneous})

(defn- ->business-type [m]
  (if-let [result (get b-type-table (s/trim (:businessTypes m)))]
    result
    (do
      (prn "ETL found unkown business type" (:businessTypes m) (:custNm m) (:regNo m))
      (log/error "ETL found unkown business type" (:businessTypes m) (:custNm m) (:regNo m))
      :customer.bus/unknown)))

(defn- get-customers-from-lamp [addr]
  (let [d (get-lamp-data addr)
        d-edn (che/parse-string d true)
        d-content-list (:DATA_OBJECT d-edn)
        ->customer (fn [m] {:customer/tax-id (:regNo m)
                            :customer/name (:custNm m)
                            :customer/name-en (:alterNm1 m)
                            :customer/id (:legacyNo m)
                            :customer/business-type (->business-type m)})]
    (->> (map ->customer d-content-list)
         set)))

(defn- c-eid->customer
  "Transfrom customer eid -> {HashMap with customer fields}"
  [db c-eid]
  (let [e (d/entity db c-eid)
        b-type (:customer/business-type e)
        customer-data (d/pull db '[:customer/tax-id :customer/name
                                   :customer/name-en :customer/id] c-eid)]
    (assoc customer-data :customer/business-type b-type)))

(defn- get-customers-from-db [db]
  (let [eids (dcore/get-customer-eids db)
        query-result (map #(c-eid->customer db %) eids)]
    (set query-result)))

(defn- get-new-customers
  "From LAMP system, get the current lamp customers.
   From DB, get the customers inside DB.
   Find out the new customers in LAMP but not in DB.

   Quick test:
   (get-new-customers url (d/db conn))"
  [addr db]
  (let [l-customer-rel (get-customers-from-lamp addr)
        d-customer-rel (get-customers-from-db db)]
    (cs/difference l-customer-rel d-customer-rel)))

(defn- rel->tx-customers [c-rel]
  (let [cust->two-products (fn [m]
                             [(assoc m :customer/inventory-type :customer.inv/account
                                     :customer/rp-id (str "a-" (:customer/id m)))
                              (assoc m :customer/inventory-type :customer.inv/display
                                     :customer/rp-id (str "d-" (:customer/id m)))])]
    (vec (mapcat cust->two-products c-rel))))

(defn sync-lamp-data
  "Get the LAMP data.
   Calculate the difference.
   Write into database"
  []
  (let [customer-rels (get-new-customers url (d/db conn))
        tx-data (rel->tx-customers customer-rels)]
    (do (prn "tx-data write into db, length: " (count tx-data))
        (prn "first item of tx-data" (first tx-data))
        (when (seq tx-data)
          @(d/transact conn tx-data)))))
