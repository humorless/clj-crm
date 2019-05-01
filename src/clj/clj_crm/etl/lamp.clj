(ns clj-crm.etl.lamp
  (:require [clj-http.client :as hc]
            [cheshire.core :as che]
            [clojure.set :as cs]
            [clojure.string :as s]
            [clojure.tools.logging :as log]
            [mount.core :refer [defstate]]
            [clj-crm.db.core :as dcore :refer [conn]]
            [datomic.api :as d]
            [clj-crm.config :refer [env]]
            [clojure.data.csv :as csv]
            [dk.ative.docjure.spreadsheet :as spreadsheet]
            [clojure.java.io :as io])
  (:import [java.io StringWriter]))

(defstate url
  :start (:lamp-url env)
  :stop "")

(defn- lamp-path [base]
  (str base "/nfi/CustomerInterfaceBO/getCustomerList?EMPLOYEE_NO=ANONYMOUS&DATA=[{compNm:LINETWLTD,sourceSystem:LAMP}]"))

(defn- get-lamp-data [addr]
  (:body (hc/get (lamp-path addr))))

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
  (if-let [result (get b-type-table (s/trim (:customer/business-type m)))]
    result
    (do
      (prn "ETL found unkown business type" (:customer/business-type m) (:customer/name m) (:customer/tax-id m))
      (log/error "ETL found unkown business type" (:customer/business-type m) (:customer/name m) (:customer/tax-id m))
      :customer.bus/unknown)))

(defn- get-customers-from-api [addr]
  (let [d (get-lamp-data addr)
        d-edn (che/parse-string d true)
        d-content-list (:DATA_OBJECT d-edn)
        ->customer (fn [m] {:customer/tax-id (:regNo m)
                            :customer/name (:custNm m)
                            :customer/name-en (:alterNm1 m)
                            :customer/id (:legacyNo m)
                            :customer/business-type (:businessTypes m)})]
    (->> (map ->customer d-content-list)
         (map #(assoc % :customer/business-type (->business-type %)))
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
  (let [eids (dcore/customer-eids db)
        query-result (map #(c-eid->customer db %) eids)]
    (set query-result)))

(defn- select-rows [m]
  (case (:registrationStatus m)
    "[C]LINE TAIWAN" true
    "[C,T]LINE TAIWAN" true
    "[C]LINE COMPANY(TH) / [C]LINE TAIWAN" true
    "[C]LINE CORP / [C]LINE TAIWAN" true
    false))

(defn- get-customers-from-excel
  "Read the excel file, and retrieve the customers data

  Implementation details:
  rest - remove the title row
  set  - remove duplicated rows"
  [addr filename]
  (with-open [stream (io/input-stream (str addr filename))]
    (->> (spreadsheet/load-workbook stream)
         (spreadsheet/select-sheet "Sheet0")
         (spreadsheet/select-columns {:A :customer/id
                                      :B :customer/name
                                      :C :customer/name-en
                                      :H :customer/tax-id
                                      :P :customer/business-type
                                      :F :registrationStatus})
         rest
         (filter select-rows)
         (map #(dissoc % :registrationStatus))
         (map #(assoc % :customer/business-type (->business-type %)))
         set)))
;;;;; public API ;;;;;

(defn sync-data
  "From LAMP system, get the current lamp customers.
   From DB, get the customers inside DB.
   Calculate the difference. Find out the new customers in LAMP but not in DB.
   Write into database"
  [filename]
  (log/info "sync-data triggered!")
  (let [l-customer-rel (get-customers-from-excel url filename)
        d-customer-rel (get-customers-from-db (d/db conn))
        tx-data (vec (cs/difference l-customer-rel d-customer-rel))]
    (do (log/info "tx-data write into db, length: " (count tx-data))
        (log/info "first item of tx-data" (first tx-data))
        (when (seq tx-data)
          @(d/transact conn tx-data)))))

(defn lamp-data-csv
  "Get data from LAMP, and change it to csv form"
  []
  (let [rels (get-customers-from-api url)
        data (map vals rels)
        string-writer (StringWriter.)]
    (csv/write-csv string-writer data)
    (str string-writer)))
