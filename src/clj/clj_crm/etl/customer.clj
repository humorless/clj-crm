(ns clj-crm.etl.customer
  (:require
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [clojure.string :as string]
   [clj-crm.etl.utility :as utility]
   [clojure.spec.alpha :as spec]))

(spec/def ::customer-id string?)
(spec/def ::customer-name string?)
(spec/def ::customer-name-en string?)
(spec/def ::neon-customer-code (spec/nilable string?))
(spec/def ::registration-status string?)
(spec/def ::tax-id string?)
(spec/def ::business-type string?)

(spec/def ::customer
  (spec/*
   (spec/keys :reg-un
              [::customer-id ::customer-name ::customer-name-en
               ::neon-customer-code ::registration-status
               ::tax-id ::business-type])))

(def ^:private columns-map
  {:A :customer-id
   :B :customer-name
   :C :customer-name-en
   :E :neon-customer-code
   :F :registration-status
   :H :tax-id
   :P :business-type})

(def ^:private  b-type-table {"Arts / Movie / Entertainment" :customer.bus/arts
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

(defn- ->business-type [b-type-str]
  (if-let [result (get b-type-table b-type-str)]
    result
    (do
      (log/error "ETL found unkown business type string" b-type-str)
      :customer.bus/unknown)))

(defn- select-rows [m]
  (case (:registration-status m)
    "[C]LINE TAIWAN" true
    "[C,T]LINE TAIWAN" true
    "[C,R]LINE TAIWAN" true
    "[C]LINE COMPANY(TH) / [C]LINE TAIWAN" true
    "[C]LINE CORP / [C]LINE TAIWAN" true
    false))

(defn- basic-mapping
  "handle the mapping that does not need to lookup any tables in DB"
  [{c-id :customer-id c-name :customer-name c-name-en :customer-name-en
    neon :neon-customer-code tax-id :tax-id b-type-str :business-type}]
  (let [b-type (->business-type (string/trim b-type-str))]
    {:customer/id c-id
     :customer/name c-name
     :customer/name-en c-name-en
     :customer/neon-code (if (some? neon) (string/trim neon) nil)
     :customer/tax-id tax-id
     :customer/business-type b-type}))

(def ^:private check-raw
  (utility/check-raw-fn ::customer))

(def ^:private get-raw-from-excel
  (utility/get-raw-from-excel-fn columns-map))

(defn- data->data-txes
  [data]
  (->> data
       (filter select-rows)
       (map basic-mapping)
       (map utility/compact))) ;; remove the nil-valued :customer/neon-code
;;;;; public API ;;;;;


(def sync-data
  (utility/sync-data-fn get-raw-from-excel check-raw data->data-txes))

(comment
  (def raw (get-raw-from-excel "http://10.20.30.40:5001/" "customer.xlsx")))
