(ns clj-crm.etl.lamp
  (:require [clj-http.client :as hc]
            [cheshire.core :as che]
            [clojure.string :as s]
            [clojure.tools.logging :as log]
            [mount.core :refer [defstate]]
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

(defn get-customer-data [addr]
  (let [d (get-lamp-data addr)
        d-edn (che/parse-string d true)
        d-content-list (:DATA_OBJECT d-edn)
        ->customer (fn [m] {:customer/tax-id (:regNo m)
                            :customer/name (:custNm m)
                            :customer/name-en (:alterNm1 m)
                            :customer/id (:legacyNo m)
                            :customer/business-type (->business-type m)})]
    (->> (map ->customer d-content-list)
         (sort-by :customer/id))))
