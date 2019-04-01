(ns clj-crm.etl.lamp
  (:require [clj-http.client :as hc]
            [cheshire.core :as che]
            [clojure.string :as s]
            [mount.core :refer [defstate]]
            [clj-crm.config :refer [env]]))

(defn- lamp-path [base]
  (str base "/nfi/CustomerInterfaceBO/getCustomerList?EMPLOYEE_NO=ANONYMOUS&DATA=[{compNm:LINETWLTD,sourceSystem:LAMP}]"))

(defstate url
  :start (-> env :lamp-url lamp-path)
  :stop "")

(defn- get-data-from-url [addr]
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

(defn src-data []
  (let [d (get-data-from-url url)
        d-edn (che/parse-string d true)
        d-content-list (:DATA_OBJECT d-edn)
        f (fn [m] {:customer/tax-id (:regNo m)
                   :customer/name (:custNm m)
                   :customer/name-en (:alterNm1 m)
                   :customer/id (:legacyNo m)
                   :customer/business-type (get b-type-table (s/trim (:businessTypes m)))})]
    (map f d-content-list)))

(def d (src-data))
(count (into #{} (map #(:customer/business-type %) d)))
