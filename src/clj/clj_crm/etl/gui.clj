(ns clj-crm.etl.gui
  (:require
   [clj-crm.parallel :refer [plet]]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [clj-crm.etl.utility :as utility]
   [clojure.spec.alpha :as spec]))

(spec/def ::month double?)
(spec/def ::service-category string?)
(spec/def ::gui-no string?)
(spec/def ::item-seq double?)
(spec/def ::net-amount double?)
(spec/def ::campaign-no double?)
(spec/def ::campaign-name string?)
(spec/def ::ad-item string?)
(spec/def ::debtor-tax-id string?)
(spec/def ::advertisor-tax-id string?)
(spec/def ::ad-unit string?)
(spec/def ::sign-status string?)
(spec/def ::sign-status-eio string?)

(spec/def ::order
  (spec/*
   (spec/keys :req-un
              [::month ::service-category ::gui-no
               ::item-seq ::net-amount
               ::campaign-no ::campaign-name ::ad-item
               ::debtor-tax-id ::advertisor-tax-id
               ::ad-unit ::sign-status ::sign-status-eio])))

(def ^:private columns-map
  {:A :month
   :H :service-category
   :B :gui-no
   :C :item-seq
   :F :debtor-tax-id
   :K :net-amount
   :Z :campaign-no
   :AB :ad-item
   :AD :campaign-name
   :AE :advertisor-tax-id
   :I :ad-unit
   :AN :sign-status
   :AO :sign-status-eio})

(defn- compact
  "remove the nil value key from a hashmap

  (compact {:a nil :b 1})
  ;; => {:b 1}"
  [m]
  (into {} (remove #(nil? (val %)) m)))

(defn- basic-mapping
  "`campaign-no`, `io-writing-time`, and `product-net-price` needs type transformation"
  [{y-m :month g-no :gui-no i-s :item-seq
    net-r :net-amount a-i :ad-item
    c-no :campaign-no c-name :campaign-name
    ad-unit :ad-unit s-status :sign-status s-status-eio :sign-status-eio}]
  (let [y-m-str (str (int y-m))
        i-s-str (str (int i-s))
        t-inst (utility/y-m->dt y-m-str)
        np-long (long net-r)
        c-no-long (long c-no)]
    {:order/product-unique-id (str g-no "_"  i-s-str)
     :order/io-writing-time t-inst
     :order/product-net-price np-long
     :order/product-name a-i
     :order/campaign-no  c-no-long
     :order/campaign-name c-name
     :order/campaign-status "Invoice Issued"
     :order/ad-unit ad-unit
     :order/sign-status s-status
     :order/sign-status-eio s-status-eio
     :order/source :etl.source/gui}))

(defn- chan-mapping
  [table {d-t-id :debtor-tax-id t-id-str :advertisor-tax-id}]
  (let [c-eid (get table t-id-str)
        trim-d-t-id (string/trim d-t-id)
        chan-eid (get table trim-d-t-id)]
    (when (nil? c-eid) (log/info :tax-id t-id-str "has no mapping in c-table"))
    (when (nil? chan-eid) (log/info :debtor-tax-id trim-d-t-id "has no mapping in c-table"))
    (compact {:order/customer c-eid
              :order/channel chan-eid})))

(defn- sc-mapping
  [table {sc :service-category}]
  (let [p-enum (get table sc)]
    (when (nil? p-enum) (log/info :service-category sc "has no mapping in p-table"))
    (compact {:order/service-category-enum p-enum})))

(defn- yearmonth->year-month
  "change `201905` to `2019-05`"
  [in-str]
  {:pre [(string? in-str)]}
  (let [front (apply str (take 4 in-str))
        back (apply str (take-last 2 in-str))]
    (str front "-" back)))

(defn- accounting-mapping
  [{ym-d :month r :net-amount}]
  (if (number? r)
    (let [y-m-str (utility/yearmonth->year-month (str (int ym-d)))
          ar {:accounting/month y-m-str
              :accounting/revenue (long r)}]
      {:order/accounting-data [ar]})
    {}))

(defn- valid-order?
  "This function distinguish the valid-order"
  [{d-t-i :debtor-tax-id}]
  (re-matches #"[0-9]+"  d-t-i))

(defn- data->data-txes
  [raw]
  (let [data (filter valid-order? raw)
        db (d/db conn)
        c-table (utility/tax-id->c-eid db)
        p-table (utility/service-category->enum db)]
    (plet [basic-v (map basic-mapping data)  ;; basic transform
           chan-v (map #(chan-mapping c-table %) data) ;; transform with lookup db
           sc-v (map #(sc-mapping p-table %) data) ;; transform with lookup db
           revenue-v (map #(accounting-mapping %) data)]
          (->> (map merge basic-v chan-v sc-v revenue-v)
               (filter :order/service-category-enum)
               (mapv #(vector :fn/upsert-order %))))))

(def ^{:private true
       :doc "Using -fn, this check-raw ignores invalid order"}
  check-raw
  (comp (utility/check-raw-fn ::order) #(filter valid-order? %)))

(def ^{:private true
       :doc "Using -fn, this get-raw-from-excel return the data part only, no title"}
  get-raw-from-excel
  (utility/get-raw-from-excel-fn columns-map))

(def sync-data
  (utility/sync-data-fn get-raw-from-excel check-raw data->data-txes))

(comment
  (def raw (get-raw-from-excel "http://10.20.30.40:5001/" "dev_gui.xlsx")))
