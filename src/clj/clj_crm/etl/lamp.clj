(ns clj-crm.etl.lamp
  (:require
   [clojure.string :as string]
   [clojure.instant :as instant]
   [clojure.tools.logging :as log]
   [clj-crm.db.core :as dcore :refer [conn]]
   [datomic.api :as d]
   [clj-crm.etl.utility :as utility]
   [clojure.spec.alpha :as spec]))

(spec/def ::campaign-no double?)
(spec/def ::campaign-name string?)
(spec/def ::campaign-status string?)
(spec/def ::io-writing-time string?)
(spec/def ::tax-id string?)
(spec/def ::debtor-tax-id string?)
(spec/def ::product-unique-id string?)
(spec/def ::product-name string?)
(spec/def ::service-category string?)
(spec/def ::terms-start-date string?)
(spec/def ::terms-end-date string?)
(spec/def ::product-net-price double?)
(spec/def ::1 (spec/or :revenue double? :none string?))
(spec/def ::2 (spec/or :revenue double? :none string?))
(spec/def ::3 (spec/or :revenue double? :none string?))
(spec/def ::4 (spec/or :revenue double? :none string?))
(spec/def ::5 (spec/or :revenue double? :none string?))
(spec/def ::6 (spec/or :revenue double? :none string?))
(spec/def ::7 (spec/or :revenue double? :none string?))
(spec/def ::8 (spec/or :revenue double? :none string?))
(spec/def ::9 (spec/or :revenue double? :none string?))
(spec/def ::10 (spec/or :revenue double? :none string?))
(spec/def ::11 (spec/or :revenue double? :none string?))
(spec/def ::12 (spec/or :revenue double? :none string?))
(spec/def ::ad-unit string?)
(spec/def ::sign-status string?)
(spec/def ::sign-status-eio string?)

(spec/def ::order
  (spec/*
   (spec/keys :req-un
              [::campaign-no ::campaign-name ::campaign-status
               ::io-writing-time ::tax-id ::debtor-tax-id
               ::product-unique-id ::product-name ::service-category
               ::terms-start-date ::terms-end-date ::product-net-price
               ::1 ::2 ::3 ::4 ::5 ::6
               ::7 ::8 ::9 ::10 ::11 ::12
               ::ad-unit ::sign-status ::sign-status-eio])))

(def ^:private columns-map
  {:B :campaign-no
   :C :campaign-name
   :D :campaign-status
   :E :io-writing-time
   :J :tax-id
   :N :debtor-tax-id
   :O :product-unique-id
   :R :product-name
   :V :service-category
   :AC :terms-start-date
   :AD :terms-end-date
   :AF :product-net-price
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
   :AT :12
   :W  :ad-unit
   :AA :sign-status
   :AB :sign-status-eio})

(defn- compact
  "remove the nil value key from a hashmap

  (compact {:a nil :b 1})
  ;; => {:b 1}"
  [m]
  (into {} (remove #(nil? (val %)) m)))

(defn- entity-merger
  "Given a month str and revenue, return an entity when revenue is number"
  [k r]
  (when (number? r)
    {:accounting/month k
     :accounting/revenue (long r)}))

(def ^:private
  month-fields (mapv #(keyword (str %)) (range 1 13)))

(defn- process-accounting-revenue
  [title data]
  (let [title-entity (select-keys title month-fields)
        revenue-data (map #(select-keys % month-fields) data)]
    (->> revenue-data
         (map #(merge-with entity-merger title-entity %))
         (map compact)
         (map vals)
         (map #(hash-map :order/accounting-data (vec %))))))

(defn- t-fmt->inst
  "original format is `2019-04-01 10:39 ASIA/TAIPEI`"
  [io-w-t]
  (let [[date clock-time time-zone] (string/split io-w-t #" ")
        t-inst (instant/read-instant-date (str date "T" clock-time "+08:00"))]
    t-inst))

(defn- basic-mapping
  "`campaign-no`, `io-writing-time`, and `product-net-price` needs type transformation"
  [{c-no :campaign-no c-name :campaign-name c-status :campaign-status
    io-w-t :io-writing-time
    pui :product-unique-id p-name :product-name
    start :terms-start-date end :terms-end-date np :product-net-price
    ad-unit :ad-unit s-status :sign-status s-status-eio :sign-status-eio}]
  (let [t-inst (t-fmt->inst io-w-t)
        np-long (long np)
        c-no-long (long c-no)]
    {:order/product-unique-id pui
     :order/product-name p-name
     :order/io-writing-time t-inst
     :order/terms-start-date start
     :order/terms-end-date end
     :order/product-net-price np-long
     :order/campaign-no  c-no-long
     :order/campaign-name c-name
     :order/campaign-status c-status
     :order/ad-unit ad-unit
     :order/sign-status s-status
     :order/sign-status-eio s-status-eio
     :order/source :etl.source/lamp}))

(defn- chan-mapping
  [table {d-t-id :debtor-tax-id t-id :tax-id}]
  (let [c-eid (get table t-id)
        trim-d-t-id (string/trim d-t-id)
        chan-eid (get table trim-d-t-id)]
    (when (nil? c-eid) (log/info :tax-id t-id "has no mapping in c-table"))
    (when (nil? chan-eid) (log/info :debtor-tax-id trim-d-t-id "has no mapping in c-table"))
    (compact {:order/customer c-eid
              :order/channel chan-eid})))

(defn- sc-mapping
  [table {sc :service-category}]
  (let [p-enum (get table sc)]
    (when (nil? p-enum) (log/info :service-category sc "has no mapping in p-table"))
    (compact {:order/service-category-enum p-enum})))

(defn- data->data-txes
  [raw]
  (let [db (d/db conn)
        c-table (utility/tax-id->c-eid db)
        p-table (utility/service-category->enum db)
        title (first raw)
        data (rest raw)]
    (let [basic-v (map basic-mapping data)  ;; basic transform
          chan-v (map #(chan-mapping c-table %) data) ;; transform with lookup db
          sc-v (map #(sc-mapping p-table %) data) ;; transform with lookup db
          revenue-v (process-accounting-revenue title data)] ;; transform with multiplex with title
      (->> (map merge basic-v chan-v sc-v revenue-v)
           (filter :order/service-category-enum)
           (mapv #(vector :fn/upsert-order-lamp %))))))

(def ^{:private true
       :doc "this check-raw ignores title"}
  check-raw
  (comp (utility/check-raw-fn ::order) rest))

(def ^{:private true
       :doc "Using -fn*, this get-raw-from-excel return the title+data"}
  get-raw-from-excel
  (utility/get-raw-from-excel-fn* columns-map))

(def sync-data
  (utility/sync-data-fn get-raw-from-excel check-raw data->data-txes))

(comment
  (def raw (get-raw-from-excel "http://10.20.30.40:5001/" "raw.xlsx")))

(comment
  (def test-txes-1
    [[:fn/upsert-order-lamp
      #:order{:io-writing-time #inst "2019-04-01T02:39:00.000-00:00",
              :service-category-enum :product.type/today,
              :accounting-data
              [#:accounting{:month "2019-04", :revenue 99995}
               #:accounting{:month "2019-05", :revenue 200005}],
              :terms-start-date "2019-04-24",
              :terms-end-date "2019-05-14",
              :product-net-price 300000,
              :product-unique-id "10006-1"}]]))

(comment
  (d/q '[:find ?m ?r
         :in $ ?e
         :where
         [?e :order/accounting-data ?a]
         [?e :order/product-net-price ?p]
         [?a :accounting/month ?m]
         [?a :accounting/revenue ?r]]
       (d/db conn) [:order/product-unique-id "10006-1"]))

(comment
  (d/q '[:find ?p .
         :in $ ?e
         :where
         [?e :order/product-net-price ?p]]
       (d/db conn) [:order/product-unique-id "10006-1"]))

(comment
  (def test-txes-2
    [[:fn/upsert-order-lamp
      #:order{:io-writing-time #inst "2019-04-01T02:39:00.000-00:00",
              :service-category-enum :product.type/today,
              :accounting-data
              [#:accounting{:month "2019-04", :revenue -2}
               #:accounting{:month "2019-05", :revenue -3}
               #:accounting{:month "2019-02", :revenue 4}
               #:accounting{:month "2019-01", :revenue 5}],
              :terms-start-date "2019-04-24",
              :terms-end-date "2019-05-14",
              :product-net-price 400,
              :product-unique-id "10006-1"}]]))
