(ns clj-crm.db.revenue
  (:require [clj-crm.db.user :as duser]
            [datomic.api :as d]
            [clj-crm.db.core :refer [conn]]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clj-time.core :as time.core]
            [clj-time.format :as time.format]
            [clj-time.periodic :as time.periodic]))

;; Order matching functions
(def order-match-rules
  '[[(direct-allo-customer-order ?a ?o)
     [?a :allo/customer ?c]
     [?o :order/customer ?c]]
    [(direct-allo-product-order ?a ?o ?p-keyword)
     [?a :allo/product ?p-category]
     [?p :product/category ?p-category]
     [?p :product/type ?sc]
     [?o :order/service-category-enum ?sc]
     [?sc :db/ident ?p-keyword]]
    [(allo-time-order ?a ?o ?less)
     [?a :allo/time ?a-t]
     [?o :order/io-writing-time ?o-t]
     [(compare ?a-t ?o-t) ?less]]
    [(indirect-allo-customer-order ?a ?o)
     [?a :allo/customer ?c]
     [?o :order/channel ?c]]
    [(indirect-allo-product-order ?a ?o ?p-keyword)
     [?a :allo/product ?sc]
     [?o :order/service-category-enum ?sc]
     [?sc :db/ident ?p-keyword]]])

(defn- agency-u-eid->orders [db u-eid]
  (d/q '[:find ?o ?p-keyword ?pui
         :in $ % ?s ?less
         :where
         [?a :allo/sales ?s]
         (indirect-allo-customer-order ?a ?o)
         (indirect-allo-product-order ?a ?o ?p-keyword)
         (allo-time-order ?a ?o ?less)
         [?o :order/product-unique-id ?pui]
         (not-join [?o]
                   [?b :allo/sales ?s]
                   (direct-allo-customer-order ?b ?o)
                   (direct-allo-product-order ?b ?o _)
                   (allo-time-order ?b ?o ?less))]
       db order-match-rules u-eid -1))

(defn- reseller-u-eid->orders [db u-eid]
  (d/q '[:find ?o ?p-keyword ?pui
         :in $ % ?s ?less
         :where
         [?a :allo/sales ?s]
         (indirect-allo-customer-order ?a ?o)
         (indirect-allo-product-order ?a ?o ?p-keyword)
         (allo-time-order ?a ?o ?less)
         [?o :order/product-unique-id ?pui]]
       db order-match-rules u-eid -1))

(defn- direct-u-eid->orders [db u-eid]
  (d/q '[:find ?o ?p-keyword ?pui
         :in $ % ?s ?less
         :where
         [?a :allo/sales ?s]
         (direct-allo-customer-order ?a ?o)
         (direct-allo-product-order ?a ?o ?p-keyword)
         (allo-time-order ?a ?o ?less)
         [?o :order/product-unique-id ?pui]]
       db order-match-rules u-eid -1))

(defn- u-eid->orders [db u-eid]
  (let [chan (duser/u-eid->chan-type db u-eid)]
    (case chan
      :user.channel/direct (direct-u-eid->orders db u-eid)
      :user.channel/reseller (reseller-u-eid->orders db u-eid)
      :user.channel/agency (agency-u-eid->orders db u-eid)
      #{})))

(defn- o-eid->normal-revenues
  [db o-eid]
  (d/q '[:find ?pui ?m ?p-keyword ?r
         :in $ ?o
         :where
         [?o :order/product-unique-id ?pui]
         [?o :order/accounting-data ?ad]
         [?o :order/service-category-enum ?sc]
         [?sc :db/ident ?p-keyword]
         [?ad :accounting/month ?m]
         [?ad :accounting/revenue ?r]] db o-eid))

(defn- o-eid->delta-revenues
  "specify net-revenue at the starting month, just like a delta revenue"
  [db o-eid]
  (let [date->month (fn [s]
                      (string/join "-" (butlast (string/split s #"-"))))]
    (->>  (d/q '[:find ?pui ?sd ?p-keyword ?np
                 :in $ ?o
                 :where
                 [?o :order/product-unique-id ?pui]
                 [?o :order/service-category-enum ?sc]
                 [?sc :db/ident ?p-keyword]
                 [?o :order/product-net-price ?np]
                 [?o :order/terms-start-date ?sd]] db o-eid)
          (mapv #(update % 1 date->month)))))

(def td-fmt-date (time.format/formatter "yyyy-MM-dd"))

(def td-fmt-y-m (time.format/formatter "yyyy-MM"))

(defn- date-str->dt [date-str]
  (time.format/parse td-fmt-date date-str))

(defn- dt->y-m-str [dt]
  (time.format/unparse td-fmt-y-m dt))

(defn- days-of-closed-closed-dt
  [s-dt e-dt]
  (time.core/in-days (time.core/interval s-dt (time.core/plus e-dt (time.core/days 1)))))

(defn- revenue-per-day
  [total-revenue days]
  (/ total-revenue days))

(defn- frequencies-by [f coll]
  (let [gp (group-by f coll)]
    (zipmap (keys gp)  (map #(count (second %)) gp))))

(defn- month-dts->month-revenue-tuple
  [pui p-name r-p-d [y-m-str number-of-day]]
  (let [revenue (* r-p-d number-of-day)
        r (Math/round (double revenue))]
    [pui y-m-str p-name r]))

(defn- o-eid->day-revenues
  "specify net-revenue according to how many days per month has"
  [db o-eid]
  (let [[pui p-name np sd ed]  (d/q '[:find  [?pui ?p-keyword ?np ?sd ?ed]
                                      :in $ ?o
                                      :where
                                      [?o :order/product-unique-id ?pui]
                                      [?o :order/service-category-enum ?sc]
                                      [?sc :db/ident ?p-keyword]
                                      [?o :order/product-net-price ?np]
                                      [?o :order/terms-start-date ?sd]
                                      [?o :order/terms-end-date ?ed]] db o-eid)
        s-dt (date-str->dt sd) ;; dt stands for class #clj-time/date-time
        e-dt (date-str->dt ed)
        span-days-int (days-of-closed-closed-dt s-dt e-dt)
        r-p-d  (revenue-per-day np span-days-int)]
    (->> (time.periodic/periodic-seq s-dt (time.core/days 1))
         (take span-days-int)
         (frequencies-by #(dt->y-m-str %))
         (map #(month-dts->month-revenue-tuple pui p-name r-p-d %))
         (sort-by second))))

(defn- o-tuple->revenues
  "Example output: #{[17592186045536 \"2019-02\" :product.type/line_now 100]
                     [17592186045536 \"2019-03\" :product.type/more_tab 200] ...}"
  [db [o-eid p-type & more]]
  (case p-type
    :product.type/SS (o-eid->delta-revenues db o-eid)
    (o-eid->normal-revenues db o-eid)))

(def quarter-table
  {"01" :q1
   "02" :q1
   "03" :q1
   "04" :q2
   "05" :q2
   "06" :q2
   "07" :q3
   "08" :q3
   "09" :q3
   "10" :q4
   "11" :q4
   "12" :q4})

(defn- month-lookup [tuple]
  (let [date-str (second tuple)
        month-str (second (string/split date-str #"-"))]
    (keyword month-str)))

(defn- quarter-lookup [tuple]
  (let [date-str (second tuple)
        month-str (second (string/split date-str #"-"))]
    (get quarter-table month-str)))

(defn- year-lookup [tuple]
  (let [date-str (second tuple)
        year-str (first (string/split date-str #"-"))]
    (keyword year-str)))

(defn- update-map [m f]
  (reduce-kv (fn [m k v]
               (assoc m k (f v))) {} m))

(defn- sum-over-tuples [tuples]
  (walk/walk last #(apply + %) tuples))

(defn- group-by-year [coll]
  (group-by year-lookup coll))

(defn- group-by-quarter [coll]
  (group-by quarter-lookup coll))

(defn- group-by-month [coll]
  (group-by month-lookup coll))

(defn- sum-by-product-over-tuples [tuples]
  (->> tuples
       (group-by #(nth % 2))
       (map (fn [[k v]] (vector k (sum-over-tuples v))))
       (into {})))

(defn- statistics [tuples]
  {:sum (sum-over-tuples tuples)
   :sum-by-product (sum-by-product-over-tuples tuples)})

(defn- revenues->revenue-report
  [db revenues]
  (let [y-revenues   (group-by-year revenues)
        y-q-revenues (update-map y-revenues group-by-quarter)
        y-m-revenues (update-map y-revenues group-by-month)
        y-q-sum-revenues (update-map y-q-revenues
                                     #(update-map % statistics))
        y-m-sum-revenues (update-map y-m-revenues
                                     #(update-map % statistics))]
    {:quarterly y-q-sum-revenues
     :monthly   y-m-sum-revenues}))

;; Revenue stream matching functions
(def stream-match-rules
  '[[(etl-source ?ra ?o)
     [?ra :rev-allo/source ?s-etl]
     [?o  :rev-stream/source ?s-etl]]
    [(direct-allo-customer-stream ?a ?ra ?o)
     [?a :allo/customer ?c]
     [?ra :rev-allo/customer ?c]
     [?ra :rev-allo/customer-id ?ci]
     [?o :rev-stream/customer-id ?ci]]
    [(direct-allo-product-stream ?a ?o ?p-keyword)
     [?a :allo/product ?p-category]
     [?p :product/category ?p-category]
     [?p :product/type ?sc]
     [?o :rev-stream/service-category-enum ?sc]
     [?sc :db/ident ?p-keyword]]
    [(rev-allo-time-stream ?ra ?o ?less)
     [?ra :rev-allo/time ?a-t]
     [?o  :rev-stream/writing-time ?o-t]
     [(compare ?a-t ?o-t) ?less]]
    [(allo-time-stream ?a ?o ?less)
     [?a :allo/time ?a-t]
     [?o :rev-stream/writing-time ?o-t]
     [(compare ?a-t ?o-t) ?less]]
    [(indirect-allo-customer-stream ?a ?o)
     [?a :allo/customer ?c]
     [?o :rev-stream/channel ?c]]
    [(direct-allo-customer-stream-by-channel ?a ?o ?n1 ?n2)
     [?a :allo/customer ?c]
     [?o :rev-stream/channel ?c]]
    [(indirect-allo-product-stream ?a ?o ?p-keyword)
     [?a :allo/product ?sc]
     [?o :rev-stream/service-category-enum ?sc]
     [?sc :db/ident ?p-keyword]]])

(defn- inst->y-m-str
  "inst is of the type java.util.Date"
  [inst]
  (.format (java.text.SimpleDateFormat. "yyyy-MM") inst))

;; (direct-u-eid->revenues (d/db conn) [:user/email "userA1@example.com"]))
(defn- direct-u-eid->revenues [db u-eid]
  (->>  (d/q '[:find ?sui ?o-t ?p-keyword ?r
               :in $ % ?s ?less
               :where
               [?ra :rev-allo/sales ?s]
               [?a :allo/sales ?s]
               (or
                (direct-allo-customer-stream-by-channel ?a ?o ?ra ?less)
                (and (etl-source ?ra ?o)
                     (rev-allo-time-stream ?ra ?o ?less)
                     (direct-allo-customer-stream ?a ?ra ?o)))
               (direct-allo-product-stream ?a ?o ?p-keyword)
               (allo-time-stream ?a ?o ?less)
               [?o :rev-stream/stream-unique-id ?sui]
               [?o :rev-stream/writing-time ?o-t]
               [?o :rev-stream/revenue ?r]]
             db stream-match-rules u-eid -1)
        (mapv #(update % 1 inst->y-m-str))))

;; (agency-u-eid->revenues (d/db conn) [:user/email "userB2@example.com"])
(defn- agency-u-eid->revenues [db u-eid]
  (->> (d/q '[:find ?sui ?o-t ?p-keyword ?r
              :in $ % ?s ?less
              :where
              [?a :allo/sales ?s]
              (indirect-allo-customer-stream ?a ?o)
              (indirect-allo-product-stream ?a ?o ?p-keyword)
              (allo-time-stream ?a ?o ?less)
              [?o :rev-stream/stream-unique-id ?sui]
              [?o :rev-stream/writing-time ?o-t]
              [?o :rev-stream/revenue ?r]
              (not-join [?o]
                        [?rb :rev-allo/sales ?s]
                        [?b :allo/sales ?s]
                        (etl-source ?rb ?o)
                        (rev-allo-time-stream ?rb ?o ?less)
                        (direct-allo-customer-stream ?b ?rb ?o)
                        (direct-allo-product-stream ?b ?o _)
                        (allo-time-stream ?b ?o ?less))]
            db stream-match-rules u-eid -1)
       (mapv #(update % 1 inst->y-m-str))))

;; (reseller-u-eid->revenues (d/db conn) [:user/email "userB1@example.com"]))
(defn- reseller-u-eid->revenues [db u-eid]
  (->>
   (d/q '[:find ?sui ?o-t ?p-keyword ?r
          :in $ % ?s ?less
          :where
          [?a :allo/sales ?s]
          (indirect-allo-customer-stream ?a ?o)
          (indirect-allo-product-stream ?a ?o ?p-keyword)
          (allo-time-stream ?a ?o ?less)
          [?o :rev-stream/stream-unique-id ?sui]
          [?o :rev-stream/writing-time ?o-t]
          [?o :rev-stream/revenue ?r]]
        db stream-match-rules u-eid -1)
   (mapv #(update % 1 inst->y-m-str))))

(defn- u-eid->stream-revenues
  [db u-eid]
  (let [chan (duser/u-eid->chan-type db u-eid)]
    (case chan
      :user.channel/direct (direct-u-eid->revenues db u-eid)
      :user.channel/reseller (reseller-u-eid->revenues db u-eid)
      :user.channel/agency (agency-u-eid->revenues db u-eid)
      #{})))

;; Module API for revenue
(defn u-eid->revenue-report
  [db eid]
  (let [orders (u-eid->orders db eid)
        order-revenues (mapcat #(o-tuple->revenues db %) orders) ;; vector of revenue tuple
        stream-revenues (u-eid->stream-revenues db eid)
        revenue (revenues->revenue-report db (concat order-revenues stream-revenues))
        [u t] (duser/u-eid->userName-teamName-tuple db eid)]
    {:salesName u
     :teamName t
     :revenue revenue}))

(defn t-u-entry->revenue-report
  [db [teamName eids]]
  (let [orders (mapcat #(u-eid->orders db %) eids)
        order-revenues (mapcat #(o-tuple->revenues db %) orders) ;; vector of revenue tuple
        stream-revenues (mapcat #(u-eid->stream-revenues db %) eids)
        revenue (revenues->revenue-report db (concat order-revenues stream-revenues))]
    {:salesName "total"
     :teamName teamName
     :revenue revenue}))

;; Orders export API
(defn rev-stream-eids
  [db]
  (d/q '[:find [?e ...]
         :in $
         :where
         [?e :rev-stream/stream-unique-id]]
       db))

(defn order-eids
  [db]
  (d/q '[:find [?o-eid ...]
         :in $
         :where
         [?o-eid :order/product-unique-id]]
       db))

(defn s-eid->rev-stream
  [db eid]
  (d/pull db '[:rev-stream/stream-unique-id
               :rev-stream/campaign-name
               :rev-stream/customer-id
               {:rev-stream/channel [:customer/name :customer/id]}
               {:rev-stream/service-category-enum [:db/ident]}
               :rev-stream/writing-time
               :rev-stream/revenue
               :rev-stream/source] eid))

(defn o-eid->order
  "Transfrom order eid -> {HashMap with order fields}"
  [db eid]
  (d/pull db '[:order/product-unique-id
               {:order/customer [:customer/name :customer/id]}
               {:order/channel [:customer/name :customer/id]}
               {:order/service-category-enum [:db/ident]}
               :order/io-writing-time
               :order/accounting-data
               :order/product-net-price
               :order/terms-start-date
               :order/terms-end-date] eid))

(defn target-eids
  [db]
  (d/q '[:find [?e ...]
         :in $
         :where [?e :target/revenue]]
       db))

(defn target-eid->target
  [db eid]
  (d/pull db '[:target/year-quarterly
               {:target/user [:user/name :user/email]}
               :target/revenue] eid))

(defn- u-eid->target
  [db eid]
  (d/q '[:find ?q ?r
         :in $ ?u
         :where
         [?t :target/user ?u]
         [?t :target/year-quarterly ?q]
         [?t :target/revenue ?r]] db eid))
