(ns clj-crm.db.revenue
  (:require [clj-crm.db.user :as duser]
            [datomic.api :as d]
            [clj-crm.db.core :refer [conn]]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clojure.set :as cset]
            [clj-time.core :as time.core]
            [clj-time.format :as time.format]
            [clj-time.periodic :as time.periodic]))

;; Order matching functions
(def order-match-rules
  '[[(direct-allo-customer-order ?a ?o ?c)
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
    [(indirect-allo-customer-order ?a ?o ?c)
     [?a :allo/customer ?c]
     [?o :order/channel ?c]]
    [(indirect-allo-product-order ?a ?o ?p-keyword)
     [?a :allo/product ?sc]
     [?o :order/service-category-enum ?sc]
     [?sc :db/ident ?p-keyword]]])

(defn- agency-u-eid->orders [db u-eid]
  (d/q '[:find ?o ?p-keyword ?c ?pui
         :in $ % ?s ?less
         :where
         [?a :allo/sales ?s]
         (indirect-allo-customer-order ?a ?o ?c)
         (indirect-allo-product-order ?a ?o ?p-keyword)
         (allo-time-order ?a ?o ?less)
         [?o :order/product-unique-id ?pui]
         (not-join [?o ?less]
                   [?b :allo/sales ?_b-s]
                   [?_b-s :user/channel :user.channel/direct]
                   (direct-allo-customer-order ?b ?o ?_b-c)
                   (direct-allo-product-order ?b ?o ?_b-p)
                   (allo-time-order ?b ?o ?less))]
       db order-match-rules u-eid -1))

(defn- reseller-u-eid->orders [db u-eid]
  (d/q '[:find ?o ?p-keyword ?c ?pui
         :in $ % ?s ?less
         :where
         [?a :allo/sales ?s]
         (indirect-allo-customer-order ?a ?o ?c)
         (indirect-allo-product-order ?a ?o ?p-keyword)
         (allo-time-order ?a ?o ?less)
         [?o :order/product-unique-id ?pui]]
       db order-match-rules u-eid -1))

(defn- direct-u-eid->orders [db u-eid]
  (d/q '[:find ?o ?p-keyword ?c ?pui
         :in $ % ?s ?less
         :where
         [?a :allo/sales ?s]
         (direct-allo-customer-order ?a ?o ?c)
         (direct-allo-product-order ?a ?o ?p-keyword)
         (allo-time-order ?a ?o ?less)
         (not-join [?o ?less]
                   [?b :allo/sales ?_b-s]
                   [?_b-s :user/channel :user.channel/reseller]
                   (indirect-allo-customer-order ?b ?o ?_b-c)
                   (indirect-allo-product-order ?b ?o ?_b-p)
                   (allo-time-order ?b ?o ?less))
         [?o :order/product-unique-id ?pui]]
       db order-match-rules u-eid -1))

(defn- all-orders-but [db1 db2]
  (d/q '[:find ?o ?p-keyword ?c ?pui
         :in $a $b ?c
         :where
         [$a ?o :order/service-category-enum ?sc]
         [$a ?sc :db/ident ?p-keyword]
         [$a ?o :order/product-unique-id ?pui]
         ($b not [?o])]
       db1 db2 -1))

(defn- u-eid->orders [db u-eid]
  (let [chan (duser/u-eid->chan-type db u-eid)]
    (case chan
      :user.channel/direct (direct-u-eid->orders db u-eid)
      :user.channel/reseller (reseller-u-eid->orders db u-eid)
      :user.channel/agency (agency-u-eid->orders db u-eid)
      #{})))

(defn- o-eid->normal-revenues
  [db o-eid c-eid]
  (d/q '[:find ?pui ?m ?c ?r
         :in $ ?o ?c
         :where
         [?o :order/product-unique-id ?pui]
         [?o :order/accounting-data ?ad]
         [?ad :accounting/month ?m]
         [?ad :accounting/revenue ?r]] db o-eid c-eid))

(defn- o-eid->delta-revenues
  "specify net-revenue at the starting month, just like a delta revenue"
  [db o-eid c-eid]
  (let [date->month (fn [s]
                      (string/join "-" (butlast (string/split s #"-"))))]
    (->>  (d/q '[:find ?pui ?sd ?c ?np
                 :in $ ?o ?c
                 :where
                 [?o :order/product-unique-id ?pui]
                 [?o :order/product-net-price ?np]
                 [?o :order/terms-start-date ?sd]] db o-eid c-eid)
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
  [pui c-name r-p-d [y-m-str number-of-day]]
  (let [revenue (* r-p-d number-of-day)
        r (Math/round (double revenue))]
    [pui y-m-str c-name r]))

(defn- o-eid->day-revenues
  "specify net-revenue according to how many days per month has"
  [db o-eid c-eid]
  (let [[pui c-name np sd ed]  (d/q '[:find [?pui ?c ?np ?sd ?ed]
                                      :in $ ?o ?c
                                      :where
                                      [?o :order/product-unique-id ?pui]
                                      [?o :order/product-net-price ?np]
                                      [?o :order/terms-start-date ?sd]
                                      [?o :order/terms-end-date ?ed]] db o-eid c-eid)
        s-dt (date-str->dt sd) ;; dt stands for class #clj-time/date-time
        e-dt (date-str->dt ed)
        span-days-int (days-of-closed-closed-dt s-dt e-dt)
        r-p-d  (revenue-per-day np span-days-int)]
    (->> (time.periodic/periodic-seq s-dt (time.core/days 1))
         (take span-days-int)
         (frequencies-by #(dt->y-m-str %))
         (map #(month-dts->month-revenue-tuple pui c-name r-p-d %))
         (sort-by second))))

(defn- o-eid-src-match?
  [db o-eid src]
  (d/q '[:find ?o .
         :in $ ?o ?src
         :where [?o :order/source ?src]]
       db o-eid src))

(defn- o-tuple->revenues
  "Example output: #{[pui-a \"2019-02\" c-eid-a 100]
                     [pui-b \"2019-03\" c-eid-b 200] ...}"
  [db [o-eid p-type c-eid & more]]
  (case p-type
    :product.type/SS (if (o-eid-src-match? db o-eid :etl.source/lamp)
                       (o-eid->delta-revenues db o-eid c-eid)
                       (o-eid->normal-revenues db o-eid c-eid))
    (o-eid->normal-revenues db o-eid c-eid)))

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
   :ori tuples})

(defn- report-by-quarterly-monthly
  [revenues]
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
    [(direct-allo-customer-stream ?a ?ra ?o ?c)
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
    [(indirect-allo-customer-stream ?a ?o ?c)
     [?a :allo/customer ?c]
     [?o :rev-stream/channel ?c]]
    [(direct-allo-customer-stream-by-channel ?a ?o ?c ?_1 ?_2)
     [?a :allo/customer ?c]
     [?o :rev-stream/channel ?c]]
    [(direct-allo-customer-stream-by-customer-id ?a ?o ?c ?s ?less)
     [?ra :rev-allo/sales ?s]
     (etl-source ?ra ?o)
     (rev-allo-time-stream ?ra ?o ?less)
     (direct-allo-customer-stream ?a ?ra ?o ?c)]
    [(indirect-allo-product-stream ?a ?o ?p-keyword)
     [?a :allo/product ?sc]
     [?o :rev-stream/service-category-enum ?sc]
     [?sc :db/ident ?p-keyword]]])

(defn- inst->y-m-str
  "inst is of the type java.util.Date"
  [inst]
  (.format (java.text.SimpleDateFormat. "yyyy-MM") inst))

(defn- all-stream-revenues [db]
  (d/q '[:find ?sui ?a-t ?c ?r
         :in $ ?c
         :where
         [?o :rev-stream/stream-unique-id ?sui]
         [?o :rev-stream/accounting-time ?a-t]
         [?o :rev-stream/revenue ?r]]
       db -1))

(defn- all-stream-revenues-but [db1 db2]
  (let [s-db (all-stream-revenues db1)]
    (d/q '[:find ?sui ?m ?c ?r
           :in $a $b
           :where
           [$a ?sui ?m ?c ?r]
           ($b not [?sui ?m])]
         s-db db2)))

;; (direct-u-eid->revenues (d/db conn) [:user/email "userA1@example.com"]))
(defn- direct-u-eid->revenues [db u-eid]
  (d/q '[:find ?sui ?a-t ?c ?r
         :in $ % ?s ?less
         :where
         [?a :allo/sales ?s]
         (or
          (direct-allo-customer-stream-by-channel ?a ?o ?c ?s ?less)
          (direct-allo-customer-stream-by-customer-id ?a ?o ?c ?s ?less))
         (direct-allo-product-stream ?a ?o ?p-keyword)
         (allo-time-stream ?a ?o ?less)
         [?o :rev-stream/stream-unique-id ?sui]
         [?o :rev-stream/accounting-time ?a-t]
         [?o :rev-stream/revenue ?r]]
       db stream-match-rules u-eid -1))

;; (agency-u-eid->revenues (d/db conn) [:user/email "userB2@example.com"])

(defn- staging-direct-u-eid->revenue-eids
  [db u-eid]
  (into #{}
        (d/q '[:find ?o
               :in $ % ?s ?less
               :where
               [?b :allo/sales ?s]
               (allo-time-stream ?b ?o ?less)
               (direct-allo-customer-stream-by-customer-id ?b ?o ?_c ?s ?less)
               (direct-allo-product-stream ?b ?o ?_p)]
             db stream-match-rules u-eid -1)))

(defn- staging-direct->revenue-eids*
  "return the revenue eid collections that will be negated by agency-u-eid->revenues"
  [db t]
  (let [direct-u-eids (duser/direct-sales-eids db)
        r-eids-set (map #(staging-direct-u-eid->revenue-eids db %) direct-u-eids)]
    (apply cset/union r-eids-set)))

(def ^:private staging-direct->revenue-eids (memoize staging-direct->revenue-eids*))

(defn- staging-agency-u-eid->revenue-rels
  "return the maximal possible relation set of agency revenue"
  [db u-eid]
  (d/q '[:find ?o ?c
         :in $ % ?s ?less
         :where
         [?a :allo/sales ?s]
         (indirect-allo-customer-stream ?a ?o ?c)
         (indirect-allo-product-stream ?a ?o ?p-keyword)
         (allo-time-stream ?a ?o ?less)]
       db stream-match-rules u-eid -1))

(defn- agency-u-eid->revenues [db u-eid]
  (let [not-join-eids (staging-direct->revenue-eids db (d/basis-t db))
        possible-rels (staging-agency-u-eid->revenue-rels db u-eid)]
    (d/q '[:find ?sui ?a-t ?c ?r
           :in $A $B $D
           :where
           [$A ?o ?c]
           ($B not [?o])
           [$D ?o :rev-stream/stream-unique-id ?sui]
           [$D ?o :rev-stream/accounting-time ?a-t]
           [$D ?o :rev-stream/revenue ?r]]
         possible-rels not-join-eids db)))

(defn- agency-u-eid->revenues* [db u-eid]
  (d/q '[:find ?sui ?a-t ?c ?r
         :in $ % ?s ?less
         :where
         [?a :allo/sales ?s]
         (indirect-allo-customer-stream ?a ?o ?c)
         (indirect-allo-product-stream ?a ?o ?p-keyword)
         (allo-time-stream ?a ?o ?less)
         [?o :rev-stream/stream-unique-id ?sui]
         [?o :rev-stream/accounting-time ?a-t]
         [?o :rev-stream/revenue ?r]
         (not-join [?o ?less]
                   [?b :allo/sales ?_b-s]
                   [?_b-s :user/channel :user.channel/direct]
                   (direct-allo-customer-stream-by-customer-id ?b ?o ?_b-c ?_b-s ?less)
                   (direct-allo-product-stream ?b ?o ?_b-p)
                   (allo-time-stream ?b ?o ?less))]
       db stream-match-rules u-eid -1))

;; (reseller-u-eid->revenues (d/db conn) [:user/email "userB1@example.com"]))


(defn- reseller-u-eid->revenues [db u-eid]
  (d/q '[:find ?sui ?a-t ?c ?r
         :in $ % ?s ?less
         :where
         [?a :allo/sales ?s]
         (indirect-allo-customer-stream ?a ?o ?c)
         (indirect-allo-product-stream ?a ?o ?p-keyword)
         (allo-time-stream ?a ?o ?less)
         [?o :rev-stream/stream-unique-id ?sui]
         [?o :rev-stream/accounting-time ?a-t]
         [?o :rev-stream/revenue ?r]]
       db stream-match-rules u-eid -1))

(defn- u-eid->stream-revenues
  [db u-eid]
  (let [chan (duser/u-eid->chan-type db u-eid)]
    (case chan
      :user.channel/direct (direct-u-eid->revenues db u-eid)
      :user.channel/reseller (reseller-u-eid->revenues db u-eid)
      :user.channel/agency (agency-u-eid->revenues db u-eid)
      #{})))

(defn- u-eid->total-revenues
  [db eid]
  (let [orders (u-eid->orders db eid)
        order-revenues (mapcat #(o-tuple->revenues db %) orders) ;; vector of revenue tuple
        stream-revenues (u-eid->stream-revenues db eid)]
    (concat order-revenues stream-revenues)))

(defn- c-eid->customerName
  [db]
  (->> (d/q '[:find ?c ?n
              :where
              [?c :customer/name ?n]] db)
       (into {})))

(defn- ->tucr-entity
  "create team, user, customer, revenue entity"
  [table t u [c-eid revenues]]
  (let [c-name (get table c-eid)]
    {:teamName t
     :salesName u
     :customerName c-name
     :revenue (report-by-quarterly-monthly revenues)}))

(defn- u-eids->other-order-revenues
  [db eids]
  (let [order-db (mapcat #(u-eid->orders db %) eids)
        other-orders (all-orders-but db order-db)]
    (mapcat #(o-tuple->revenues db %) other-orders)))

(defn- u-eids->other-stream-revenues
  [db eids]
  (let [stream-revenue-db (mapcat #(u-eid->stream-revenues db %) eids)]
    (all-stream-revenues-but db stream-revenue-db)))

(defn- all-order-revenues
  [db]
  (let [orders (all-orders-but db [])]
    (mapcat #(o-tuple->revenues db %) orders)))

(def place-holder (apply str (repeat 50 "z")))
(def place-holder-other (str (apply str (repeat 49 "z")) "a"))

(defn- ->order-ru-tuple
  "The output is: `[o-eid year-month-string u-eid revenue]`"
  [u-eid [pui y-m c r]]
  (let [o [:order/product-unique-id pui]]
    [o y-m u-eid r]))

(defn- ->stream-ru-tuple
  "The output is: `[o-eid year-month-string u-eid c-eid revenue]`"
  [db u-eid [sui y-m c r]]
  (let [o (d/q '[:find ?o .
                 :in $ ?sui ?y-m
                 :where
                 [?o :rev-stream/stream-unique-id ?sui]
                 [?o :rev-stream/accounting-time ?y-m]]
               db sui y-m)]
    [o y-m u-eid c r]))

;; Module API for revenue
(defn u-eid->stream-ru-tuples
  [db u-eid]
  (->> (u-eid->stream-revenues db u-eid)
       (map #(->stream-ru-tuple db u-eid %))))

(defn u-eid->order-ru-tuples
  [db u-eid]
  (->> (u-eid->orders db u-eid)
       (mapcat #(o-tuple->revenues db %))
       (map #(->order-ru-tuple u-eid %))))

(defn u-eids->other-ru-tuples
  [db eids]
  (let [order-revenues (u-eids->other-order-revenues db eids)
        stream-revenues (u-eids->other-stream-revenues db eids)
        order-ru-tuples  (map #(->order-ru-tuple nil %) order-revenues)
        stream-ru-tuples  (map #(->stream-ru-tuple db nil %) stream-revenues)]
    {:order order-ru-tuples
     :stream stream-ru-tuples}))

(defn place-holder->total
  [ent]
  (let [{t :teamName s :salesName c :customerName} ent
        t-s-c-total {:teamName "total" :salesName "total" :customerName "total"}
        ot-s-c-total {:teamName "other" :salesName "total" :customerName "total"}
        s-c-total   {:salesName "total" :customerName "total"}
        c-total     {:customerName "total"}]
    (cond
      (and (= t place-holder) (= s place-holder) (= c place-holder)) (merge ent t-s-c-total)
      (and (= t place-holder-other) (= s place-holder) (= c place-holder)) (merge ent ot-s-c-total)
      (and (= s place-holder) (= c place-holder)) (merge ent s-c-total)
      (= c place-holder) (merge ent c-total)
      :else ent)))

(defn total-revenue-report
  [db]
  (let [order-revenues (all-order-revenues db)
        stream-revenues (all-stream-revenues db)
        total-revenues (concat order-revenues stream-revenues)]
    {:teamName place-holder
     :salesName place-holder
     :customerName place-holder
     :revenue (report-by-quarterly-monthly total-revenues)}))

(defn u-eids->other-revenue-report
  [db eids]
  (let [order-revenues (u-eids->other-order-revenues db eids)
        stream-revenues (u-eids->other-stream-revenues db eids)
        total-revenues (concat order-revenues stream-revenues)]
    {:teamName  place-holder-other
     :salesName place-holder
     :customerName place-holder
     :revenue (report-by-quarterly-monthly total-revenues)}))

(defn u-eid->customer-revenue-report-v
  [db eid]
  (let [total-revenues (u-eid->total-revenues db eid)
        c-eid-revenues-m (group-by #(nth % 2) total-revenues)
        [u t] (duser/u-eid->userName-teamName-tuple db eid)]
    (let [table (c-eid->customerName db)
          data-v (mapv #(->tucr-entity table t u %) c-eid-revenues-m)]
      data-v)))

(defn u-eid->revenue-report
  [db eid]
  (let [total-revenues (u-eid->total-revenues db eid)
        [u t] (duser/u-eid->userName-teamName-tuple db eid)]
    {:teamName t
     :salesName u
     :customerName place-holder
     :revenue (report-by-quarterly-monthly total-revenues)}))

(defn t-u-entry->revenue-report
  [db [teamName eids]]
  (let [total-team-revenues (mapcat #(u-eid->total-revenues db %) eids)]
    {:teamName teamName
     :salesName place-holder
     :customerName place-holder
     :revenue (report-by-quarterly-monthly total-team-revenues)}))

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
               :rev-stream/accounting-time
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
