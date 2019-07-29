(ns clj-crm.fjr.core
  (:require [clj-crm.db.user :as duser]
            [datomic.api :as d]
            [clj-crm.db.core :refer [conn]]
            [clojure.string :as string]))

;; fjr stands for full join report
(defn- customer-view
  [db fjr]
  (let [{c :c-eid d :d-eid} fjr
        c-entity (d/pull db '[:customer/id
                              :customer/name
                              :customer/name-en
                              :customer/tax-id
                              {:customer/business-type [:db/ident]}] c)
        d-entity (d/pull db  '[[:customer/id      :as :channel/id]
                               [:customer/name    :as :channel/name]
                               [:customer/name-en :as :channel/name-en]
                               [:customer/tax-id  :as :channel/tax-id]] d)]
    (merge c-entity d-entity)))

(defn- user-view
  [db fjr]
  (let [{u :u-eid} fjr
        u-graph (d/pull db '[:user/name
                             {:user/team [:db/ident]}] u)
        t-n (get-in u-graph [:user/team :db/ident])
        teamName (if (some? t-n) (name t-n) nil)]
    {:user/name (:user/name u-graph)
     :team/name teamName}))

(defn- revenue-view
  [db fjr]
  (let [{r :r t :y-m} fjr]
    {:revenue/value r
     :revenue/time  t}))

(defn- stream-rebate-view
  [db r-table fjr]
  (let [{d :d-eid o :o-eid r :r} fjr
        p-graph (d/pull db '[:rev-stream/service-category-enum] o)
        sc (get-in p-graph [:rev-stream/service-category-enum :db/id])
        rebate (get r-table [d sc] 0)]
    {:revenue/net-value (* r (- 1 rebate))}))

(defn- order-rebate-view
  [db r-table fjr]
  (let [{d :d-eid o :o-eid r :r} fjr
        p-graph (d/pull db '[:order/service-category-enum] o)
        sc (get-in p-graph [:order/service-category-enum :db/id])
        rebate (get r-table [d sc] 0)]
    {:revenue/net-value (* r (- 1 rebate))}))

(defn- rebate-tidy
  [rbs]
  (let [c-s (map first rbs)
        p-s (map second rbs)
        r-s (map last rbs)]
    (map #(vector (vector %1 %2) %3)  c-s p-s r-s)))

(defn- c-p->rebate
  [db]
  (->> (d/q '[:find ?c ?p ?rb
              :in $
              :where
              [?e :allo/customer ?c]
              [?e :allo/product ?p]
              [?e :allo/rebate ?rb]]
            db)
       (rebate-tidy)
       (into {})))

(defn- sc-enum->p-id
  [db]
  (->> (d/q '[:find ?t ?p
              :in $
              :where
              [?e :product/type ?t]
              [?e :product/type-id ?p]]
            db)
       (into {})))

(defn- sc-entity->product-id
  [table {sc :db/id}]
  (get table sc))

(defn- rev-stream-view
  [db p-table fjr]
  (let [{o :o-eid} fjr]
    (-> (d/pull db '[:rev-stream/stream-unique-id
                     :rev-stream/writing-time
                     :rev-stream/accounting-time
                     :rev-stream/source
                     :rev-stream/campaign-name
                     :rev-stream/customer-id
                     :rev-stream/service-category-enum] o)
        (update :rev-stream/source name)
        (update :rev-stream/service-category-enum #(sc-entity->product-id p-table %)))))

(defn- order-view
  [db p-table fjr]
  (let [{o :o-eid} fjr]
    (-> (d/pull db '[:order/product-unique-id
                     :order/product-name
                     :order/io-writing-time
                     :order/source
                     :order/campaign-no
                     :order/campaign-status
                     :order/campaign-name
                     :order/product-net-price
                     :order/terms-start-date
                     :order/terms-end-date
                     :order/service-category-enum] o)
        (update :order/source name)
        (update :order/service-category-enum #(sc-entity->product-id p-table %)))))

(defn- ru->d-entity
  [db ru]
  (let [d (d/q '[:find ?d .
                 :in $ ?o
                 :where
                 [?o :rev-stream/channel ?d]]
               db (first ru))]
    {:d-eid d}))

(defn- ru->c-entity
  "The output may be {:c-eid nil}"
  [db ru]
  (let [[o y-m u c r] ru
        chan-type (if (nil? u)
                    nil
                    (d/q '[:find ?ti .
                           :in $ ?u
                           :where
                           [?u :user/channel ?t]
                           [?t :db/ident ?ti]]
                         db u))]
    (if (not= chan-type :user.channel/direct)
      {:c-eid nil}
      {:c-eid c})))

(defn- ru->otur-entity
  [ru]
  (let [[o y-m u c r] ru]
    {:o-eid o
     :y-m y-m
     :u-eid u
     :r r}))

(defn- stream-ru-tuples->fjr-entity-xs
  "fjr stands for full join revenue"
  [db ru-ts]
  (let [otur-xs (map ru->otur-entity ru-ts)
        d-xs (map #(ru->d-entity db %) ru-ts)
        c-xs (map #(ru->c-entity db %) ru-ts)]
    (map merge otur-xs d-xs c-xs)))

(defn- order-ru-tuples->fjr-entity-xs
  "fjr stands for full join revenue"
  [db ru-ts]
  (->>
   (d/q '[:find ?o ?y-m ?c ?d ?u ?r
          :in $ [[?o ?y-m ?u ?r]]
          :where
          [?o :order/customer ?c]
          [?o :order/channel ?d]]
        db ru-ts)
   (map #(zipmap [:o-eid :y-m :c-eid :d-eid :u-eid :r] %))))

(defn- valid-time-span?
  "ts is of the form: #{\"2019-05\" \"2019-06\"}"
  [ts fjr]
  {:pre [(set? ts)]}
  (ts (:y-m fjr)))
;; Module API


(defn stream-ru-tuples->full-join-reports
  "ru-tuple is the form [o-eid year-month-string u-eid c-eid revenue]"
  [db time-span ru-tuples]
  (let [fjr-ety-xs (filter #(valid-time-span? time-span %) (stream-ru-tuples->fjr-entity-xs db ru-tuples))
        p-table (sc-enum->p-id db)
        r-table (c-p->rebate db)]
    (let [user-xs (map #(user-view db %) fjr-ety-xs)
          rev-stream-xs (map #(rev-stream-view db p-table %) fjr-ety-xs)
          customer-xs (map #(customer-view db %) fjr-ety-xs)
          rebate-xs (map #(stream-rebate-view db r-table %) fjr-ety-xs)
          revenue-xs (map #(revenue-view db %) fjr-ety-xs)]
      (map merge user-xs rev-stream-xs customer-xs rebate-xs revenue-xs))))

(defn order-ru-tuples->full-join-reports
  "ru-tuple is the form [o-eid year-month-string u-eid revenue]"
  [db time-span ru-tuples]
  (let [fjr-ety-xs (filter #(valid-time-span? time-span %) (order-ru-tuples->fjr-entity-xs db ru-tuples))
        p-table (sc-enum->p-id db)
        r-table (c-p->rebate db)]
    (let [user-xs (map #(user-view db %) fjr-ety-xs)
          order-xs (map #(order-view db p-table %) fjr-ety-xs)
          customer-xs (map #(customer-view db %) fjr-ety-xs)
          rebate-xs (map #(order-rebate-view db r-table %) fjr-ety-xs)
          revenue-xs (map #(revenue-view db %) fjr-ety-xs)]
      (map merge user-xs order-xs customer-xs rebate-xs revenue-xs))))
