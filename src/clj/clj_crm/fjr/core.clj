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
                             {:user/team [:db/ident]}] u)]
    {:user/name (:user/name u-graph)
     :team/name (name (get-in u-graph [:user/team :db/ident]))}))

(defn- revenue-view
  [db fjr]
  (let [{r :r t :y-m} fjr]
    {:revenue/value r
     :revenue/time  t}))

(defn- rev-stream-view
  [db fjr]
  (let [{o :o-eid} fjr]
    (-> (d/pull db '[:rev-stream/stream-unique-id
                     :rev-stream/writing-time
                     :rev-stream/accounting-time
                     :rev-stream/source
                     :rev-stream/campaign-name
                     :rev-stream/customer-id
                     {:rev-stream/service-category-enum [:db/ident]}] o)
        (update :rev-stream/source name)
        (update :rev-stream/service-category-enum (comp name :db/ident)))))

(defn- order-view
  [db fjr]
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
                     {:order/service-category-enum [:db/ident]}] o)
        (update :order/source name)
        (update :order/service-category-enum (comp name :db/ident)))))

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
  (let [c (d/q '[:find ?c .
                 :in $ ?o
                 :where
                 [?o :rev-stream/customer-id ?ci]
                 [?ra :rev-allo/customer-id ?ci]
                 [?ra :rev-allo/customer ?c]]
               db (first ru))]
    {:c-eid c}))

(defn- ru->otur-entity
  [ru]
  (let [[o y-m u r] ru]
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

;; Module API


(defn stream-ru-tuples->full-join-reports
  "ru-tuple is the form [o-eid year-month-string u-eid revenue]"
  [db ru-tuples]
  (let [fjr-ety-xs (stream-ru-tuples->fjr-entity-xs db ru-tuples)]
    (let [user-xs (map #(user-view db %) fjr-ety-xs)
          rev-stream-xs (map #(rev-stream-view db %) fjr-ety-xs)
          customer-xs (map #(customer-view db %) fjr-ety-xs)
          revenue-xs (map #(revenue-view db %) fjr-ety-xs)]
      (map merge user-xs rev-stream-xs customer-xs revenue-xs))))

(defn order-ru-tuples->full-join-reports
  "ru-tuple is the form [o-eid year-month-string u-eid revenue]"
  [db ru-tuples]
  (let [fjr-ety-xs (order-ru-tuples->fjr-entity-xs db ru-tuples)]
    (let [user-xs (map #(user-view db %) fjr-ety-xs)
          order-xs (map #(order-view db %) fjr-ety-xs)
          customer-xs (map #(customer-view db %) fjr-ety-xs)
          revenue-xs (map #(revenue-view db %) fjr-ety-xs)]
      (map merge user-xs order-xs customer-xs revenue-xs))))
